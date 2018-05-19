module Main where

import Common
import Control.Concurrent.STM
import GHC.Conc

import Control.Concurrent.Log
import Control.Lens

import System.FilePath
import System.Directory
import System.IO

import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson (eitherDecode)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as Text

import qualified Data.ByteString.Lazy                      as BS
import Data.ByteString.Lazy (ByteString)

import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as Static


import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

import Servant
import Servant.Utils.StaticFiles

import qualified Types as T
import qualified Document as Doc
import qualified Options as Opt

import Types
import AppState
import ImageInfo



data ClientAction = ClientClose | ClientSend ServerMsg deriving (Show, Generic)

data Client  = Client
  { connection :: TChan ClientAction
  , document   :: Maybe DocName
  } deriving (Generic)


type Clients = TVar (Map ClientId Client)
type Documents = TVar (Map DocName [ClientId])



type LogMsg = String


data Env    = Env
  { clients     :: Clients
  , documents   :: Documents
  , state       :: Log AppState
  , logChan     :: TChan LogMsg
  } deriving (Generic)



data Error = LogError String | DecodeError String
   deriving (Show, Typeable)


instance Exception Error


nextClient :: Map ClientId Client -> ClientId
nextClient m = fromMaybe 0 (succ . fst . fst <$>  M.maxViewWithKey m)


sendHello :: Env -> ClientId -> STM ()
sendHello env clientId = do
  ds <- getCollection <$> readLog (env ^. #state)
  sendClient env clientId (ServerHello clientId ds)



connectClient :: Env -> WS.Connection ->  IO ClientId
connectClient env conn = do
  (clientId, chan) <- atomically $ do
    clientId <- nextClient <$> readTVar (env ^. #clients)
    chan <- newTChan
    modifyTVar (env ^. #clients) (M.insert clientId (Client chan Nothing))
    writeLog env $ "connected: " <> show clientId
    return (clientId, chan)

  clientId <$ forkIO (sendThread chan)

  where
    sendThread chan = do
      action <- atomically $ readTChan chan
      case action of
        ClientClose   -> return ()
        ClientSend msg -> liftIO $ WS.sendTextData conn (encode msg) >> sendThread chan


withClient :: Clients -> ClientId -> (Client -> STM a) -> STM (Maybe a)
withClient clients clientId  f = do
  mClient <- M.lookup clientId <$> readTVar clients
  traverse f mClient


disconnectClient :: Env -> ClientId -> IO ()
disconnectClient env clientId = atomically $ do
  withClient (env ^. #clients) clientId $ \Client {..} -> do
    writeTChan connection ClientClose

    closeDocument env clientId
    time <- unsafeIOToSTM getCurrentTime
    broadcast env (ServerOpen Nothing clientId time)

  modifyTVar (view #clients env) (M.delete clientId)
  writeLog env $ "disconnected: " <> show clientId



tryDecode :: (MonadIO m, FromJSON a) => ByteString -> m a
tryDecode str = case eitherDecode str of
    Right a -> return a
    Left err -> liftIO (throw $ DecodeError err)



-- flushDocument :: MonadIO m => AcidAppState Storage -> FilePath -> FilePath -> m ()
-- flushDocument storage root filename = do
--   (info, mDoc) <- query storage (GetDocument filename)
--   forM_ mDoc $ \doc -> do
--     liftIO $ BS.writeFile (root </> filename) (encode (info, doc))
--

at' :: (At m, Applicative f) =>
     Index m -> (IxValue m -> f (IxValue m)) -> m -> f m
at' i = at i . traverse


--
-- respond :: MonadIO m => WS.Connection -> T.ServerMsg -> m ()
-- respond conn msg = liftIO $ WS.sendTextData conn (encode msg)



clientDoc :: ClientId -> Traversal' (Map ClientId Client) DocName
clientDoc clientId = at' clientId . #document . traverse


closeDocument :: Env -> ClientId -> STM ()
closeDocument env@(Env {..}) clientId  = (^? clientDoc clientId) <$> readTVar clients >>= traverse_ withDoc
  where
    withDoc docName = do
        writeLog env ("closing " <> show clientId <> ", " <> show docName)

        refs <- M.lookup docName <$> readTVar documents
        modifyTVar documents (M.update removeClient docName)

        mInfo <- fst . lookupDoc docName <$> readLog state
        forM_ mInfo $ \info ->
          broadcast env (ServerUpdateInfo docName info)

    removeClient cs = case (filter (/= clientId) cs) of
        []  -> Nothing
        cs' -> Just cs'

        --   (#documents . at' doc ^?) <$> readLog state >>= traverse_ (\doc -> do
        --     writeTChan docWriter (DocFlush docName doc))

ordNub = S.toList . S.fromList


openDocument :: Env -> ClientId -> DocName -> STM ()
openDocument env@(Env {..}) clientId docName = do
  closeDocument env clientId

  writeLog env ("opening " <> show clientId <> ", " <> show docName)

  modifyTVar clients (at' clientId . #document .~ Just docName)
  modifyTVar documents ( M.alter addClient docName)

  time <- unsafeIOToSTM getCurrentTime
  broadcast env (ServerOpen (Just docName) clientId time)

    where
      addClient = \case
        Just cs -> Just $ ordNub (clientId:cs)
        Nothing -> Just [clientId]


modifyDoc :: Env -> DocName -> DocCmd -> STM ()
modifyDoc env@(Env {..}) docName cmd = do

  time <- unsafeIOToSTM getCurrentTime
  updateLog state (CmdDoc docName cmd time)

  clients <- getEditing <$> readTVar documents
  for_ clients $ \clientId ->
    sendClient env clientId (ServerCmd docName cmd)

  where
    getEditing = fromMaybe [] . M.lookup docName


findMin :: Ord a => Set a -> Maybe a
findMin s
  | length s == 0 = Nothing
  | otherwise     = Just (S.elemAt 0 s)

nextCircular :: Ord a => Set a -> a -> Maybe a
nextCircular s x = S.lookupGT x s' <|> findMin s'
  where s' = S.delete x s

findNext' :: AppState -> Map DocName [ClientId] -> Maybe DocName -> Maybe DocName
findNext' AppState{..} docs = \case
    Nothing      -> findMin editable
    Just current -> nextCircular editable current

    where
      editable = M.keysSet (M.filter isFresh images)
      isFresh = (== New) . view #category

findNext :: Env -> Maybe DocName -> STM (Maybe DocName)
findNext Env{..} maybeCurrent =
  findNext' <$> readLog state <*> readTVar documents <*> pure maybeCurrent

clientOpen :: Env -> ClientId -> DocName -> STM ()
clientOpen env clientId docName = do
  (mInfo, mDoc) <- lookupDoc docName <$> readLog (env ^. #state)
  for_ mInfo $ \info -> do
    openDocument env clientId docName
    sendClient env clientId (ServerDocument docName info (fromMaybe Doc.emptyDoc mDoc))

recieveLoop :: Env -> WS.Connection -> ClientId -> IO ()
recieveLoop env conn clientId = do
  atomically $ sendHello env clientId
  forever $ do
    req <- tryDecode =<< liftIO (WS.receiveData conn)
    atomically $ writeLog env (show clientId <> " <- " <> show req)
    case req of
        ClientOpen docName      -> atomically $ clientOpen env clientId docName
        ClientCmd docName cmd -> atomically $ modifyDoc env docName cmd

        ClientSubmit docName cat -> atomically $ do
          updateLog (env ^. #state) (CmdCategory docName cat)
          nextImage env clientId (Just docName)

        ClientNext current -> atomically $ nextImage env clientId current


nextImage :: Env -> ClientId -> Maybe DocName -> STM ()
nextImage env clientId current = do
  maybeDoc <- findNext env current
  case maybeDoc of
    Just docName -> clientOpen env clientId docName
    Nothing      -> sendClient env clientId ServerEnd

sendClient :: Env -> ClientId -> T.ServerMsg -> STM ()
sendClient env clientId msg = void $ do
  writeLog env (show clientId <> " -> " <> show msg)

  withClient (env ^. #clients) clientId $ \Client {..} ->
    writeTChan connection (ClientSend msg)



broadcast :: Env -> T.ServerMsg -> STM ()
broadcast env msg = do
  writeLog env ("* -> " <> show msg)

  clients <- readTVar (env ^. #clients)
  for_ clients $ \Client {..} ->
    writeTChan connection (ClientSend msg)

websocketServer :: Env -> WS.ServerApp
websocketServer env pending = do
  conn <- WS.acceptRequest pending

  clientId <- connectClient env conn

  WS.forkPingThread conn 30
  finally
    (recieveLoop env conn clientId)
    (disconnectClient env clientId)



type Api =
  "ws" :> Raw
  :<|> "images" :> Raw
  :<|> Raw


server :: FilePath -> Env -> Server Api
server root env =
  withDefault (websocketServer env)
    :<|> serveDirectoryWebApp root
    :<|> serveDirectoryWebApp "html"


withDefault :: WS.ServerApp -> Server Raw
withDefault ws = Tagged $ WS.websocketsOr WS.defaultConnectionOptions ws backupApp
  where backupApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a WebSocket request"






validExtension :: [String] -> FilePath -> Bool
validExtension exts filename = any (\e -> fmap toLower e == ext) exts where
  ext = fmap toLower (takeExtension filename)


findImages :: Config -> FilePath -> IO [DocName]
findImages config root = do
  contents <- fmap fromString <$> listDirectory root
  return $ fromString <$> filter (validExtension exts) contents
    where exts = Text.unpack <$> config ^. #extensions


-- imageInfo :: FilePath -> IO (Maybe DocInfo)
-- imageInfo filename = do
--   info <- docInfo <$> Codec.readImageWithMetadata filename
--   info <$ print (filename, info)
--   where
--     docInfo (Left _)              = Nothing
--     docInfo (Right (_, metadata)) = toInfo <$>
--       (Codec.lookup Width metadata) <*> (Codec.lookup Height metadata)
--
--     toInfo w h = DocInfo Nothing False (fromIntegral w, fromIntegral h)

-- /home/oliver/trees/_DSC2028.JPG JPEG 1600x1064 1600x1064+0+0 8-bit sRGB 958KB 0.000u 0:00.000


findNewImages :: Config -> FilePath -> Map DocName DocInfo -> IO [(DocName, DocInfo)]
findNewImages config root existing = do
  images <- findImages config root

  catMaybes <$> (forM (filter (flip M.notMember existing) images) $ \image -> do
    fmap (image, ) <$> imageInfo root image)



writeLog :: Env -> LogMsg -> STM ()
writeLog env msg = writeTChan (env ^. #logChan) msg

startLogger :: Handle -> IO (TChan LogMsg)
startLogger handle = do
  logChan   <- atomically newTChan

  forkIO $ forever $ do
    msg <- atomically $ readTChan logChan
    hPutStrLn handle msg

  return logChan


readRoot :: Log AppState -> IO FilePath
readRoot state = Text.unpack . view (#config . #root) <$> atomically (readLog state)

main :: IO ()
main = do
  Opt.Options {..} <- Opt.getArgs
  logChan <- startLogger stdout

  create' <- forM create $ \root -> do
    createDirectoryIfMissing True root
    return (initialState (defaultConfig & #root .~ (fromString root)))

  import' <- forM importJson $ \file ->
    BS.readFile file >>= fmap fromExport . tryDecode

  state <- case create' <|> import' of
    Just initial -> freshLog initial database
    Nothing      -> openLog database >>= either (throw . LogError) return

  root <- readRoot state

  clients   <- atomically (newTVar M.empty)
  documents <- atomically (newTVar M.empty)

  atomically $ do
    config <- view #config <$> readLog state
    existing <- view #images <$> readLog state
    images <- unsafeIOToSTM (findNewImages config root existing)
    updateLog state (CmdImages images)

  forM_ exportJson $ \file -> do
    atomically (readLog state) >>= BS.writeFile file . encodePretty . toExport
    putStrLn ("exported state to: " <> file)

  -- print =<< atomically (readLog state)
  Warp.run 3000 $ serve (Proxy @ Api) (server root $ Env {..})
