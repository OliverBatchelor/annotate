module Main where

import Annotate.Common
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

import Annotate.Types
import qualified Annotate.Types as T
import qualified Annotate.Document as Doc

import qualified Options as Opt

import AppState
import ImageInfo
import Stream

import Types


nextClient :: Map ClientId Client -> ClientId
nextClient m = fromMaybe 0 (succ . fst . fst <$>  M.maxViewWithKey m)


sendHello :: Env -> ClientId -> STM ()
sendHello env clientId = do
  -- ds <- getCollection <$> readLog (env ^. #state)
  sendClient env clientId (ServerHello clientId)


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



clientDoc :: ClientId -> Traversal' (Map ClientId Client) DocName
clientDoc clientId = ix clientId . #document . traverse


closeDocument :: Env -> ClientId -> STM ()
closeDocument env@(Env {..}) clientId  = (^? clientDoc clientId) <$> readTVar clients >>= traverse_ withDoc
  where
    withDoc k = do
        writeLog env ("closing " <> show clientId <> ", " <> show k)

        refs <- M.lookup k <$> readTVar documents
        modifyTVar documents (M.update removeClient k)

        mInfo <- fst . lookupDoc k <$> readLog state
        forM_ mInfo $ \info ->
          broadcast env (ServerUpdateInfo k info)

    removeClient cs = case (filter (/= clientId) cs) of
        []  -> Nothing
        cs' -> Just cs'


ordNub = S.toList . S.fromList


openDocument :: Env -> ClientId -> DocName -> STM ()
openDocument env@(Env {..}) clientId k = do
  closeDocument env clientId

  writeLog env ("opening " <> show clientId <> ", " <> show k)

  modifyTVar clients (ix clientId . #document .~ Just k)
  modifyTVar documents ( M.alter addClient k)

  time <- unsafeIOToSTM getCurrentTime
  broadcast env (ServerOpen (Just k) clientId time)

    where
      addClient = \case
        Just cs -> Just $ ordNub (clientId:cs)
        Nothing -> Just [clientId]


modifyDoc :: Env -> DocName -> DocCmd -> STM ()
modifyDoc env@(Env {..}) k cmd = do

  time <- unsafeIOToSTM getCurrentTime
  updateLog state (CmdDoc k cmd time)

  clients <- getEditing <$> readTVar documents
  for_ clients $ \clientId ->
    sendClient env clientId (ServerCmd k cmd)

  where
    getEditing = fromMaybe [] . M.lookup k


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
clientOpen env clientId k = do
  (mInfo, mDoc) <- lookupDoc k <$> readLog (env ^. #state)
  for_ mInfo $ \info -> do
    openDocument env clientId k
    sendClient env clientId (ServerDocument k info (fromMaybe Doc.emptyDoc mDoc))

recieveLoop :: Env -> WS.Connection -> ClientId -> IO ()
recieveLoop env@Env{state} conn clientId = do
  atomically $ sendHello env clientId
  forever $ do
    req <- tryDecode =<< liftIO (WS.receiveData conn)
    time <- getCurrentTime

    atomically $ writeLog env (show clientId <> " <- " <> show req)
    case req of
        ClientOpen k      -> atomically $ clientOpen env clientId k
        ClientCmd k cmd -> atomically $ modifyDoc env k cmd

        ClientSubmit k cat doc -> atomically $ do
          updateLog state (CmdSubmit k doc time)
          updateLog state (CmdCategory k cat)
          nextImage env clientId (Just k)

        ClientDiscard k -> atomically $ do
          updateLog state (CmdCategory k Discard)
          nextImage env clientId (Just k)

        ClientNext current -> atomically $
          nextImage env clientId current

        ClientDetect k -> atomically $
          sendTrainer env (TrainerDetect clientId k)



nextImage :: Env -> ClientId -> Maybe DocName -> STM ()
nextImage env clientId current = do
  maybeDoc <- findNext env current
  case maybeDoc of
    Just k -> clientOpen env clientId k
    Nothing      -> sendClient env clientId ServerEnd

sendClient :: Env -> ClientId -> T.ServerMsg -> STM ()
sendClient env clientId msg = void $ do
  writeLog env (show clientId <> " -> " <> show msg)

  withClient (env ^. #clients) clientId $ \Client {..} ->
    writeTChan connection (ClientSend msg)

sendTrainer :: Env -> ToTrainer -> STM ()
sendTrainer env msg = do
  writeLog env ("trainer -> " <> show msg)
  writeTChan send msg
    where (send, _) = env ^. #trainer


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

  trainer <- atomically $ liftA2 (,) newTChan newTChan

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
