module Server.Common
  ( module Server.Common
  , module Annotate.Prelude
  , module Annotate.Common
  , module Control.Concurrent.STM
  , module Control.Concurrent.Log
  )
where


import Annotate.Prelude
import Annotate.Common

import GHC.Conc

import Control.Concurrent.STM
import Control.Concurrent.Log

import qualified Data.Map as M
import qualified Data.Text as Text

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Network.WebSockets             as WS

import qualified Data.Aeson as Aeson

import Data.Aeson (eitherDecode)
import Data.Generics.Product.Subtype (upcast)

import Data.Char as Char

import Text.Megaparsec hiding (some, many)
import Text.Megaparsec.Char 
import Text.Megaparsec.Char.Lexer (decimal)

import qualified Data.ByteString.Lazy  as BS




data Store = Store
  { config    :: Config
  , images :: Map DocName Document
  , trainer :: TrainerState
  , preferences :: Map UserId Preferences
  } deriving (Show,  Generic)


data Checkpoint = Checkpoint 
  { networkId :: NetworkId
  , score     :: Float
  , isBest    :: Bool
  } deriving (Show, Eq, Generic)


data Command where
  CmdCategory     :: DocName -> ImageCat -> Command
  CmdUpdate       :: Document -> UTCTime  -> Command
  CmdModified     :: DocName -> UTCTime -> Command
  CmdImages       :: [(DocName, DocInfo)] -> Command
  CmdClass        :: ClassId -> Maybe ClassConfig -> Command
  CmdSetRoot      :: Text -> Command
  CmdCheckpoint   :: Checkpoint -> Command
  CmdPreferences  :: UserId -> Preferences -> Command
  CmdDetections   :: Map DocName Detections -> Command
  CmdSubmit       :: UserId -> Submission -> UTCTime  -> Command
  CmdTraining     :: Map DocName [TrainSummary] -> Command
    deriving (Show, Generic)



data Client  = Client
  { connection  :: TChan (Maybe ServerMsg)
  , document    :: Maybe DocName
  , userId      :: UserId
  } deriving (Generic)


type Clients = TVar (Map ClientId Client)
type Documents = TVar (Map DocName [ClientId])

type LogMsg = String



data Trainer = Trainer
  { connection :: TChan (Maybe ToTrainer)
  , status     :: TrainerStatus
  } deriving (Generic)

data Env    = Env
  { clients     :: Clients
  , documents   :: Documents
  , store       :: Log Store
  , logChan     :: TChan LogMsg
  , trainer     :: TVar (Maybe Trainer)
  } deriving (Generic)

data ClientEnv    = ClientEnv
  { clientId    :: ClientId
  , userId      :: UserId
  , clients     :: Clients
  , documents   :: Documents
  , store       :: Log Store
  , logChan     :: TChan LogMsg
  , trainer     :: TVar (Maybe Trainer)
  } deriving (Generic)

clientEnv :: Env -> ClientId -> UserId -> ClientEnv
clientEnv Env{..} clientId userId = ClientEnv{..}

data ServerException = LogError String | DecodeError Text | FileError Text | FileExists FilePath
   deriving (Show, Typeable)

instance Exception ServerException

data DetectRequest
  = DetectClient ClientId
  | DetectLoad NavId ClientId
  | DetectPre
    deriving (Show, Generic, Eq)

-- Types for dealing with the trainer
data ToTrainer
  = TrainerInit Config
  | TrainerUpdate DocName (Maybe TrainImage)
  | TrainerDetect DetectRequest DocName (Map AnnotationId BasicAnnotation) DetectionParams
  | UserCommand UserCommand
    deriving (Show, Generic)

data FromTrainer
  = TrainerDetectRequest DetectRequest DocName Detections
  | TrainerReqError DetectRequest DocName Text
  | TrainerError Text
  | TrainerCheckpoint NetworkId Float Bool
  | TrainerProgress (Maybe Progress)
  | TrainerDetections (Map DocName Detections)
  | TrainerTraining (Map DocName [TrainSummary])
    deriving (Show, Generic)


-- Input/export types
data TrainImage = TrainImage
  { imageFile   :: DocName,
    annotations :: [BasicAnnotation],
    imageSize   :: (Int, Int),
    category    :: ImageCat,
    validArea   :: Maybe Box,
    evaluated   :: Maybe NetworkId,
    history     :: [(UTCTime, HistoryEntry)]
  } deriving (Show,  Generic)


data TrainCollection = TrainCollection
  { config :: Config
  , images :: [TrainImage]
  } deriving (Show,  Generic)


data ModelState = ModelState
  { state :: Maybe ByteString
  , epoch :: Epoch
  , score :: Float
  } deriving (Show, Eq, Generic)

data TrainerState = TrainerState
  { best    :: ModelState
  , current :: ModelState
  , run     :: RunId
  } deriving (Show, Eq, Generic)

bestModel :: TrainerState -> NetworkId
bestModel TrainerState{best, run} = (run, best ^. #epoch)

instance FromJSON DetectRequest where parseJSON = Aeson.genericParseJSON options
instance FromJSON ToTrainer where parseJSON = Aeson.genericParseJSON options
instance FromJSON FromTrainer where parseJSON = Aeson.genericParseJSON options
instance FromJSON TrainCollection where parseJSON = Aeson.genericParseJSON options
instance FromJSON TrainImage where parseJSON = Aeson.genericParseJSON options

instance ToJSON DetectRequest where toJSON = Aeson.genericToJSON options
instance ToJSON ToTrainer where toJSON = Aeson.genericToJSON options
instance ToJSON FromTrainer where toJSON = Aeson.genericToJSON options
instance ToJSON TrainCollection where toJSON = Aeson.genericToJSON options
instance ToJSON TrainImage where toJSON = Aeson.genericToJSON options



-- Collection of miscellaneous utilities / common functions

sendThread :: ToJSON a => WS.Connection -> IO (TChan (Maybe a))
sendThread conn = do
  chan <- atomically newTChan
  forkIO (run chan)
  return chan

  where
    run chan = do
      action <- atomically $ readTChan chan
      case action of
        Nothing   -> return ()
        Just msg -> liftIO $ do
          -- L.appendFile "server.log" (encode msg)
          WS.sendTextData conn (encode msg) >> run chan


tryDecode :: (MonadIO m, FromJSON a) => ByteString -> m a
tryDecode str = case eitherDecode str of
    Right a -> return a
    Left err -> liftIO (throw $ DecodeError (fromString err))


writeLog :: Env -> LogMsg -> STM ()
writeLog Env{logChan} = writeTChan logChan

clientLog :: ClientEnv -> LogMsg -> STM ()
clientLog ClientEnv{logChan, clientId} = writeTChan logChan . (show clientId <>)


trainerConnected :: ClientEnv -> STM Bool
trainerConnected env = isJust <$> readTVar (env ^. #trainer)


truncate :: String -> String
truncate str = if length str > 460
  then take 460 str <> "..."
  else str

sendTrainer' :: Env -> Maybe ToTrainer -> STM Bool
sendTrainer' env msg = do
  mt <- readTVar (env ^. #trainer)
  for_ mt $ \trainer -> do
    writeLog env ("trainer -> " <> truncate (show msg))
    writeTChan (trainer ^. #connection) msg
  return (isJust mt)


sendTrainer :: Env -> ToTrainer -> STM Bool
sendTrainer env = sendTrainer' env . Just


trainerStatus :: Env -> STM  TrainerStatus
trainerStatus Env{trainer} = do 
  trainer <- readTVar trainer
  return $ fromMaybe StatusDisconnected $ view #status <$> trainer


withClient :: ClientEnv -> (Client -> STM a) -> STM (Maybe a)
withClient ClientEnv{clients, clientId}  f = do
  mClient <- M.lookup clientId <$> readTVar clients
  traverse f mClient

withClient_ :: ClientEnv -> (Client -> STM a) -> STM ()
withClient_ ClientEnv{clients, clientId}  f = do
  mClient <- M.lookup clientId <$> readTVar clients
  traverse_ f mClient

sendClient :: ClientEnv -> ServerMsg -> STM ()
sendClient env = sendClient' (upcast env) (env ^. #clientId)
  

withClient' :: Env -> ClientId -> (Client -> STM a) -> STM ()
withClient' Env{clients} clientId  f = do
  mClient <- M.lookup clientId <$> readTVar clients
  traverse_ f mClient

logServerMsg :: ServerMsg -> Bool
logServerMsg (ServerStatus _) = False
logServerMsg _ = True
  

sendClient' :: Env -> ClientId -> ServerMsg -> STM ()
sendClient' env clientId msg = void $ do
  when (logServerMsg msg) $ 
    writeLog env (show clientId <> " -> " <> truncate (show msg))

  withClient' env clientId $ \Client{connection} ->
    writeTChan connection (Just msg)



broadcast :: Env -> ServerMsg -> STM ()
broadcast env msg = do

  when (logServerMsg msg) $ 
    writeLog env ("* -> " <> truncate (show msg))

  clients <- readTVar (env ^. #clients)
  for_ clients $ \Client {..} ->
    writeTChan connection (Just msg)

withClientEnvs :: Env -> (ClientEnv -> STM ()) -> STM ()
withClientEnvs env f = do
  clients <- readTVar (env ^. #clients)
  for_ (M.toList clients) $ \(clientId, Client {..}) ->
    f (clientEnv env clientId userId)


withClients :: Env -> ((ClientId, Client) -> STM ()) -> STM ()
withClients env f = do
  clients <- readTVar (env ^. #clients)
  for_ (M.toList clients) f


withClientEnv :: Env -> ClientId -> (ClientEnv -> STM ()) -> STM ()
withClientEnv env clientId f = do
  clients <- readTVar (env ^. #clients)
  for_ (M.lookup clientId clients) $ \Client {..} ->
    f (clientEnv env clientId userId)



withImages :: Env -> (Map DocName Document -> a) -> STM a
withImages env f = f . view #images <$> readLog (env ^. #store)


lookupDocument :: Env -> DocName -> STM (Maybe Document)
lookupDocument env k = withImages env (M.lookup k)

documentMap :: [Document] -> Map DocName Document
documentMap = M.fromList . fmap toKey where
  toKey doc = (doc ^. #name, doc)

lookupDocuments :: Env -> [DocName] -> STM (Map DocName Document)
lookupDocuments env ks = withImages env (documentMap . findImages) where
  findImages images = catMaybes $ flip M.lookup images <$> ks    


getCollection :: Store -> Collection
getCollection Store{..} = Collection $ view #info <$> images


getCurrentTime' :: STM UTCTime
getCurrentTime' = unsafeIOToSTM getCurrentTime


makeNaturalKey :: DocName -> NaturalKey
makeNaturalKey filename = fromMaybe
  (error "failed to parse sort key!") (parseMaybe parseNaturalKey (Text.unpack filename))



parseNaturalKey :: Parser NaturalKey
parseNaturalKey = (NaturalKey <$> many part) <* eof where
  nonNumber = Text.pack <$> takeWhile1P (Just "non digit") (not . isDigit)
  part = (Left <$> numeric) <|> (Right <$> nonNumber)

  numeric = do 
    ds <- takeWhile1P (Just "digit") Char.isDigit
    return (value ds, Text.pack ds)

  value = foldl (\ x -> ((10 * x) +) . fromIntegral . Char.digitToInt) 0


type Parser = Parsec Void String


defaultInfo :: Dim -> DocName -> DocInfo
defaultInfo dim filename = def 
    & #naturalKey .~ makeNaturalKey filename
    & #hashedName .~ Hash32 (fromIntegral (hash filename))
    & #imageSize  .~ dim


instance Default TrainerState where
  def =  TrainerState
    { best    = def
    , current = def
    , run = 0
    }


instance Default ModelState where
  def =  ModelState
    { state = Nothing
    , epoch = 0
    , score = 0.0
    }

unpackBS :: BS.ByteString -> String
unpackBS = fmap (chr . fromEnum) . BS.unpack
