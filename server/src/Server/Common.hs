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

import Data.Aeson (eitherDecode)
import Data.Generics.Product.Subtype (upcast)


import Text.Megaparsec hiding (some, many)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

data Store = Store
  { config    :: Config
  , images :: Map DocName Document
  , trainer :: TrainerState
  , preferences :: Map UserId Preferences
  } deriving (Show, Eq, Generic)

data Command where
  CmdCategory :: DocName -> ImageCat -> Command
  CmdSubmit :: Document -> UTCTime  -> Command
  CmdModified :: DocName -> UTCTime -> Command
  CmdImages :: [(DocName, DocInfo)] -> Command
  CmdClass :: ClassId -> Maybe ClassConfig -> Command
  CmdSetRoot  :: Text -> Command
  CmdCheckpoint :: NetworkId -> Float -> Bool -> Command
  CmdPreferences :: UserId -> Preferences -> Command
  CmdDetections  :: [(DocName, [Detection])] -> NetworkId -> Command
    deriving (Show, Eq, Generic)


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
  = TrainerInit TrainCollection
  | TrainerUpdate DocName (Maybe TrainImage)
  | TrainerDetect DetectRequest DocName DetectionParams
    deriving (Show, Generic, Eq)

data FromTrainer
  =  TrainerDetections DetectRequest DocName [Detection] NetworkId
  | TrainerReqError DetectRequest DocName Text
  | TrainerError Text
  | TrainerCheckpoint NetworkId Float Bool
    deriving (Show, Generic, Eq)


-- Input/export types
data TrainImage = TrainImage
  { imageFile   :: DocName,
    annotations :: [Annotation],
    imageSize   :: (Int, Int),
    category    :: ImageCat,
    validArea   :: Maybe Box
  } deriving (Show, Eq, Generic)


data TrainCollection = TrainCollection
  { config :: Config
  , images :: [TrainImage]
  } deriving (Show, Eq, Generic)


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

instance FromJSON DetectRequest
instance FromJSON ToTrainer
instance FromJSON FromTrainer
instance FromJSON TrainCollection
instance FromJSON TrainImage

instance ToJSON DetectRequest
instance ToJSON ToTrainer
instance ToJSON FromTrainer
instance ToJSON TrainCollection
instance ToJSON TrainImage



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


sendTrainer' :: Env -> Maybe ToTrainer -> STM Bool
sendTrainer' env msg = do
  mt <- readTVar (env ^. #trainer)
  for_ mt $ \trainer -> do
    writeLog env ("trainer -> " <> show msg)
    writeTChan (trainer ^. #connection) msg
  return (isJust mt)


sendTrainer :: Env -> ToTrainer -> STM Bool
sendTrainer env = sendTrainer' env . Just



withClient :: ClientEnv -> (Client -> STM a) -> STM (Maybe a)
withClient ClientEnv{clients, clientId}  f = do
  mClient <- M.lookup clientId <$> readTVar clients
  traverse f mClient

withClient_ :: ClientEnv -> (Client -> STM a) -> STM ()
withClient_ ClientEnv{clients, clientId}  f = do
  mClient <- M.lookup clientId <$> readTVar clients
  traverse_ f mClient


sendClient :: ClientEnv -> ServerMsg -> STM ()
sendClient env msg = void $ do
  clientLog env (" -> " <> show msg)

  withClient env $ \Client {..} ->
    writeTChan connection (Just msg)




withClient' :: Env -> ClientId -> (Client -> STM a) -> STM ()
withClient' Env{clients} clientId  f = do
  mClient <- M.lookup clientId <$> readTVar clients
  traverse_ f mClient


sendClient' :: Env -> ClientId -> ServerMsg -> STM ()
sendClient' env clientId msg = void $ do
  writeLog env (show clientId <> " -> " <> show msg)

  withClient' env clientId $ \Client{connection} ->
    writeTChan connection (Just msg)



broadcast :: Env -> ServerMsg -> STM ()
broadcast env msg = do
  writeLog env ("* -> " <> show msg)

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




lookupDoc :: DocName -> Store -> Maybe Document
lookupDoc k Store{..} = M.lookup k images

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
  part = (Left <$> decimal) <|> (Right <$> nonNumber)


type Parser = Parsec Void String


defaultInfo :: Dim -> DocName -> DocInfo
defaultInfo dim filename = DocInfo
  { naturalKey = makeNaturalKey filename
  , hashedName = Hash32 (fromIntegral (hash filename))
  , modified = Nothing
  , category = New
  , imageSize = dim
  , numAnnotations = 0
  }

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
