module Server.Common
  ( module Server.Common
  , module Annotate.Common
  , module Annotate.Types
  , module Control.Concurrent.STM
  , module Control.Concurrent.Log
  )
where


import Annotate.Common
import Annotate.Types

import GHC.Conc

import Control.Concurrent.STM
import Control.Concurrent.Log

import qualified Data.Map as M

import Data.ByteString.Lazy (ByteString)
import qualified Network.WebSockets             as WS

import Data.Aeson (eitherDecode)
import Data.Generics.Product.Subtype (upcast)



data Store = Store
  { config    :: Config
  , images    :: Map DocName DocInfo
  , documents :: Map DocName Document
  } deriving (Show, Eq, Generic)

data Command where
  CmdDoc :: DocName -> DocCmd -> UTCTime -> Command
  CmdCategory :: DocName -> ImageCat -> Command
  CmdSubmit :: DocName -> Document -> UTCTime -> Command

  CmdModified :: DocName -> UTCTime -> Command
  CmdImages :: [(DocName, DocInfo)] -> Command
    deriving (Show, Eq, Generic)



data Client  = Client
  { connection :: TChan (Maybe ServerMsg)
  , document   :: Maybe DocName
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


-- Types for dealing with the trainer
data ServerException = LogError String | DecodeError Text | FileError Text
   deriving (Show, Typeable)

instance Exception ServerException

data ToTrainer
  = TrainerDataset Export
  | TrainerUpdate DocName (Maybe ImageE)
  | TrainerDetect ClientId DocName
    deriving (Show, Generic, Eq)


data FromTrainer
  =  TrainerDetections ClientId DocName
  | TrainerReqError ClientId Text
  | TrainerError Text
    deriving (Show, Generic, Eq)


-- Input/export types
data ImageE = ImageE
  { imageFile :: DocName,
    instances :: [Object],
    imageSize :: (Int, Int),
    category :: ImageCat
  } deriving (Show, Eq, Generic)

data Export = Export
  { config :: Config
  , images :: [ImageE]
  } deriving (Show, Eq, Generic)



instance FromJSON ToTrainer
instance FromJSON FromTrainer
instance FromJSON Export
instance FromJSON ImageE

instance ToJSON ToTrainer
instance ToJSON FromTrainer
instance ToJSON Export
instance ToJSON ImageE

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
        Just msg -> liftIO $ WS.sendTextData conn (encode msg) >> run chan


tryDecode :: (MonadIO m, FromJSON a) => ByteString -> m a
tryDecode str = case eitherDecode str of
    Right a -> return a
    Left err -> liftIO (throw $ DecodeError (fromString err))


writeLog :: Env -> LogMsg -> STM ()
writeLog env = writeTChan (env ^. #logChan)



sendTrainer' :: Env -> Maybe ToTrainer -> STM Bool
sendTrainer' env msg = do
  mt <- readTVar (env ^. #trainer)
  for_ mt $ \trainer -> do
    writeLog env ("trainer -> " <> show msg)
    writeTChan (trainer ^. #connection) msg
  return (isJust mt)


sendTrainer :: Env -> ToTrainer -> STM Bool
sendTrainer env = sendTrainer' env . Just



withClient :: Clients -> ClientId -> (Client -> STM a) -> STM (Maybe a)
withClient clients clientId  f = do
  mClient <- M.lookup clientId <$> readTVar clients
  traverse f mClient


sendClient :: Env -> ClientId -> ServerMsg -> STM ()
sendClient env clientId msg = void $ do
  writeLog env (show clientId <> " -> " <> show msg)

  withClient (env ^. #clients) clientId $ \Client {..} ->
    writeTChan connection (Just msg)


broadcast :: Env -> ServerMsg -> STM ()
broadcast env msg = do
  writeLog env ("* -> " <> show msg)

  clients <- readTVar (env ^. #clients)
  for_ clients $ \Client {..} ->
    writeTChan connection (Just msg)


lookupDoc :: DocName -> Store -> (Maybe DocInfo, Maybe Document)
lookupDoc k Store{..} = (M.lookup k images, M.lookup k documents)

getCollection :: Store -> Collection
getCollection = upcast




getCurrentTime' :: STM UTCTime
getCurrentTime' = unsafeIOToSTM getCurrentTime
