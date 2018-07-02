
module Types where


import Annotate.Common
import Annotate.Types

import Control.Concurrent.STM
import Control.Concurrent.Log


data AppState = AppState
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



data ClientAction
  = ClientClose
  | ClientSend ServerMsg
    deriving (Show, Generic)

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
  , trainer     :: (TChan ToTrainer, TChan FromTrainer)
  } deriving (Generic)



data Error = LogError String | DecodeError String | FileError String
   deriving (Show, Typeable)

instance Exception Error

data ToTrainer
  = TrainerDataset Collection
  | TrainerUpdate DocName (Maybe Document)
  | TrainerDetect ClientId DocName
    deriving (Show, Generic, Eq)


data FromTrainer
  = TrainerConnected
  | TrainerDetections ClientId DocName
  | TrainerError String
    deriving (Show, Generic, Eq)



data ExportImage = ExportImage
  { imageFile :: DocName,
    instances :: [Object],
    imageSize :: (Int, Int),
    category :: ImageCat
  } deriving (Show, Eq, Generic)

data Export = Export
  { config :: Config
  , images :: [ExportImage]
  } deriving (Show, Eq, Generic)


instance FromJSON Export
instance FromJSON ExportImage

instance ToJSON Export
instance ToJSON ExportImage
