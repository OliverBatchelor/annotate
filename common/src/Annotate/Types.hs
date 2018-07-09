module Annotate.Types (
  module Annotate.Types,
  module Annotate.Geometry,

  Generic(..),
) where

import Annotate.Common

import qualified Data.Map as M

import Data.Generics.Product
import Annotate.Geometry

import Control.Lens (makePrisms)

type ObjId = Int
type ClientId = Int
type ClassId = Int

type DocName = Text
type DateTime = UTCTime

data DocCmd = DocEdit Edit | DocUndo | DocRedo
  deriving (Show, Eq, Generic)

data Edit
  = Add [(ObjId, Object)]
  | Delete [ObjId]
  | Transform [ObjId] Float Vec
  deriving (Generic, Show, Eq)

-- instance Monoid Edit where
--   mempty = Many []
--   mappend (Many []) e = e
--   mappend e (Many []) = e
  -- mappend e e' = Many [e, e']

data Shape = CircleShape Circle
           | BoxShape    Box
   deriving (Generic, Show, Eq)

data Object = Object { shape :: Shape, label :: ClassId, predictions :: [(ClassId, Float)] }
    deriving (Generic, Show, Eq)


type ObjectMap = Map ObjId Object

data Document = Document
  { undos :: [Edit]
  , redos :: [Edit]
  , instances :: ObjectMap
  } deriving (Generic, Show, Eq)


data ImageCat = New | Train | Test | Discard deriving (Eq, Ord, Enum, Generic, Show)

data DocInfo = DocInfo
  { modified :: Maybe DateTime
  , category :: ImageCat
  , imageSize :: (Int, Int)
  } deriving (Generic, Show, Eq)


data Config = Config
  { root      :: Text
  , extensions :: [Text]
  , classes    :: Map ClassId Text
  } deriving (Generic, Show, Eq)

data Collection = Collection
  { config :: Config
  , images :: Map DocName DocInfo
  } deriving (Generic, Show, Eq)


data ErrCode = ErrDecode Text | ErrNotFound DocName | ErrNotRunning | ErrTrainer Text
   deriving (Generic, Show, Eq)


data ServerMsg
  = ServerHello ClientId
  | ServerUpdateInfo DocName DocInfo
  | ServerDocument DocName DocInfo Document
  | ServerOpen (Maybe DocName) ClientId DateTime
  | ServerCmd DocName DocCmd
  | ServerError ErrCode
  | ServerEnd
      deriving (Generic, Show, Eq)

data ClientMsg
  = ClientOpen DocName
  | ClientCmd DocName DocCmd
  | ClientNext (Maybe DocName)
  | ClientSubmit DocName ImageCat Document
  | ClientDiscard DocName
  | ClientDetect DocName

      deriving (Generic, Show, Eq)


instance FromJSON Edit
instance FromJSON DocCmd
instance FromJSON ImageCat
instance FromJSON Shape
instance FromJSON Object
instance FromJSON Document
instance FromJSON Config
instance FromJSON DocInfo
instance FromJSON Collection
instance FromJSON ServerMsg
instance FromJSON ClientMsg
instance FromJSON ErrCode

instance ToJSON Edit
instance ToJSON DocCmd
instance ToJSON ImageCat
instance ToJSON Shape
instance ToJSON Object
instance ToJSON Document
instance ToJSON Config
instance ToJSON DocInfo
instance ToJSON Collection
instance ToJSON ServerMsg
instance ToJSON ClientMsg
instance ToJSON ErrCode


defaultConfig :: Config
defaultConfig = Config
  { root = ""
  , extensions = [".png", ".jpg", ".jpeg"]
  , classes    = M.fromList [(0, "default")]
  }

makePrisms ''ClientMsg
makePrisms ''ServerMsg
makePrisms ''DocCmd
makePrisms ''Shape
makePrisms ''Edit