module Annotate.Common (
  module Annotate.Common,
  module Annotate.Geometry,
  module Annotate.Colour,

  Generic(..),
) where

import Annotate.Prelude
import Annotate.Colour

import qualified Data.Map as M
import qualified Data.Set as S

import Data.Generics.Product
import Annotate.Geometry

import Control.Lens (makePrisms)


type AnnotationId = Int
type ClientId = Int
type ClassId = Int

type DocName = Text
type DateTime = UTCTime

data Shape = BoxShape     Box
           | PolygonShape Polygon
           | LineShape    WideLine
     deriving (Generic, Show, Eq)

data ShapeConfig = BoxConfig | PolygonConfig | LineConfig
  deriving (Generic, Show, Eq, Ord)


instance HasBounds Shape where
 getBounds (BoxShape s)     = getBounds s
 getBounds (PolygonShape s) = getBounds s
 getBounds (LineShape s)    = getBounds s


data Detection = Detection
  { label      :: ClassId
  , bounds     :: Box
  , confidence :: Float
  } deriving (Generic, Show, Eq)

data Annotation = Annotation
  { shape :: Shape
  , label :: ClassId
  , detection :: Maybe Detection
  , confirm :: Bool
  } deriving (Generic, Show, Eq)


type AnnotationMap = Map AnnotationId Annotation


data Document = Document
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea   :: Maybe Box
  } deriving (Generic, Show, Eq)


data ImageCat = New | Train | Test | Discard deriving (Eq, Ord, Enum, Generic, Show)

data DocInfo = DocInfo
  { modified    :: Maybe DateTime
  , numAnnotations :: Int
  , category    :: ImageCat
  , imageSize   :: (Int, Int)
  } deriving (Generic, Show, Eq)


data ClassConfig = ClassConfig
  { name :: Text
  , shape :: ShapeConfig
  , colour :: HexColour
  } deriving (Generic, Show, Eq)


data Config = Config
  { root      :: Text
  , extensions :: [Text]
  , classes     :: Map ClassId ClassConfig
  } deriving (Generic, Show, Eq)

data Preferences = Preferences
  { controlSize       :: Float
  , brushSize         :: Float

  , instanceColours   :: Bool
  , opacity           :: Float
  , hiddenClasses     :: Set Int

  , detection    :: DetectionParams
  , threshold    :: Float
  } deriving (Generic, Show, Eq)

data DetectionParams = DetectionParams
  {   nms            :: Float
  ,   threshold      :: Float
  ,   detections     :: Int
  } deriving (Generic, Show, Eq)


data Collection = Collection
  { images :: Map DocName DocInfo
  } deriving (Generic, Show, Eq)


data ErrCode = ErrDecode Text | ErrNotFound DocName | ErrNotRunning | ErrTrainer Text
   deriving (Generic, Show, Eq)


data ServerMsg
  = ServerHello ClientId Config
  | ServerConfig Config
  | ServerCollection Collection
  | ServerUpdateInfo DocName DocInfo
  | ServerDocument Document
  | ServerOpen (Maybe DocName) ClientId DateTime
  | ServerError ErrCode
  | ServerDetection DocName [Detection]
  | ServerEnd
      deriving (Generic, Show, Eq)

data ClientMsg
  = ClientOpen DocName
  | ClientNext (Maybe DocName)
  | ClientSubmit Document
  | ClientDiscard DocName
  | ClientDetect DocName DetectionParams
  | ClientClass ClassId (Maybe ClassConfig)
  | ClientCollection

      deriving (Generic, Show, Eq)

instance FromJSON ShapeConfig
instance FromJSON ClassConfig

instance FromJSON DetectionParams
instance FromJSON Preferences

instance FromJSON ImageCat
instance FromJSON Shape
instance FromJSON Annotation
instance FromJSON Detection

instance FromJSON Document
instance FromJSON Config
instance FromJSON DocInfo
instance FromJSON Collection
instance FromJSON ServerMsg
instance FromJSON ClientMsg
instance FromJSON ErrCode

instance ToJSON ShapeConfig
instance ToJSON ClassConfig

instance ToJSON DetectionParams
instance ToJSON Preferences

instance ToJSON ImageCat
instance ToJSON Shape
instance ToJSON Annotation
instance ToJSON Detection
instance ToJSON Document
instance ToJSON Config
instance ToJSON DocInfo
instance ToJSON Collection
instance ToJSON ServerMsg
instance ToJSON ClientMsg
instance ToJSON ErrCode

instance Default Config where
  def = Config
    { root = ""
    , extensions = [".png", ".jpg", ".jpeg"]
    , classes    = M.fromList [(0, newClass 0)]
    }

instance Default Preferences where
  def = Preferences
    { controlSize = 10
    , brushSize = 40
    , instanceColours = False
    , opacity = 0.4
    , hiddenClasses = mempty
    , detection = def
    , threshold = 0.5
    }

instance Default DetectionParams where
  def = DetectionParams
    { nms = 0.5
    , threshold = 0.05
    , detections = 100
    }

newClass :: ClassId -> ClassConfig
newClass k = ClassConfig
  { name    = "unnamed-" <> fromString (show k)
  , colour  = fromMaybe 0xFFFF00 $ preview (ix k) defaultColours
  , shape   = BoxConfig
  }


getConfidence :: Annotation -> Float
getConfidence Annotation{confirm, detection} = if confirm
    then 1.0
    else fromMaybe 1.0 (view #confidence <$> detection)


maxKey :: Ord k => Map k a -> Maybe k
maxKey = fmap fst . maxMap

minKey :: Ord k => Map k a -> Maybe k
minKey = fmap fst . minMap


maxElem :: Ord k => Map k a -> Maybe a
maxElem = fmap snd . maxMap

minElem :: Ord k => Map k a -> Maybe a
minElem = fmap snd . minMap

maxMap :: Ord k => Map k a -> Maybe (k, a)
maxMap m | M.null m = Nothing
         | otherwise = Just $ M.findMax m

minMap :: Ord k => Map k a -> Maybe (k, a)
minMap m | M.null m = Nothing
        | otherwise = Just $ M.findMin m


setToMap :: Ord k =>  a ->  Set k -> Map k a
setToMap a = M.fromDistinctAscList . fmap (, a) . S.toAscList


setToMap' :: Ord k => Set k -> Map k ()
setToMap' = setToMap ()


emptyCollection :: Collection
emptyCollection = Collection mempty

makePrisms ''ClientMsg
makePrisms ''ServerMsg
makePrisms ''Shape
