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

data DocCmd = DocEdit Edit | DocUndo | DocRedo
  deriving (Show, Eq, Generic)

data EditAction
  = Add Annotation
  | Delete
  | Modify Annotation
  deriving (Generic, Show, Eq)

newtype Edit = Edit { unEdit :: Map AnnotationId EditAction }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type DocParts = Map AnnotationId (Set Int)

type DocPart = (AnnotationId, Maybe Int)

mergeParts :: DocParts -> DocParts -> DocParts
mergeParts = M.unionWith mappend


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


data Annotation = Annotation { shape :: Shape, label :: ClassId, predictions :: [(ClassId, Float)] }
    deriving (Generic, Show, Eq)


type AnnotationMap = Map AnnotationId Annotation
type DocumentPatch = Map AnnotationId (Maybe Annotation)


data Document = Document
  { undos :: [Edit]
  , redos :: [Edit]
  , name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
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
  { controlSize :: Float
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
  | ServerEnd
      deriving (Generic, Show, Eq)

data ClientMsg
  = ClientOpen DocName
  | ClientNext (Maybe DocName)
  | ClientSubmit Document
  | ClientDiscard DocName
  | ClientDetect DocName
  | ClientClass ClassId (Maybe ClassConfig)
  | ClientCollection

      deriving (Generic, Show, Eq)

instance FromJSON ShapeConfig
instance FromJSON ClassConfig

instance FromJSON Preferences

instance FromJSON EditAction
instance FromJSON DocCmd
instance FromJSON ImageCat
instance FromJSON Shape
instance FromJSON Annotation
instance FromJSON Document
instance FromJSON Config
instance FromJSON DocInfo
instance FromJSON Collection
instance FromJSON ServerMsg
instance FromJSON ClientMsg
instance FromJSON ErrCode

instance ToJSON ShapeConfig
instance ToJSON ClassConfig

instance ToJSON Preferences

instance ToJSON EditAction
instance ToJSON DocCmd
instance ToJSON ImageCat
instance ToJSON Shape
instance ToJSON Annotation
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
  , classes    = M.fromList [(0, newClass 0)]
  }

defaultPreferences :: Preferences
defaultPreferences = Preferences
  { controlSize = 10
  }

newClass :: ClassId -> ClassConfig
newClass k = ClassConfig
  { name    = "unnamed-" <> fromString (show k)
  , colour  = fromMaybe 0xFFFF00 $ preview (ix k) defaultColours
  , shape   = BoxConfig
  }


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


setToMap :: Ord k => Set k -> Map k ()
setToMap = M.fromDistinctAscList . fmap (, ()) . S.toAscList


emptyCollection :: Collection
emptyCollection = Collection mempty

makePrisms ''ClientMsg
makePrisms ''ServerMsg
makePrisms ''DocCmd
makePrisms ''Shape
makePrisms ''Edit
