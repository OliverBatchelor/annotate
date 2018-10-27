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
import Data.Hashable

import qualified Data.Text as Text
import qualified Data.Aeson as Aeson

type AnnotationId = Int
type ClientId = Int
type UserId = Int

type ClassId = Int

type DocName = Text
type DateTime = UTCTime

type Epoch = Int
type RunId = Int

type NetworkId = (RunId, Epoch)

data Shape = BoxShape     Box
           | CircleShape  Circle
           | PolygonShape Polygon
           | LineShape    WideLine
     deriving (Generic, Show, Eq)

data ShapeConfig = CircleConfig | BoxConfig | PolygonConfig | LineConfig
  deriving (Generic, Show, Eq, Ord)


instance HasBounds Shape where
 getBounds (CircleShape s)  = getBounds s
 getBounds (BoxShape s)     = getBounds s
 getBounds (PolygonShape s) = getBounds s
 getBounds (LineShape s)    = getBounds s


data Detection = Detection
  { label      :: ClassId
  , shape      :: Shape
  , confidence :: Float
  } deriving (Generic, Show, Eq)

data Annotation = Annotation
  { shape :: Shape
  , label :: ClassId
  , detection :: Maybe Detection
  , confirm :: Bool
  } deriving (Generic, Show, Eq)


type AnnotationMap = Map AnnotationId Annotation


data EditAction
  = Add Annotation
  | Delete
  | Modify Annotation
  deriving (Generic, Show, Eq)


  -- data AddAction
  --   = AddObject Annotation
  --   | AddPolygon (Map Int Position)
  --   | AddLine (Map Int Circle)
  --   deriving (Generic, Show, Eq)
  --
  --
  -- data Edit
  --   = Add (Map AnnotationId AddAction)
  --   | Delete DocParts
  --   | SetClass (Set AnnotationId) ClassId
  --   | Transform DocParts Float Vector
  --   | SetArea (Maybe Box)
  --   deriving (Eq, Show, Generic)


-- TODO: Edit _should_ be basic operations as above, e.g. add/delete/move
data Edit = Edit (Map AnnotationId EditAction)
          | SetArea (Maybe Box)
  deriving (Eq, Show, Generic)


data HistoryEntry = HistOpen | HistSubmit | HistEdit Edit | HistUndo | HistRedo
  deriving (Show, Eq, Generic)

data EditCmd = DocEdit Edit | DocUndo | DocRedo
  deriving (Show, Eq, Generic)

newtype NaturalKey = NaturalKey [Either Int Text]
  deriving (Ord, Eq, Generic, Show)



data Document = Document
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea   :: Maybe Box

  , history :: [(UTCTime, HistoryEntry)]
  , detections :: Maybe ([Detection], NetworkId)

  } deriving (Generic, Show, Eq)


data ImageCat = New | Train | Test | Discard deriving (Eq, Ord, Enum, Generic, Show)
newtype Hash32 = Hash32 { unHash :: Word32 }
  deriving (Eq, Ord, Enum, Generic, Show)

data DocInfo = DocInfo
  { hashedName :: Hash32
  , naturalKey :: NaturalKey
  , modified    :: Maybe DateTime
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


data ImageOrdering = OrderSequential | OrderMixed
  deriving (Show, Eq, Ord, Enum, Generic)


data Preferences = Preferences
  { controlSize       :: Float
  , brushSize         :: Float

  , instanceColours   :: Bool
  , opacity           :: Float
  , hiddenClasses     :: Set Int

  , gamma             :: Float
  , brightness        :: Float
  , contrast          :: Float

  , detection    :: DetectionParams
  , threshold    :: Float

  , ordering    :: ImageOrdering
  } deriving (Generic, Show, Eq)

data DetectionParams = DetectionParams
  {   nms            :: Float
  ,   threshold      :: Float
  ,   detections     :: Int
  } deriving (Generic, Show, Eq)


data Collection = Collection
  { images :: Map DocName DocInfo
  } deriving (Generic, Show, Eq)


data ErrCode
  = ErrDecode Text
  | ErrNotFound NavId DocName
  | ErrNotRunning
  | ErrTrainer Text
  | ErrEnd NavId
    deriving (Generic, Show, Eq)

type NavId = Int

data ServerMsg
  = ServerHello ClientId Preferences Config
  | ServerConfig Config
  | ServerCollection Collection
  | ServerUpdateInfo DocName DocInfo
  | ServerDocument NavId Document
  | ServerOpen (Maybe DocName) ClientId DateTime
  | ServerError ErrCode
  | ServerDetection DocName [Detection]
      deriving (Generic, Show, Eq)



data Navigation
  = NavNext
  | NavTo DocName
    deriving (Generic, Show, Eq)

data ConfigUpdate
  = ConfigClass ClassId (Maybe ClassConfig)
    deriving (Generic, Show, Eq)

data ClientMsg
  = ClientNav NavId Navigation
  | ClientSubmit Document
  | ClientDetect DocName
  | ClientConfig ConfigUpdate
  | ClientPreferences Preferences
  | ClientCollection
      deriving (Generic, Show, Eq)


instance FromJSON Hash32 where
  parseJSON (Aeson.String v) = return $ Hash32 $ read (Text.unpack v)
  parseJSON _          = fail "expected string value"

instance ToJSON Hash32 where
  toJSON (Hash32 v) = Aeson.String (Text.pack (show v))


instance FromJSON ShapeConfig
instance FromJSON ClassConfig

instance FromJSON DetectionParams
instance FromJSON ImageOrdering

instance FromJSON Preferences

instance FromJSON ImageCat
instance FromJSON Shape
instance FromJSON Annotation
instance FromJSON Detection

instance FromJSON EditAction
instance FromJSON HistoryEntry
instance FromJSON Edit
instance FromJSON EditCmd

instance FromJSON Navigation
instance FromJSON ConfigUpdate
instance FromJSON NaturalKey
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
instance ToJSON ImageOrdering
instance ToJSON Preferences

instance ToJSON ImageCat
instance ToJSON Shape
instance ToJSON Annotation
instance ToJSON Detection

instance ToJSON EditAction
instance ToJSON HistoryEntry
instance ToJSON Edit
instance ToJSON EditCmd

instance ToJSON Navigation
instance ToJSON ConfigUpdate
instance ToJSON NaturalKey
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
    , gamma = 1.0
    , brightness = 0.0
    , contrast = 1.0

    , detection = def
    , threshold = 0.5
    , ordering = OrderMixed
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

data HashedKey k = HashedKey { unKey :: k, hashedKey :: Word32 }
  deriving Show

hashKey :: (Hashable k) => k -> HashedKey k
hashKey k = HashedKey k (fromIntegral $ hash k)

instance (Hashable a, Eq a) => Eq (HashedKey a) where
  (==) k k' = hashedKey k == hashedKey k' && unKey k == unKey k'

instance (Hashable a, Ord a) => Ord (HashedKey a) where
  compare k k' = case compare (hashedKey k) (hashedKey k') of
    GT -> GT
    LT -> LT
    EQ -> compare (unKey k) (unKey k')

hashKeys :: (Ord k, Hashable k) => Map k a -> Map (HashedKey k) a
hashKeys = M.mapKeys hashKey

unNatural :: NaturalKey -> Text
unNatural (NaturalKey xs) = Text.concat (show' <$> xs) where
  show' (Left i)  = Text.pack (show i)
  show' (Right t) = t


emptyCollection :: Collection
emptyCollection = Collection mempty

makePrisms ''Navigation

makePrisms ''ClientMsg
makePrisms ''ServerMsg
makePrisms ''Shape
