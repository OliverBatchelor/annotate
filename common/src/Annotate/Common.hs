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
import qualified Data.Aeson.Types as Aeson

import qualified Text.Fuzzy as Fuzzy

import Data.Ord (comparing)
import Data.List (sortBy, drop, dropWhile)

import Data.GADT.Compare.TH


type AnnotationId = Int
type ClientId = Int
type UserId = Int

type ClassId = Int

type DocName = Text
type DateTime = UTCTime

type Epoch = Int
type RunId = Int

type NetworkId = (RunId, Epoch)

data Shape = ShapeBox     Box
           | ShapeCircle  Circle
           | ShapePolygon Polygon
           | ShapeLine    WideLine
     deriving (Generic, Show, Eq)


data ShapeKey a where
  BoxKey      :: ShapeKey Box
  CircleKey   :: ShapeKey Circle
  PolygonKey  :: ShapeKey Polygon
  LineKey     :: ShapeKey WideLine


deriving instance Eq a => Eq (ShapeKey a)

shapeKey :: Shape -> DSum ShapeKey Identity
shapeKey (ShapeBox b)     = BoxKey      :=> Identity b
shapeKey (ShapeCircle c)  = CircleKey   :=> Identity c
shapeKey (ShapePolygon p) = PolygonKey  :=> Identity p
shapeKey (ShapeLine l)    = LineKey    :=> Identity l


deriveGEq ''ShapeKey
deriveGCompare ''ShapeKey

data ShapeConfig = ConfigCircle | ConfigBox | ConfigPolygon | ConfigLine
  deriving (Generic, Show, Eq, Ord)
 

instance HasBounds Shape where
 getBounds (ShapeCircle s)  = getBounds s
 getBounds (ShapeBox s)     = getBounds s
 getBounds (ShapePolygon s) = getBounds s
 getBounds (ShapeLine s)    = getBounds s

data Detection = Detection
  { label      :: ClassId
  , shape      :: Shape
  , confidence :: Float
  } deriving (Generic, Show, Eq)

data ShapeTag 
  = Detected
  | Positive
  | Negative 
  | Confirmed
  | Deleted
    deriving (Generic, Show, Eq)
  
data Annotation = Annotation
  { shape :: Shape
  , label :: ClassId
  , detection :: Maybe (ShapeTag, Detection)
  } deriving (Generic, Show, Eq)

data BasicAnnotation = BasicAnnotation 
  { shape :: Shape
  , label :: ClassId
  } deriving (Generic, Show, Eq)

type AnnotationMap = Map AnnotationId Annotation

type DocParts = Map AnnotationId (Set Int)
type Rigid = (Float, Vec)

data Edit
  = EditSetClass ClassId (Set AnnotationId)
  | EditDeleteParts DocParts
  | EditTransformParts Rigid DocParts
  | EditClearAll
  | EditDetection [Detection]
  | EditSetArea (Maybe Box)
  | EditAdd [BasicAnnotation]
  | EditConfirmDetection (Set AnnotationId)

  deriving (Generic, Show, Eq)

data AnnotationPatch
  = Add Annotation
  | Delete
  | Modify Annotation
  deriving (Generic, Show, Eq)


data DocumentPatch
    = PatchAnns (Map AnnotationId AnnotationPatch)
    | PatchArea (Maybe Box)
  deriving (Eq, Show, Generic)


data HistoryEntry 
  = HistoryOpen 
  | HistorySubmit 
  | HistoryEdit Edit 
  | HistoryUndo 
  | HistoryRedo 
  | HistoryClose 
  | HistoryOpenNew [Detection]
  | HistoryOpenReview [Detection]
  deriving (Show, Eq, Generic)

data EditCmd = DocEdit Edit | DocUndo | DocRedo
  deriving (Show, Eq, Generic)


newtype NaturalKey = NaturalKey [Either (Int, Text) Text]
  deriving (Ord, Eq, Generic, Show)


data Detections = Detections 
  { detections :: [Detection]
  , networkId :: NetworkId
  } deriving (Show, Eq, Generic)


data Document = Document
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: Map AnnotationId BasicAnnotation
  , validArea   :: Maybe Box

  , history :: [(UTCTime, HistoryEntry)]
  , detections :: Maybe Detections
  } deriving (Generic, Show, Eq)


data ImageCat = CatNew | CatTrain | CatTest | CatDiscard 
  deriving (Eq, Ord, Enum, Generic)

instance Show ImageCat where
  show CatNew = "new"
  show CatTest = "test"
  show CatTrain = "train"
  show CatDiscard = "discard"


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




data SortKey = SortCat | SortNum | SortName | SortModified | SortMixed
  deriving (Eq, Show, Generic)

data FilterOption = FilterAll | FilterCat ImageCat | FilterEdited
  deriving (Eq, Generic)

instance Show FilterOption where
  show FilterAll        = "all"
  show (FilterCat cat)  = show cat
  show FilterEdited     = "edited"


data SortOptions = SortOptions 
  { sortKey   :: SortKey
  , reversed :: Bool 
  , filtering :: FilterOption
  , search   :: Text
  } deriving (Show, Generic, Eq)
  


data Preferences = Preferences
  { controlSize       :: Float
  , brushSize         :: Float

  , instanceColours   :: Bool

  , opacity           :: Float
  , border            :: Float

  , hiddenClasses     :: Set Int

  , gamma             :: Float
  , brightness        :: Float
  , contrast          :: Float

  , detection    :: DetectionParams

  , threshold    :: Float
  , margin       :: Float

  , sortOptions :: SortOptions
  , autoDetect  :: Bool

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
  = ServerHello ClientId Preferences Config TrainerStatus
  | ServerConfig Config
  | ServerCollection Collection
  | ServerUpdateInfo DocName DocInfo
  | ServerDocument NavId Document
  | ServerOpen (Maybe DocName) ClientId DateTime
  | ServerError ErrCode
  | ServerDetection DocName Detections
  | ServerStatus TrainerStatus
      deriving (Generic, Show, Eq)

 
data Progress = Progress { activity :: TrainerActivity, progress :: (Int, Int) }
  deriving (Generic, Show, Eq)


data TrainerActivity
  = ActivityTrain { epoch :: Epoch }
  | ActivityTest  { epoch :: Epoch }
  | ActivityReview
  | ActivityDetect
  deriving (Generic,  Eq)


instance Show TrainerActivity where
  show (ActivityTrain epoch) = "Train " <> show epoch
  show (ActivityTest epoch) = "Test " <> show epoch

  show (ActivityReview) = "Review"
  show (ActivityDetect) = "Review"

data TrainerStatus 
  = StatusDisconnected
  | StatusPaused
  | StatusTraining Progress
  deriving (Generic, Show, Eq)


data StatusKey a where
  DisconnectedKey :: StatusKey ()
  PausedKey       :: StatusKey ()
  TrainingKey    :: StatusKey Progress
    
  
trainerKey :: TrainerStatus -> DSum StatusKey Identity
trainerKey StatusDisconnected = DisconnectedKey :=> Identity ()
trainerKey StatusPaused       = PausedKey   :=> Identity ()
trainerKey (StatusTraining p) = TrainingKey :=> Identity p

deriveGEq ''StatusKey
deriveGCompare ''StatusKey

data UserCommand 
  = UserPause
  | UserResume
  | UserReview
  | UserDetect
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
  | ClientCommand UserCommand
      deriving (Generic, Show, Eq)


dropCamel :: String -> String
dropCamel name = case f name of 
  ""     -> error ("empty JSON constructor after prefix removed: " <> name)
  result -> result
  where
    f = drop 1 . dropWhile (/= '_') . camel 

camel :: String -> String 
camel = Aeson.camelTo2 '_'

options :: Aeson.Options
options = Aeson.defaultOptions { Aeson.constructorTagModifier = dropCamel }      

instance FromJSON Hash32 where
  parseJSON (Aeson.String v) = return $ Hash32 $ read (Text.unpack v)
  parseJSON _          = fail "expected string value"

instance ToJSON Hash32 where
  toJSON (Hash32 v) = Aeson.String (Text.pack (show v))



instance FromJSON ShapeConfig where parseJSON = Aeson.genericParseJSON options
instance FromJSON ClassConfig where parseJSON = Aeson.genericParseJSON options

instance FromJSON DetectionParams where parseJSON = Aeson.genericParseJSON options

instance FromJSON Preferences where parseJSON = Aeson.genericParseJSON options

instance FromJSON ImageCat    where parseJSON = Aeson.genericParseJSON options
instance FromJSON Shape       where parseJSON = Aeson.genericParseJSON options
instance FromJSON Annotation      where parseJSON = Aeson.genericParseJSON options
instance FromJSON BasicAnnotation where parseJSON = Aeson.genericParseJSON options
instance FromJSON ShapeTag        where parseJSON = Aeson.genericParseJSON options
instance FromJSON Detection   where parseJSON = Aeson.genericParseJSON options
instance FromJSON Detections   where parseJSON = Aeson.genericParseJSON options

instance FromJSON AnnotationPatch where parseJSON = Aeson.genericParseJSON options
instance FromJSON HistoryEntry    where parseJSON = Aeson.genericParseJSON options

instance FromJSON Edit    where parseJSON = Aeson.genericParseJSON options
instance FromJSON EditCmd where parseJSON = Aeson.genericParseJSON options

instance FromJSON Navigation    where parseJSON = Aeson.genericParseJSON options
instance FromJSON ConfigUpdate  where parseJSON = Aeson.genericParseJSON options
instance FromJSON NaturalKey    where parseJSON = Aeson.genericParseJSON options

instance FromJSON Document     where parseJSON = Aeson.genericParseJSON options
instance FromJSON Config       where parseJSON = Aeson.genericParseJSON options
instance FromJSON DocInfo      where parseJSON = Aeson.genericParseJSON options
instance FromJSON Collection   where parseJSON = Aeson.genericParseJSON options
instance FromJSON ServerMsg    where parseJSON = Aeson.genericParseJSON options
instance FromJSON ClientMsg    where parseJSON = Aeson.genericParseJSON options
instance FromJSON ErrCode      where parseJSON = Aeson.genericParseJSON options

instance FromJSON Progress      where parseJSON = Aeson.genericParseJSON options
instance FromJSON TrainerStatus where parseJSON = Aeson.genericParseJSON options


instance FromJSON SortKey       where parseJSON = Aeson.genericParseJSON options
instance FromJSON FilterOption  where parseJSON = Aeson.genericParseJSON options
instance FromJSON SortOptions   where parseJSON = Aeson.genericParseJSON options


instance ToJSON ShapeConfig  where toJSON = Aeson.genericToJSON options
instance ToJSON ClassConfig  where toJSON = Aeson.genericToJSON options

instance ToJSON DetectionParams where toJSON = Aeson.genericToJSON options
instance ToJSON Preferences     where toJSON = Aeson.genericToJSON options

instance ToJSON ImageCat    where toJSON = Aeson.genericToJSON options
instance ToJSON Shape       where toJSON = Aeson.genericToJSON options  
instance ToJSON Annotation  where toJSON = Aeson.genericToJSON options
instance ToJSON BasicAnnotation where toJSON = Aeson.genericToJSON options

instance ToJSON ShapeTag  where toJSON = Aeson.genericToJSON options
instance ToJSON Detection where toJSON = Aeson.genericToJSON options
instance ToJSON Detections where toJSON = Aeson.genericToJSON options


instance ToJSON AnnotationPatch where toJSON = Aeson.genericToJSON options
instance ToJSON HistoryEntry    where toJSON = Aeson.genericToJSON options

instance ToJSON Edit    where toJSON = Aeson.genericToJSON options
instance ToJSON EditCmd where toJSON = Aeson.genericToJSON options

instance ToJSON Navigation    where toJSON = Aeson.genericToJSON options
instance ToJSON ConfigUpdate  where toJSON = Aeson.genericToJSON options
instance ToJSON NaturalKey    where toJSON = Aeson.genericToJSON options
instance ToJSON Document  where toJSON = Aeson.genericToJSON options
instance ToJSON Config    where toJSON = Aeson.genericToJSON options
instance ToJSON DocInfo   where toJSON = Aeson.genericToJSON options
instance ToJSON Collection  where toJSON = Aeson.genericToJSON options
instance ToJSON ServerMsg   where toJSON = Aeson.genericToJSON options
instance ToJSON ClientMsg   where toJSON = Aeson.genericToJSON options
instance ToJSON ErrCode     where toJSON = Aeson.genericToJSON options

instance ToJSON SortKey  where toJSON = Aeson.genericToJSON options
instance ToJSON FilterOption where toJSON = Aeson.genericToJSON options
instance ToJSON SortOptions  where toJSON = Aeson.genericToJSON options

instance ToJSON Progress      where toJSON = Aeson.genericToJSON options
instance ToJSON TrainerStatus where toJSON = Aeson.genericToJSON options


instance ToJSON TrainerActivity   where toJSON = Aeson.genericToJSON options
instance ToJSON UserCommand       where toJSON = Aeson.genericToJSON options

instance FromJSON TrainerActivity where parseJSON = Aeson.genericParseJSON options
instance FromJSON UserCommand where parseJSON = Aeson.genericParseJSON options
  


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
    , border = 1


    , hiddenClasses = mempty
    , gamma = 1.0
    , brightness = 0.0
    , contrast = 1.0

    , detection = def
    , threshold = 0.5
    , margin = 0.1

    , sortOptions = def
    , autoDetect = True
    }

instance Default SortOptions where
  def = SortOptions 
    { sortKey = SortMixed
    , reversed = False
    , filtering = FilterAll
    , search = ""
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
  , shape   = ConfigBox
  }

fromBasic :: BasicAnnotation -> Annotation
fromBasic BasicAnnotation{..} = Annotation{..} where
  detection = Nothing

toBasic :: Annotation -> BasicAnnotation
toBasic Annotation{..} = BasicAnnotation{..}  


getConfidence :: Annotation -> Float
getConfidence Annotation{detection} = case detection of 
  Just (Detected, d)  -> d ^. #confidence
  Just (Deleted, _) -> 0.0
  _                 -> 1.0


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
  show' (Left (i, t))  = t
  show' (Right t) = t


emptyCollection :: Collection
emptyCollection = Collection mempty


filterImage :: FilterOption -> (DocName, DocInfo) -> Bool
filterImage opt (_, DocInfo{category}) = case opt of
  FilterAll     -> category /= CatDiscard
  FilterEdited  -> category == CatTrain || category == CatTest
  FilterCat cat -> category == cat


reverseComparing :: Ord b => (a -> b) -> a -> a -> Ordering
reverseComparing f x y = reverseOrdering $ compare (f x) (f y)

reverseOrdering :: Ordering -> Ordering
reverseOrdering LT = GT
reverseOrdering GT = LT
reverseOrdering EQ = EQ

compareWith ::  Bool -> SortKey -> (DocName, DocInfo) -> (DocName, DocInfo) -> Ordering
compareWith rev key = (case key of
  SortCat         -> compares (view #category &&& view #naturalKey)
  SortNum         -> compares (view #numAnnotations &&& view #naturalKey)
  SortModified    -> compares (view #modified &&& view #naturalKey)
  SortName        -> compares (view #naturalKey)
  SortMixed       -> compares (view #hashedName))
  where
    compares :: forall a. Ord a => (DocInfo -> a) -> (DocName, DocInfo) -> (DocName, DocInfo) -> Ordering
    compares f = if rev then reverseComparing (f . snd) else comparing (f . snd)



sortImages :: SortOptions -> [(DocName, DocInfo)] -> [(DocName, DocInfo)]
sortImages SortOptions{..} = searchImages search . sortBy (compareWith reversed sortKey) . filter (filterImage filtering)

searchImages :: Text ->  [(DocName, DocInfo)] -> [(DocName, DocInfo)]
searchImages ""   = id
searchImages str  = \images -> Fuzzy.original <$>
  Fuzzy.filter str images "" "" fst False


makePrisms ''Navigation

makePrisms ''ClientMsg
makePrisms ''ServerMsg
makePrisms ''Shape


