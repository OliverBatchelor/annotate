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

import qualified Text.Fuzzy as Fuzzy

import Data.Ord (comparing)
import Data.List (sortBy)

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

data Tag 
  = Detected
  | Positive
  | Negative 
  | Confirmed
  | Deleted
    deriving (Generic, Show, Eq)
  
data Annotation = Annotation
  { shape :: Shape
  , label :: ClassId
  , detection :: Maybe (Tag, Detection)
  } deriving (Generic, Show, Eq)

data BasicAnnotation = BasicAnnotation 
  { shape :: Shape
  , label :: ClassId
  } deriving (Generic, Show, Eq)

type AnnotationMap = Map AnnotationId Annotation

type DocParts = Map AnnotationId (Set Int)
type Rigid = (Float, Vec)

data Edit
  = SetClassEdit ClassId (Set AnnotationId)
  | DeletePartsEdit DocParts
  | TransformPartsEdit Rigid DocParts
  | ClearAllEdit
  | DetectionEdit [Detection]
  | SetAreaEdit (Maybe Box)
  | AddEdit [BasicAnnotation]
  | ConfirmDetectionEdit (Set AnnotationId)

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
  = HistOpen 
  | HistSubmit 
  | HistEdit Edit 
  | HistUndo 
  | HistRedo 
  | HistClose 
  | HistDetections [Detection] 
  | HistReview [Detection] 

  deriving (Show, Eq, Generic)

data EditCmd = DocEdit Edit | DocUndo | DocRedo
  deriving (Show, Eq, Generic)


newtype NaturalKey = NaturalKey [Either (Int, Text) Text]
  deriving (Ord, Eq, Generic, Show)



data Document = Document
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: Map AnnotationId BasicAnnotation
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




data SortKey = SortCat | SortNum | SortName | SortModified | SortMixed
  deriving (Eq, Show, Generic)

data FilterOption = FilterAll | FilterCat ImageCat | FilterEdited
  deriving (Eq, Generic)

instance Show FilterOption where
  show FilterAll        = "All"
  show (FilterCat cat)  = show cat
  show FilterEdited     = "Edited"


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

instance FromJSON Preferences

instance FromJSON ImageCat
instance FromJSON Shape
instance FromJSON Annotation
instance FromJSON BasicAnnotation
instance FromJSON Tag
instance FromJSON Detection

instance FromJSON AnnotationPatch
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

instance FromJSON SortKey
instance FromJSON FilterOption
instance FromJSON SortOptions

instance ToJSON ShapeConfig
instance ToJSON ClassConfig

instance ToJSON DetectionParams
instance ToJSON Preferences

instance ToJSON ImageCat
instance ToJSON Shape
instance ToJSON Annotation
instance ToJSON BasicAnnotation

instance ToJSON Tag
instance ToJSON Detection

instance ToJSON AnnotationPatch
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

instance ToJSON SortKey
instance ToJSON FilterOption
instance ToJSON SortOptions

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
  , shape   = BoxConfig
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
  FilterAll     -> category /= Discard
  FilterEdited  -> category == Train || category == Test
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


