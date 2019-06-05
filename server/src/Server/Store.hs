module Server.Store where

import Annotate.Prelude
import Server.Common

import qualified Data.Map as M
import qualified Data.Set as S

import Data.SafeCopy

import Control.Concurrent.Log
import qualified Data.Text as Text

import Data.List (transpose, (!!), takeWhile, dropWhile, scanl)
import Control.Lens (hasn't, _Cons)

import Data.Maybe (fromJust)
import Annotate.Editor


data OldHistoryEntry = HistOpen | HistSubmit | HistEdit DocumentPatch | HistUndo | HistRedo
  deriving (Show, Eq, Generic)


data Edit0
  = EditSetClass0 ClassId (Set AnnotationId)
  | EditDeleteParts0 DocParts
  | EditTransformParts0 Rigid DocParts
  | EditClearAll0
  | EditDetection0 [Detection]
  | EditSetArea0 (Maybe Box)
  | EditAdd0 [BasicAnnotation]
  | EditConfirmDetection0 (Set AnnotationId)    


data Edit1
  = EditSetClass1 ClassId (Set AnnotationId)
  | EditDeleteParts1 DocParts
  | EditTransformParts1 Rigid DocParts
  | EditClearAll1
  | EditDetection1 [Detection]
  | EditSetArea1 (Maybe Box)
  | EditAdd1 [BasicAnnotation]
  | EditConfirmDetection1 (Map AnnotationId Bool)
  deriving (Generic, Show, Eq)  


instance Migrate Edit1 where
  type MigrateFrom Edit1 = Edit0
  migrate (EditSetClass0 k s) = EditSetClass1 k s 
  migrate (EditDeleteParts0 k)  = EditDeleteParts1 k 
  migrate (EditTransformParts0 k s) = EditTransformParts1 k s 
  migrate EditClearAll0  = EditClearAll1
  migrate (EditDetection0 _) = EditClearAll1 
  migrate (EditSetArea0 b) = EditSetArea1 b
  migrate (EditAdd0 a) = EditAdd1 a
  migrate (EditConfirmDetection0 s) = EditConfirmDetection1 (setToMap True s)


instance Migrate Edit where
  type MigrateFrom Edit = Edit1
  migrate (EditSetClass1 k s) = EditSetClass k s 
  migrate (EditDeleteParts1 k)  = EditDeleteParts k 
  migrate (EditTransformParts1 k s) = EditTransformParts k s 
  migrate EditClearAll1  = EditClearAll
  migrate (EditDetection1 _) = error "deprecated" 
  migrate (EditSetArea1 b) = error "deprecated" 
  migrate (EditAdd1 a) = EditAdd a
  migrate (EditConfirmDetection1 s) = EditConfirmDetection s



data HistoryEntry0
  = HistoryOpen0 
  | HistorySubmit0 
  | HistoryEdit0 Edit 
  | HistoryUndo0 
  | HistoryRedo0 
  | HistoryClose0 
  | HistoryOpenNew0 [Detection]
  | HistoryOpenReview0 [Detection]
    deriving (Show, Eq, Generic)  
  


migrateHistory :: [(UTCTime, HistoryEntry0)] -> [(UTCTime, HistoryEntry)]
migrateHistory history = catMaybes (f' <$> history)
  where 
    f' (t, h) = (t,) <$> f h    

    f HistoryOpen0 = Just $ HistoryOpen (openSession OpenDisconnected)
    f (HistoryEdit0 e) = Just $ HistoryEdit e
    f HistoryUndo0 = Just $ HistoryUndo
    f HistoryRedo0 = Just $ HistoryRedo
    f HistoryClose0 = Just $ HistoryClose
    f (HistoryOpenNew0 d) = Just $ HistoryOpen (openSession $ OpenNew (detections' d))
    f (HistoryOpenReview0 d) = Just $ HistoryOpen (openSession $ OpenReview (detections' d))
    f _ = Nothing    

    openSession :: OpenType -> OpenSession
    openSession t = OpenSession 
      { openType = t
      , threshold = 0.5
      , initial = mempty
      } 

    detections' :: [Detection] -> Detections
    detections' instances = Detections 
      { instances = instances
      , networkId = def
      , stats = def
      }

    -- data HistoryEntry 
    -- = HistoryEdit Edit 
    -- | HistoryUndo 
    -- | HistoryRedo 
    -- | HistoryThreshold Float
    -- | HistoryOpen OpenSession
    -- | HistoryClose 
    -- deriving (Show, Eq, Generic)   
    

data ClassConfig0 = ClassConfig0
  { name :: Text
  , shape :: ShapeConfig
  , colour :: HexColour
  } deriving (Generic, Show, Eq)
  

instance Migrate ClassConfig1 where
  type MigrateFrom ClassConfig1 = ClassConfig0
  migrate ClassConfig0{..} = ClassConfig1{..}
    where weighting = 0.25


data ClassConfig1 = ClassConfig1
  { name :: Text
  , shape :: ShapeConfig
  , colour :: HexColour
  , weighting :: Float
  } deriving (Generic, Show, Eq)

instance Migrate ClassConfig where
  type MigrateFrom ClassConfig = ClassConfig1
  migrate ClassConfig1{..} = ClassConfig{..}
    where countWeight = 1


data DocInfo0 = DocInfo0
  { modified    :: Maybe DateTime
  , numAnnotations :: Int
  , category    :: ImageCat
  , imageSize   :: (Int, Int)
  } deriving (Generic, Show, Eq)

data DocInfo1 = DocInfo1
  { hashedName :: Int
  , naturalKey :: NaturalKey
  , modified    :: Maybe DateTime
  , numAnnotations :: Int
  , category    :: ImageCat
  , imageSize   :: (Int, Int)
  } deriving (Generic, Show, Eq)


data DocInfo2 = DocInfo2
  { hashedName :: Word32
  , naturalKey :: NaturalKey
  , modified    :: Maybe DateTime
  , numAnnotations :: Int
  , category    :: ImageCat
  , imageSize   :: (Int, Int)
  } deriving (Generic, Show, Eq)

data DocInfo3 = DocInfo3
  { hashedName :: Hash32
  , naturalKey :: NaturalKey
  , modified    :: Maybe DateTime
  , numAnnotations :: Int
  , category    :: ImageCat
  , imageSize   :: (Int, Int)
  } deriving (Generic, Show, Eq)

data DocInfo4 = DocInfo4
  { hashedName :: Hash32
  , naturalKey :: NaturalKey
  , modified    :: Maybe DateTime
  , numAnnotations :: Int
  , category    :: ImageCat
  , imageSize   :: (Int, Int)
  , detections  :: Maybe DetectionStats
  , lossMean     :: Float
  , lossMax     :: Float
  } deriving (Generic, Show, Eq)

data DocInfo5 = DocInfo5
  { hashedName :: Hash32
  , naturalKey :: NaturalKey
  , modified    :: Maybe DateTime
  , numAnnotations :: Int
  , category    :: ImageCat
  , imageSize   :: (Int, Int)
  , detections  :: Maybe DetectionStats
  , training   :: TrainStats
  } deriving (Generic, Show, Eq)  

data DocInfo6 = DocInfo6
  { hashedName :: Hash32
  , naturalKey :: NaturalKey
  , modified    :: Maybe DateTime
  , numAnnotations :: Int
  , category    :: ImageCat
  , imageSize   :: (Int, Int)

  , detections :: Maybe DetectionStats
  , training   :: TrainStats
  , reviews    :: Int
  } deriving (Generic, Eq, Show)





data Document14 = Document14
  { name  :: DocName
  , info  :: DocInfo
  , annotations    :: BasicAnnotationMap
  , validArea      :: Maybe Box
  , history       :: [(UTCTime, HistoryEntry)]
  , detections     :: Maybe Detections
  , training       :: [TrainSummary]
  } deriving (Generic, Show)  

data Document13 = Document13
  { name  :: DocName
  , info  :: DocInfo
  , annotations    :: BasicAnnotationMap
  , validArea      :: Maybe Box
  , history       :: [(UTCTime, HistoryEntry0)]
  , detections     :: Maybe Detections
  , training       :: [TrainSummary]
  } deriving (Generic, Show)  
  
data Document12 = Document12
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: BasicAnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, HistoryEntry0)]
  , detections :: Maybe Detections
  , trainingLoss   :: [Float]
  } deriving (Generic, Show)  


data Document11 = Document11
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: BasicAnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, HistoryEntry0)]
  , detections :: Maybe Detections
  } deriving (Generic, Show)  

data Document10 = Document10
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: BasicAnnotationMap
  , validArea   :: Maybe Box

  , history :: [(UTCTime, HistoryEntry0)]
  , detections :: Maybe ([Detection], NetworkId)
  } deriving (Generic, Show)  

data Document9 = Document9
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, HistoryEntry0)]
  , detections :: Maybe ([Detection], NetworkId)

  } deriving (Generic, Show)

data Document8 = Document8
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, HistoryEntry0)]
  , detections :: Maybe ([Detection], NetworkId)

  } deriving (Generic, Show)


data Document7 = Document7
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, OldHistoryEntry)]
  , detections :: Maybe ([Detection], NetworkId)

  } deriving (Generic, Show)


data Document6 = Document6
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, OldHistoryEntry)]
  } deriving (Generic, Show)


data Document5 = Document5
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, OldHistoryEntry)]
  } deriving (Generic, Show)

data Document4 = Document4
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, OldHistoryEntry)]
  } deriving (Generic, Show)


data Document3 = Document3
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea   :: Maybe Box
  } deriving (Generic, Show)

data Document2 = Document2
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  } deriving (Generic, Show)

data Document1 = Document1
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  } deriving (Generic, Show)


data Command0 where
  CmdCategory0 :: DocName -> ImageCat -> Command0
  CmdUpdate0 :: Document -> UTCTime  -> Command0
  CmdModified0 :: DocName -> UTCTime -> Command0
  CmdImages0 :: [(DocName, DocInfo)] -> Command0
  CmdClass0 :: ClassId -> Maybe ClassConfig -> Command0
  CmdSetRoot0  :: Text -> Command0
  CmdCheckpoint0 :: NetworkId -> Float -> Bool -> Command0
  CmdPreferences0 :: UserId -> Preferences -> Command0
  CmdDetections0  :: [(DocName, [Detection])] -> NetworkId -> Command0
    deriving (Show,  Generic)

data Command1 where
  CmdCategory1     :: DocName -> ImageCat -> Command1
  CmdUpdate1       :: Document -> UTCTime  -> Command1
  CmdModified1     :: DocName -> UTCTime -> Command1
  CmdImages1       :: [(DocName, DocInfo)] -> Command1
  CmdClass1        :: ClassId -> Maybe ClassConfig -> Command1
  CmdSetRoot1      :: Text -> Command1
  CmdCheckpoint1   :: Checkpoint -> Command1
  CmdPreferences1  :: UserId -> Preferences -> Command1
  CmdDetections1   :: Map DocName Detections -> Command1
  CmdSubmit1       :: Submission -> UTCTime  -> Command1
  CmdTraining1     :: Map DocName [TrainSummary] -> Command1
    deriving (Show, Generic)


instance Migrate Command1 where
  type MigrateFrom Command1 = Command0
  migrate (CmdCategory0 k c) = CmdCategory1 k c
  migrate (CmdUpdate0 k c) = CmdUpdate1 k c
  migrate (CmdModified0 k c) = CmdModified1 k c
  migrate (CmdImages0 m) = CmdImages1 m
  migrate (CmdClass0 c conf) = CmdClass1 c conf
  migrate (CmdSetRoot0 r) = CmdSetRoot1 r
  migrate (CmdCheckpoint0 k f b) = CmdCheckpoint1 $ Checkpoint k f b
  migrate (CmdPreferences0 k p) = CmdPreferences1 k p
  migrate (CmdDetections0 ds netId)   = CmdDetections1 $ f <$> M.fromList ds
    where f dets = Detections dets netId def

instance Migrate Command where
  type MigrateFrom Command = Command1
  migrate (CmdCategory1 k c) = CmdCategory k c
  migrate (CmdUpdate1 k c) = CmdUpdate k c
  migrate (CmdModified1 k c) = CmdModified k c
  migrate (CmdImages1 m) = CmdImages m
  migrate (CmdClass1 c conf) = CmdClass c conf
  migrate (CmdSetRoot1 r) = CmdSetRoot r
  migrate (CmdCheckpoint1 cp) = CmdCheckpoint cp
  migrate (CmdPreferences1 k p) = CmdPreferences k p
  migrate (CmdDetections1 ds)   = CmdDetections ds
  migrate (CmdSubmit1 s t)   = CmdSubmit 0 s t
  migrate (CmdTraining1 ds)   = CmdTraining ds


data ImageSelection0
  = SelSequential0 Bool
  | SelRandom0
  | SelDetections0 Bool
  | SelLoss0
  deriving (Eq, Show, Generic)



data ImageOrdering = OrderSequential | OrderMixed | OrderBackwards
  deriving (Show, Eq, Ord, Enum, Generic)






data SortOptions0 = SortOptions0 
  { sortKey   :: SortKey
  , reversed :: Bool 
  , filtering :: FilterOption
  , search   :: Text
  } deriving (Show, Generic, Eq)

data SortOptions1 = SortOptions1 
  { sortKey   :: SortKey
  , reversed :: Bool 
  , filtering :: FilterOption
  , search   :: Text
  , restrictClass :: Maybe ClassId
  } deriving (Show, Generic, Eq)  

data SortOptions3 = SortOptions3 
  { sortKey   :: SortKey
  , selection :: ImageSelection0
  , reversed  :: Bool 
  , filtering :: FilterOption
  , negFilter :: Bool
  , search    :: Text
  , restrictClass :: Maybe ClassId
  } deriving (Show, Generic, Eq)

data SortOptions2 = SortOptions2 
  { sortKey   :: SortKey
  , selection :: ImageSelection0
  , reversed  :: Bool 
  , filtering :: FilterOption
  , search    :: Text
  , restrictClass :: Maybe ClassId
  } deriving (Show, Generic, Eq)


data SortOptions4 = SortOptions4
  { sorting  :: (SortKey, Bool)
  , selection :: ImageSelection0
  , filtering :: (FilterOption, Bool)
  , search    :: Text
  , restrictClass :: Maybe ClassId
  } deriving (Show, Generic, Eq)    


data DisplayPreferences0 = DisplayPreferences0
  { controlSize       :: Float
  , brushSize         :: Float
  , instanceColours   :: Bool
  , showConfidence    :: Bool
  , opacity           :: Float
  , border            :: Float
  , hiddenClasses     :: Set Int
  , gamma             :: Float
  , brightness        :: Float
  , contrast          :: Float
  } deriving (Generic, Show, Eq)  


data Preferences6 = Preferences6
  { display      :: DisplayPreferences
  , detection    :: DetectionParams
  , thresholds    :: (Float, Float)
  , sortOptions :: SortOptions
  , autoDetect  :: Bool
  , assignMethod   :: AssignmentMethod
  , trainRatio  :: Int
  } deriving (Generic, Show, Eq)



data Preferences5 = Preferences5
  { display      :: DisplayPreferences
  , detection    :: DetectionParams
  , thresholds    :: (Float, Float)
  , sortOptions :: SortOptions
  , autoDetect  :: Bool
  , assignMethod   :: AssignmentMethod
  , assignRatios  :: (Int, Int)
  } deriving (Generic, Show, Eq)


data Preferences4 = Preferences4
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


data Preferences3 = Preferences3
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
  , infoFields  :: [SortKey]
  } deriving (Generic, Show, Eq)

data Preferences2 = Preferences2
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


data Preferences1 = Preferences1
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
  , ordering    :: ImageOrdering
  } deriving (Generic, Show, Eq)

data Preferences0 = Preferences0
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

data Store0 = Store0
  { config    :: Config
  , images :: Map DocName Document
  } deriving (Show,  Generic)

data Store1 = Store1
  { config    :: Config
  , images :: Map DocName Document
  , trainer :: TrainerState
  } deriving (Show,  Generic)

instance Migrate Store1 where
  type MigrateFrom Store1 = Store0
  migrate Store0{..} = Store1{..}
    where trainer = def

instance Migrate Store where
  type MigrateFrom Store = Store1
  migrate Store1{..} = Store{..}
    where preferences = mempty


instance Migrate SortOptions where
  type MigrateFrom SortOptions = SortOptions4
  migrate SortOptions4{..} = SortOptions{..} where
    selection = SelSequential
    revSelection = False



instance Migrate SortOptions4 where
  type MigrateFrom SortOptions4 = SortOptions3
  migrate SortOptions3{..} = SortOptions4
    { sorting = (sortKey, reversed)
    , filtering = (filtering, negFilter)
    , selection, search, restrictClass } 
        

instance Migrate SortOptions3 where
  type MigrateFrom SortOptions3 = SortOptions2
  migrate SortOptions2{..} = SortOptions3{..} where 
    negFilter = False

instance Migrate SortOptions2 where
  type MigrateFrom SortOptions2 = SortOptions1
  migrate SortOptions1{..} = SortOptions2{..} where 
    selection = SelSequential0 False
        

instance Migrate SortOptions1 where
  type MigrateFrom SortOptions1 = SortOptions0
  migrate SortOptions0{..} = SortOptions1{..} where 
    restrictClass = Nothing


instance Migrate DisplayPreferences where
  type MigrateFrom DisplayPreferences = DisplayPreferences0
  migrate DisplayPreferences0{..} = DisplayPreferences{..}  where
    fontSize = 14
    


instance Migrate Preferences where
  type MigrateFrom Preferences = Preferences6
  migrate Preferences6{..} = Preferences{..}  where
    reviewing = False

instance Migrate Preferences6 where
  type MigrateFrom Preferences6 = Preferences5
  migrate Preferences5{..} = Preferences6{..}  where
    trainRatio = 5
        
    
instance Migrate Preferences5 where
  type MigrateFrom Preferences5 = Preferences4
  migrate Preferences4{..} = Preferences5{..}  where
    display = def
    assignRatios = (5, 1)
    assignMethod = AssignAuto
    
    thresholds = (threshold, margin)
    showConfidence = True



    
instance Migrate Preferences4 where
  type MigrateFrom Preferences4 = Preferences3
  migrate Preferences3{..} = Preferences4{..} 

instance Migrate Preferences3 where
  type MigrateFrom Preferences3 = Preferences2
  migrate Preferences2{..} = Preferences3{..} where 
    infoFields = [SortAnnotations]

instance Migrate Preferences2 where
  type MigrateFrom Preferences2 = Preferences1
  migrate Preferences1{..} = Preferences2{..} where 
    sortOptions = def
    autoDetect = True

instance Migrate Preferences1 where
  type MigrateFrom Preferences1 = Preferences0
  migrate Preferences0{..} = Preferences1{..}
    where 
      margin = 0.1
      border = 1
   

data Submission2 = Submission2
  { name        :: DocName
  , annotations :: BasicAnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, HistoryEntry)]
  , method :: SubmitType
  } deriving (Generic, Show)    

data Submission1 = Submission1
  { name        :: DocName
  , annotations :: BasicAnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, HistoryEntry0)]
  , method :: SubmitType
  } deriving (Generic, Show)      

data Submission0 = Submission0
  { name        :: DocName
  , annotations :: BasicAnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, HistoryEntry0)]
  , category :: Maybe ImageCat
  } deriving (Generic, Show)      


migrateSessions :: [HistoryPair] -> [Session]
migrateSessions = dropDuplicates .  historySessions' . reverse


historySessions' :: [HistoryPair] -> [Session]
historySessions' [] = []
historySessions' (e:entries) = case e of 
  (t, HistoryOpen open) -> (makeSession open t entries' : historySessions' rest)  where
    (entries', rest) = (takeWhile notOpen entries, dropWhile notOpen entries)
    notOpen          = hasn't (_2 . _HistoryOpen)
  _ -> error "historySessions: missing open"

dropDuplicates :: [Session] -> [Session]
dropDuplicates (x:y:xs) = if view #time y > view #time x 
  then x:dropDuplicates (y:xs)
  else dropDuplicates (y:xs)
dropDuplicates xs = xs

migrateSession :: [HistoryPair] ->  Session
migrateSession ((t, HistoryOpen open):entries) = makeSession open t entries
migrateSession entries = error $ "migrateSession: missing open, " <> show entries

makeSession ::  OpenSession -> UTCTime -> [HistoryPair] -> Session
makeSession OpenSession{..} t entries = Session 
  { initial, time = t, history = entries, open = openType, threshold = threshold }


instance Migrate Submission where
  type MigrateFrom Submission = Submission2
  migrate Submission2{..} = Submission{name, method, session = migrateSession (reverse history), annotations}


instance Migrate Submission2 where
  type MigrateFrom Submission2 = Submission1
  migrate Submission1{..} = Submission2{name, annotations, 
    validArea, method, history = migrateHistory history} 
    
  
instance Migrate Submission1 where
  type MigrateFrom Submission1 = Submission0
  migrate Submission0{..} = Submission1{..} where
    method = case category of
      Just cat -> SubmitNew
      _        -> SubmitAutoSave

data SubmitType0
  = SubmitNew0 
  | SubmitDiscard0 
  | SubmitConfirm0 
  | SubmitAutoSave0
    deriving (Show,  Generic)

instance Migrate SubmitType where
  type MigrateFrom SubmitType = SubmitType0
  migrate SubmitNew0 = SubmitNew
  migrate SubmitDiscard0 = SubmitDiscard
  migrate SubmitConfirm0 = SubmitConfirm Nothing
  migrate SubmitAutoSave0 = SubmitAutoSave

instance Migrate Document where
  type MigrateFrom Document = Document14
  migrate Document14{..} = (case checkReplays doc of 
    True  -> doc 
    False -> doc { sessions = [] })
      where doc = Document{name, sessions = migrateSessions history, 
        detections, info, annotations, training}

instance Migrate Document14 where
  type MigrateFrom Document14 = Document13
  migrate Document13{..} = Document14{name, history = migrateHistory history, 
    validArea, detections, info, annotations, training}


  
instance Migrate Document13 where
  type MigrateFrom Document13 = Document12
  migrate Document12{..} = Document13{..} where
    training = TrainSummary <$> trainingLoss
    

instance Migrate Document12 where
  type MigrateFrom Document12 = Document11
  migrate Document11{..} = Document12{..} where
    trainingLoss = []

instance Migrate Document11 where
  type MigrateFrom Document11 = Document10
  migrate Document10{..} = Document11
    {name, history, validArea, detections = update <$> detections 
    , info, annotations}

    where
      update (detections, netId) = Detections detections netId def

instance Migrate Document10 where
  type MigrateFrom Document10 = Document9
  migrate Document9{..} = Document10
    {name, history, validArea, detections
    , info, annotations = toBasic <$> annotations}


instance Migrate Document9 where
  type MigrateFrom Document9 = Document8
  migrate Document8{..} = Document9{..}
    

instance Migrate Document8 where
  type MigrateFrom Document8 = Document7
  migrate Document7{..} = Document8{..}
    where history = []
    

instance Migrate Document7 where
  type MigrateFrom Document7 = Document6
  migrate Document6{..} = Document7{..}
    where detections = Nothing

instance Migrate Document6 where
  type MigrateFrom Document6 = Document5
  migrate Document5{..} = Document6 {name, history, annotations, validArea, info = migrateInfo info} where
    migrateInfo info = info {naturalKey, hashedName} :: DocInfo
    naturalKey = makeNaturalKey name
    hashedName = Hash32 (fromIntegral (hash name))

instance Migrate Document5 where
  type MigrateFrom Document5 = Document4
  migrate Document4{..} = Document5{..}


instance Migrate Document4 where
  type MigrateFrom Document4 = Document3
  migrate Document3{..} = Document4 {..}
    where history = []

instance Migrate Document3 where
  type MigrateFrom Document3 = Document2
  migrate Document2{..} = Document3 {..}
    where validArea = Nothing

instance Migrate Document2 where
  type MigrateFrom Document2 = Document1
  migrate Document1{..} = Document2{..}  


instance Migrate DocInfo1 where
  type MigrateFrom DocInfo1 = DocInfo0
  migrate DocInfo0{..} = DocInfo1 {..}
    where naturalKey = NaturalKey []
          hashedName = 0
  

instance Migrate DocInfo2 where
  type MigrateFrom DocInfo2 = DocInfo1
  migrate DocInfo1{..} = DocInfo2
    { naturalKey, modified, numAnnotations
    , category, imageSize, hashedName = fromIntegral hashedName}

instance Migrate DocInfo3 where
  type MigrateFrom DocInfo3 = DocInfo2
  migrate DocInfo2{..} = DocInfo3
    { naturalKey, modified, numAnnotations
    , category, imageSize, hashedName = Hash32 (fromIntegral hashedName)}
    
instance Migrate DocInfo4 where
  type MigrateFrom DocInfo4 = DocInfo3
  migrate DocInfo3{..} = DocInfo4{..} where 
    detections = def
    lossMax   = 0
    lossMean  = 0    

instance Migrate DocInfo5 where
  type MigrateFrom DocInfo5 = DocInfo4
  migrate DocInfo4{..} = DocInfo5{..} where 
    training = TrainStats {..}
    lossRunning = 0

instance Migrate DocInfo6 where
  type MigrateFrom DocInfo6 = DocInfo5
  migrate DocInfo5{..} = DocInfo6{..} where 
    reviews = 0
        
instance Migrate DocInfo where
  type MigrateFrom DocInfo = DocInfo6
  migrate DocInfo6{..} = DocInfo{..} where 
    image = ImageInfo{size=imageSize, creation=Nothing}


type OldCount = Margins Count

data DetectionStats3 = DetectionStats3
  { score       :: Float
  , classScore  ::  Map ClassId Float
  , counts      :: Maybe OldCount
  , classCounts :: Maybe (Map ClassId OldCount)
  , frameVariation :: Maybe Float
  } deriving (Generic, Eq, Show)  


data DetectionStats2 = DetectionStats2
  { score     :: Float
  , classes   :: Map ClassId Float
  , counts :: Maybe (Map ClassId OldCount)
  , frameVariation :: Maybe Float
  } deriving (Generic, Eq, Show)  
  
data DetectionStats1 = DetectionStats1
  { score     :: Float
  , classes   :: Map ClassId Float
  , counts    :: Maybe (Map ClassId OldCount)    
  }

instance Migrate DetectionStats where
  type MigrateFrom DetectionStats = DetectionStats3
  migrate DetectionStats3{score, classScore, counts, classCounts, frameVariation} = DetectionStats
    {score, classScore, counts = fmap (fmap snd) counts, classCounts, frameVariation} 


instance Migrate DetectionStats3 where
  type MigrateFrom DetectionStats3 = DetectionStats2
  migrate DetectionStats2{score, classes, counts, frameVariation} = DetectionStats3
    {score, classScore = classes, counts = Nothing, classCounts = counts, frameVariation} 


instance Migrate DetectionStats2 where
  type MigrateFrom DetectionStats2 = DetectionStats1
  migrate DetectionStats1{..} = DetectionStats2{..} where 
    frameVariation = Nothing


data DetectionStats0 = DetectionStats0
  { score       :: Float
  , classes  :: Map ClassId Float
  } deriving (Generic, Eq, Show)
  
instance Migrate DetectionStats1 where
  type MigrateFrom DetectionStats1 = DetectionStats0
  migrate DetectionStats0{..} = DetectionStats1{..} where 
    counts = Nothing
    
data Detections0 = Detections0
  { instances :: [Detection]
  , networkId :: NetworkId
  } deriving (Show,  Generic)
  

instance Migrate Detections where
  type MigrateFrom Detections = Detections0
  migrate Detections0{..} = Detections{..} where 
    stats = def 

$(deriveSafeCopy 0 'base ''TrainStats)

$(deriveSafeCopy 0 'base ''DocInfo0)
$(deriveSafeCopy 1 'extension ''DocInfo1)
$(deriveSafeCopy 2 'extension ''DocInfo2)


$(deriveSafeCopy 0 'base ''Store0)
$(deriveSafeCopy 1 'extension ''Store1)


data Detection0 = Detection0
  { label      :: ClassId
  , bounds     :: Box
  , confidence :: Float
  } deriving (Generic, Show, Eq)

data Detection1 = Detection1
  { label      :: ClassId
  , shape      :: Shape
  , confidence :: Float
  } deriving (Generic, Show, Eq)

instance Migrate Detection where
  type MigrateFrom Detection = Detection1
  migrate Detection1{..} = Detection{..}
    where match = Nothing
  
instance Migrate Detection1 where
  type MigrateFrom Detection1 = Detection0
  migrate Detection0{..} = Detection1{..}
    where shape = ShapeBox bounds

$(deriveSafeCopy 0 'base ''Detection0)
$(deriveSafeCopy 1 'extension ''Detection1)



data Annotation1 = Annotation1 { shape :: Shape, label :: ClassId }
    deriving (Generic, Show, Eq)

data Annotation2 = Annotation2
  { shape :: Shape, label :: ClassId, detection :: Maybe Detection}
    deriving (Generic, Show, Eq)

data Annotation3 = Annotation3
  { shape :: Shape, label :: ClassId, detection :: Maybe Detection, confirm :: Bool}
    deriving (Generic, Show, Eq)
  
instance Migrate Annotation2 where
  type MigrateFrom Annotation2 = Annotation1
  migrate Annotation1{..} = Annotation2{shape, label, detection = Nothing}

instance Migrate Annotation3 where
  type MigrateFrom Annotation3 = Annotation2
  migrate Annotation2{..} = Annotation3{shape, label, detection = Nothing, confirm = True}
  
instance Migrate Annotation where
  type MigrateFrom Annotation = Annotation3
  migrate Annotation3{..} = Annotation{shape, label, detection = Nothing}

newtype NaturalKey0 = NaturalKey0 [Either Int Text]
  deriving (Ord, Eq, Generic, Show)

newtype NaturalKey1 = NaturalKey1 [Either (Int, Text) Text]
  deriving (Ord, Eq, Generic, Show)  


instance Migrate NaturalKey1 where
  type MigrateFrom NaturalKey1 = NaturalKey0
  migrate (NaturalKey0 ks) = NaturalKey1 (migrate' <$> ks) where
    migrate' (Left i)  = Left (i, Text.pack (show i))
    migrate' (Right t) = Right t

instance Migrate NaturalKey where
  type MigrateFrom NaturalKey = NaturalKey1
  migrate (NaturalKey1 ks) = NaturalKey (migrate' <$> ks) where
    migrate' (Left (i, t))  = Left i
    migrate' (Right t) = Right t
    

$(deriveSafeCopy 0 'base ''DetectionTag)
$(deriveSafeCopy 0 'base ''BasicAnnotation)


$(deriveSafeCopy 1 'base ''Annotation1)
$(deriveSafeCopy 2 'extension ''Annotation2)
$(deriveSafeCopy 3 'extension ''Annotation3)
$(deriveSafeCopy 4 'extension ''Annotation)

$(deriveSafeCopy 0 'base ''Hash32)


$(deriveSafeCopy 0 'base ''V2)
$(deriveSafeCopy 0 'base ''Box)
$(deriveSafeCopy 0 'base ''Circle)
$(deriveSafeCopy 0 'base ''Polygon)
$(deriveSafeCopy 0 'base ''WideLine)

$(deriveSafeCopy 0 'base ''Extents)
$(deriveSafeCopy 0 'base ''Shape)
$(deriveSafeCopy 2 'extension ''Detection)

$(deriveSafeCopy 0 'base ''TrainSummary)


$(deriveSafeCopy 0 'base ''Detections0)
$(deriveSafeCopy 1 'extension ''Detections)


$(deriveSafeCopy 0 'base ''DetectionStats0)
$(deriveSafeCopy 1 'extension ''DetectionStats1)
$(deriveSafeCopy 2 'extension ''DetectionStats2)
$(deriveSafeCopy 3 'extension ''DetectionStats3)
$(deriveSafeCopy 4 'extension ''DetectionStats)

$(deriveSafeCopy 0 'base ''Margins)

$(deriveSafeCopy 0 'base ''Checkpoint)
$(deriveSafeCopy 0 'base ''ImageCat)
$(deriveSafeCopy 0 'base ''Submission0)
$(deriveSafeCopy 1 'base ''Submission1)
$(deriveSafeCopy 2 'base ''Submission2)
$(deriveSafeCopy 3 'extension ''Submission)

$(deriveSafeCopy 0 'base ''SubmitType0)
$(deriveSafeCopy 1 'extension ''SubmitType)

$(deriveSafeCopy 1 'base ''Document1)
$(deriveSafeCopy 2 'extension ''Document2)
$(deriveSafeCopy 3 'extension ''Document3)
$(deriveSafeCopy 4 'extension ''Document4)
$(deriveSafeCopy 5 'extension ''Document5)
$(deriveSafeCopy 6 'extension ''Document6)
$(deriveSafeCopy 7 'extension ''Document7)
$(deriveSafeCopy 8 'extension ''Document8)
$(deriveSafeCopy 9 'extension ''Document9)
$(deriveSafeCopy 10 'extension ''Document10)
$(deriveSafeCopy 11 'extension ''Document11)
$(deriveSafeCopy 12 'extension ''Document12)
$(deriveSafeCopy 13 'extension ''Document13)
$(deriveSafeCopy 14 'extension ''Document14)

$(deriveSafeCopy 15 'extension ''Document)

$(deriveSafeCopy 0 'base ''NaturalKey0)
$(deriveSafeCopy 1 'extension ''NaturalKey1)
$(deriveSafeCopy 2 'extension ''NaturalKey)

$(deriveSafeCopy 3 'extension ''DocInfo3)
$(deriveSafeCopy 4 'extension ''DocInfo4)
$(deriveSafeCopy 5 'extension ''DocInfo5)
$(deriveSafeCopy 6 'extension ''DocInfo6)
$(deriveSafeCopy 7 'extension ''DocInfo)

$(deriveSafeCopy 0 'base ''ImageInfo)


$(deriveSafeCopy 0 'base ''Config)

$(deriveSafeCopy 0 'base ''ClassConfig0)
$(deriveSafeCopy 1 'extension ''ClassConfig1)
$(deriveSafeCopy 2 'extension ''ClassConfig)

$(deriveSafeCopy 0 'base ''ShapeConfig)

$(deriveSafeCopy 0 'base ''Edit0)
$(deriveSafeCopy 1 'extension ''Edit1)
$(deriveSafeCopy 2 'extension ''Edit)

$(deriveSafeCopy 0 'base ''OldHistoryEntry)
$(deriveSafeCopy 0 'base ''HistoryEntry0)
$(deriveSafeCopy 0 'base ''HistoryEntry)

$(deriveSafeCopy 0 'base ''OpenSession)
$(deriveSafeCopy 0 'base ''Session)
$(deriveSafeCopy 0 'base ''OpenType)

$(deriveSafeCopy 0 'base ''DocumentPatch)
$(deriveSafeCopy 0 'base ''AnnotationPatch)

$(deriveSafeCopy 2 'extension ''Store)
$(deriveSafeCopy 0 'base ''Command0)
$(deriveSafeCopy 1 'extension ''Command1)
$(deriveSafeCopy 2 'extension ''Command)

$(deriveSafeCopy 0 'base ''TrainerState)
$(deriveSafeCopy 0 'base ''ModelState)

$(deriveSafeCopy 0 'base ''AssignmentMethod)


$(deriveSafeCopy 0 'base ''Preferences0)
$(deriveSafeCopy 1 'extension ''Preferences1)
$(deriveSafeCopy 2 'extension ''Preferences2)
$(deriveSafeCopy 3 'extension ''Preferences3)
$(deriveSafeCopy 4 'extension ''Preferences4)
$(deriveSafeCopy 5 'extension ''Preferences5)
$(deriveSafeCopy 6 'extension ''Preferences6)
$(deriveSafeCopy 7 'extension ''Preferences)

$(deriveSafeCopy 0 'base ''DisplayPreferences0)
$(deriveSafeCopy 1 'extension ''DisplayPreferences)


$(deriveSafeCopy 0 'base ''DetectionParams)
$(deriveSafeCopy 0 'base ''ImageOrdering)

$(deriveSafeCopy 0 'base ''SortKey)
$(deriveSafeCopy 0 'base ''FilterOption)
$(deriveSafeCopy 0 'base ''SortOptions0)
$(deriveSafeCopy 1 'extension ''SortOptions1)
$(deriveSafeCopy 2 'extension ''SortOptions2)
$(deriveSafeCopy 3 'extension ''SortOptions3)
$(deriveSafeCopy 4 'extension ''SortOptions4)
$(deriveSafeCopy 5 'extension ''SortOptions)

$(deriveSafeCopy 0 'base ''ImageSelection0)
$(deriveSafeCopy 0 'base ''ImageSelection)



docInfo :: DocName -> Traversal' Store DocInfo
docInfo k = #images . ix k . #info

updateInfo :: Document -> UTCTime -> (Store -> Store)
updateInfo doc time = over (docInfo k) $ \info ->
  info & #modified .~ Just time & #numAnnotations .~ length (doc ^. #annotations)
    where k = view #name doc

updateImageInfo :: (DocName, Maybe ImageInfo) ->  (Map DocName Document -> Map DocName Document)
updateImageInfo (k, Nothing)   = M.delete k
updateImageInfo (k, Just info) = M.alter f k where
  f Nothing      = Just (emptyImage k info)
  f (Just image) = Just (image & #info . #image .~ info) 

updateDocument :: Document -> (Store -> Store)
updateDocument doc = #images . at (doc ^. #name) .~ Just doc


updateStats :: Maybe DetectionStats -> DetectionStats -> DetectionStats 
updateStats Nothing stats = stats
updateStats (Just stats) stats' = stats' 
  { counts = stats' ^. #counts <|> stats ^. #counts 
  , frameVariation = stats' ^. #frameVariation <|> stats ^. #frameVariation
  }

updateDetections :: (DocName, Detections) -> Map DocName Document -> Map DocName Document
updateDetections (k, detections) = over (ix k) $ \doc -> doc 
  & #detections         .~ Just detections
  & #info . #detections .~ Just (updateStats (doc ^. (#info . #detections)) stats)
    where stats = detections ^. #stats


interp :: Fractional a => a -> a -> a -> a
interp t x total = t * x + (1 - t) * total

trainingStats :: [TrainSummary] -> TrainStats 
trainingStats training = TrainStats 
  { lossMean = sum losses / fromIntegral (length losses)
  , lossRunning = maybe 0 (foldr1 (interp 0.3)) (nonEmpty losses)
  } where 
    losses = view #loss <$> training
    

addTraining :: (DocName, [TrainSummary]) ->  Map DocName Document -> Map DocName Document
addTraining (k, summary) = over (ix k) addSummary where
  addSummary doc = doc 
    & #training .~ training 
    & #info . #training  .~ trainingStats training
      where training = take 20 (summary <> doc ^. #training)

emptyImage :: DocName -> ImageInfo -> Document
emptyImage k info = emptyDoc k (defaultInfo k info)

emptyDoc :: DocName -> DocInfo -> Document
emptyDoc k info = Document 
  { name = k
  , info = info 
  , annotations = mempty 
  , sessions = [] 
  , detections = Nothing
  , training = []
  }

selectCategory :: UserId -> Store -> ImageCat
selectCategory user store = (case (prefs ^. #assignMethod) of 
  AssignAuto    -> assignCat (prefs ^. #trainRatio) (countSubmitted $ store ^. #images)
  AssignCat cat -> cat)
    where  prefs = lookupPreferences store user

countSubmitted = M.size . M.filter notNew
  where notNew =  (/= CatNew) . view (#info . #category)


assignCat :: Int -> Int -> ImageCat
assignCat trainRatio n = if n `mod` (trainRatio + 1) == trainRatio 
  then CatValidate
  else CatTrain


checkSubmission :: Submission -> Document -> Maybe ErrCode
checkSubmission Submission{session, annotations} doc = ErrSubmit <$> 
    (check (session ^. #initial ~= doc ^. #annotations) "Failed to match initial annotations"
    <|> check (checkReplay (session, annotations)) "Annotation replay failed consistency")
  
  where check b msg = if not b then Just msg else Nothing


checkStore :: Store -> [(DocName, ImageCat)]
checkStore Store{images} = M.toList $ view (#info . #category) <$> 
  M.filter (not . checkReplays) images


lookupPreferences :: Store -> UserId -> Preferences
lookupPreferences store user = fromMaybe def (store ^. #preferences . at user)

submitDocument :: UserId -> UTCTime -> Submission -> (Store -> Store)
submitDocument user time Submission{..} store = store & #images . ix name %~ \doc -> 
  case method of
    SubmitNew     -> doc 
        & #sessions .~ []
        & storeChanges 
        & #info . #category .~ (selectCategory user store) 
        & #info . #reviews .~ 0

    SubmitDiscard -> doc & storeChanges 
      & #info . #category .~ CatDiscard
      & #info . #reviews .~ 0

    SubmitConfirm cat -> doc & storeChanges
      & #info . #reviews %~ (+1) 
      & #info . #category %~ maybe id const cat

    SubmitAutoSave -> doc & storeChanges

  where 
    storeChanges doc = doc 
      & #annotations .~ annotations
      & #sessions %~ (`snoc` session)

      & #info %~ \info -> info 
        & #modified .~ Just time
        & #numAnnotations .~ length annotations


  
instance Persistable Store where
  type Update Store = Command

  update (CmdUpdate doc time) = updateInfo doc time . updateDocument doc
  update (CmdImages new)        = over #images (M.union new') where
    new' = M.mapWithKey emptyDoc (M.fromList new)

  update (CmdUpdateImages updates) = over #images $ 
    (flip (foldr updateImageInfo) updates)
 

  update (CmdCategory k cat)      = docInfo k . #category .~ cat
  update (CmdClass k conf)  = over (#config . #classes) (M.alter (const conf) k)
  update (CmdSetRoot path)  = #config . #root .~ path

  update (CmdCheckpoint cp) = over #trainer $ checkpoint cp

  update (CmdPreferences user preferences) = over #preferences (M.insert user preferences)
  update (CmdDetections detections) = over #images $
    (flip (foldr updateDetections)) (M.toList detections)

  update (CmdSubmit user submission time) = submitDocument user time submission 
  update (CmdTraining summaries) = over #images $ 
    (flip (foldr addTraining)) (M.toList summaries)



applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f = f
applyWhen False _ = id

checkpoint :: Checkpoint -> TrainerState -> TrainerState
checkpoint Checkpoint{..} state = if run /= state ^. #run then reset else update
    where
      update = state & #current .~ model
                     & applyWhen isBest (#best .~ model)

      reset = TrainerState model model run
      model = ModelState Nothing epoch score

      (run, epoch) = networkId

initialStore :: Config -> Store
initialStore config = Store
  { config = config
  , images = M.empty
  , trainer = def
  , preferences = mempty
  }
