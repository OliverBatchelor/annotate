module Server.Store where


import Annotate.Prelude
import Server.Common

import qualified Data.Map as M
import Data.SafeCopy

import Control.Concurrent.Log
import qualified Data.Text as Text

data OldHistoryEntry = HistOpen | HistSubmit | HistEdit DocumentPatch | HistUndo | HistRedo
  deriving (Show, Eq, Generic)


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


data Detections0 = Detections0
  { detections :: [Detection]
  , networkId :: NetworkId
  } deriving (Show,  Generic)


data Document11 = Document11
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: Map AnnotationId BasicAnnotation
  , validArea   :: Maybe Box
  , history :: [(UTCTime, HistoryEntry)]
  , detections :: Maybe Detections
  } deriving (Generic, Show)  

data Document10 = Document10
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: Map AnnotationId BasicAnnotation
  , validArea   :: Maybe Box

  , history :: [(UTCTime, HistoryEntry)]
  , detections :: Maybe ([Detection], NetworkId)
  } deriving (Generic, Show)  

data Document9 = Document9
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, HistoryEntry)]
  , detections :: Maybe ([Detection], NetworkId)

  } deriving (Generic, Show)

data Document8 = Document8
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, HistoryEntry)]
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

instance Migrate Command where
  type MigrateFrom Command = Command0
  migrate (CmdCategory0 k c) = CmdCategory k c
  migrate (CmdUpdate0 k c) = CmdUpdate k c
  migrate (CmdModified0 k c) = CmdModified k c
  migrate (CmdImages0 m) = CmdImages m
  migrate (CmdSetRoot0 r) = CmdSetRoot r
  migrate (CmdCheckpoint0 k f b) = CmdCheckpoint $ Checkpoint k f b
  migrate (CmdPreferences0 k p) = CmdPreferences k p
  migrate (CmdDetections0 ds netId)   = CmdDetections $ f <$> M.fromList ds
    where f dets = Detections dets netId def

data ImageOrdering = OrderSequential | OrderMixed | OrderBackwards
  deriving (Show, Eq, Ord, Enum, Generic)


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


instance Migrate Preferences1 where
  type MigrateFrom Preferences1 = Preferences0
  migrate Preferences0{..} = Preferences1{..}
    where 
      margin = 0.1
      border = 1
   
instance Migrate Preferences where
  type MigrateFrom Preferences = Preferences1
  migrate Preferences1{..} = Preferences{..} where 
    sortOptions = def
    autoDetect = True


instance Migrate Document where
  type MigrateFrom Document = Document11
  migrate Document11{..} = Document{..} where
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
    
instance Migrate DocInfo where
  type MigrateFrom DocInfo = DocInfo3
  migrate DocInfo3{..} = DocInfo{..} where 
    detections = def
    lossMax       = 0
    lossMean  = 0    


instance Migrate Detections where
  type MigrateFrom Detections = Detections0
  migrate Detections0{..} = Detections{..} where 
    stats = def 


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


instance Migrate Detection where
  type MigrateFrom Detection = Detection0
  migrate Detection0{..} = Detection{..}
    where shape = ShapeBox bounds

$(deriveSafeCopy 0 'base ''Detection0)


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


instance Migrate NaturalKey where
  type MigrateFrom NaturalKey = NaturalKey0
  migrate (NaturalKey0 ks) = NaturalKey (migrate' <$> ks) where
    migrate' (Left i)  = Left (i, Text.pack (show i))
    migrate' (Right t) = Right t


$(deriveSafeCopy 0 'base ''ShapeTag)
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
$(deriveSafeCopy 1 'extension ''Detection)

$(deriveSafeCopy 0 'base ''Detections0)
$(deriveSafeCopy 1 'extension ''Detections)

$(deriveSafeCopy 0 'base ''DetectionStats)

$(deriveSafeCopy 0 'base ''Checkpoint)

$(deriveSafeCopy 0 'base ''ImageCat)
$(deriveSafeCopy 0 'base ''Submission)

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
$(deriveSafeCopy 12 'extension ''Document)


$(deriveSafeCopy 0 'base ''NaturalKey0)
$(deriveSafeCopy 1 'extension ''NaturalKey)


$(deriveSafeCopy 3 'extension ''DocInfo3)
$(deriveSafeCopy 4 'extension ''DocInfo)



$(deriveSafeCopy 0 'base ''Config)
$(deriveSafeCopy 0 'base ''ClassConfig)
$(deriveSafeCopy 0 'base ''ShapeConfig)

$(deriveSafeCopy 0 'base ''Edit)

$(deriveSafeCopy 0 'base ''OldHistoryEntry)

$(deriveSafeCopy 0 'base ''HistoryEntry)
$(deriveSafeCopy 0 'base ''DocumentPatch)
$(deriveSafeCopy 0 'base ''AnnotationPatch)

$(deriveSafeCopy 2 'extension ''Store)
$(deriveSafeCopy 0 'base ''Command0)
$(deriveSafeCopy 1 'extension ''Command)

$(deriveSafeCopy 0 'base ''TrainerState)
$(deriveSafeCopy 0 'base ''ModelState)

$(deriveSafeCopy 0 'base ''Preferences0)
$(deriveSafeCopy 1 'extension ''Preferences1)
$(deriveSafeCopy 2 'extension ''Preferences)

$(deriveSafeCopy 0 'base ''DetectionParams)
$(deriveSafeCopy 0 'base ''ImageOrdering)

$(deriveSafeCopy 0 'base ''SortKey)
$(deriveSafeCopy 0 'base ''FilterOption)
$(deriveSafeCopy 0 'base ''SortOptions)


docInfo :: DocName -> Traversal' Store DocInfo
docInfo k = #images . ix k . #info

updateInfo :: Document -> UTCTime -> (Store -> Store)
updateInfo doc time = over (docInfo k) $ \info ->
  info & #modified .~ Just time & #numAnnotations .~ length (doc ^. #annotations)
    where k = view #name doc

updateDocument :: Document -> (Store -> Store)
updateDocument doc = #images . at (doc ^. #name) .~ Just doc

updateDetections :: (DocName, Detections) -> Map DocName Document -> Map DocName Document
updateDetections (k, detections) = over (ix k) $ \doc -> doc 
  & #detections         .~ Just detections
  & #info . #detections .~ Just (detections ^. #stats)

emptyDoc :: DocName -> DocInfo -> Document
emptyDoc k info = Document 
  { name = k
  , info = info 
  , annotations = mempty 
  , validArea = Nothing 
  , history = [] 
  , detections = Nothing
  , trainingLoss = []
  }


submitDocument :: UTCTime -> Submission -> (Store -> Store)
submitDocument time Submission{..} = over (#images . ix name) $ \doc -> doc 
  & #annotations .~ annotations
  & #validArea .~ validArea
  & #history %~ (history <>)

  & #info %~ \info -> info 
    & #category %~ maybe id const category
    & #modified .~ Just time
    & #numAnnotations .~ length annotations


instance Persistable Store where
  type Update Store = Command

  update (CmdUpdate doc time) = updateInfo doc time . updateDocument doc
  update (CmdImages new)        = over #images (M.union new') where
    new' = M.mapWithKey emptyDoc (M.fromList new)


  update (CmdCategory k cat)      = docInfo k . #category .~ cat
  update (CmdClass k conf)  = over (#config . #classes) (M.alter (const conf) k)
  update (CmdSetRoot path)  = #config . #root .~ path

  update (CmdCheckpoint cp) = over #trainer $ checkpoint cp

  update (CmdPreferences user preferences) = over #preferences (M.insert user preferences)
  update (CmdDetections detections) = over #images $
    foldr (.) id (updateDetections <$> M.toList detections)

  update (CmdSubmit submission time) = submitDocument time submission 


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


exportCollection :: Store -> TrainCollection
exportCollection Store{..} = TrainCollection
  { config = config
  , images = M.elems (exportImage <$> images)
  }


importCollection :: TrainCollection -> Store
importCollection TrainCollection{..} = Store
  { config = config
  , images = M.fromList (importImage <$> images)
  , trainer = def
  , preferences = def
  }

importImage :: TrainImage -> (DocName, Document)
importImage TrainImage{..} = (imageFile, document) where
  document = emptyDoc imageFile info
    & #annotations .~ M.fromList (zip [0..] annotations)
    & #validArea   .~ validArea
  info :: DocInfo = (defaultInfo imageSize imageFile)
    {modified = Nothing, category = category, numAnnotations = length annotations}

    
exportImage :: Document -> TrainImage
exportImage Document{..} = TrainImage
  { imageFile = name
  , imageSize = info ^. #imageSize
  , category  = info ^. #category
  , annotations = M.elems annotations
  , validArea = validArea
  , history = history
  , evaluated = view #networkId <$> detections
  }
