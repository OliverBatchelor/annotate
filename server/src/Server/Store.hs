module Server.Store where


import Annotate.Prelude
import Server.Common

import qualified Data.Map as M
import Data.SafeCopy

import Control.Concurrent.Log

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


data Document5 = Document5
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, HistoryEntry)]
  } deriving (Generic, Show, Eq)


data Document4 = Document4
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea   :: Maybe Box
  , history :: [(UTCTime, HistoryEntry)]
  } deriving (Generic, Show, Eq)


data Document3 = Document3
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea   :: Maybe Box
  } deriving (Generic, Show, Eq)

data Document2 = Document2
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  } deriving (Generic, Show, Eq)

data Document1 = Document1
  { name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  } deriving (Generic, Show, Eq)


data Store0 = Store0
  { config    :: Config
  , images :: Map DocName Document
  } deriving (Show, Eq, Generic)

data Store1 = Store1
  { config    :: Config
  , images :: Map DocName Document
  , trainer :: TrainerState
  } deriving (Show, Eq, Generic)

instance Migrate Store1 where
  type MigrateFrom Store1 = Store0
  migrate Store0{..} = Store1{..}
    where trainer = def

instance Migrate Store where
  type MigrateFrom Store = Store1
  migrate Store1{..} = Store{..}
    where preferences = mempty

instance Migrate Document where
  type MigrateFrom Document = Document5
  migrate Document5{..} = Document {name, history, annotations, validArea, info = migrateInfo info} where
    migrateInfo info = info {naturalKey, hashedName} :: DocInfo
    naturalKey = makeNaturalKey name
    hashedName = Hash32 (fromIntegral (hash name))

instance Migrate Document5 where
  type MigrateFrom Document5 = Document4
  migrate Document4{..} = Document5{..}

instance Migrate DocInfo2 where
  type MigrateFrom DocInfo2 = DocInfo1
  migrate DocInfo1{..} = DocInfo2
    { naturalKey, modified, numAnnotations
    , category, imageSize, hashedName = fromIntegral hashedName}

instance Migrate DocInfo where
  type MigrateFrom DocInfo = DocInfo2
  migrate DocInfo2{..} = DocInfo
    { naturalKey, modified, numAnnotations
    , category, imageSize, hashedName = Hash32 (fromIntegral hashedName)}

instance Migrate DocInfo1 where
  type MigrateFrom DocInfo1 = DocInfo0
  migrate DocInfo0{..} = DocInfo1 {..}
    where naturalKey = NaturalKey []
          hashedName = 0

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
  migrate Document1{..} = Document2
    { name = name
    , info = info & #numAnnotations .~ M.size annotations'
    , annotations = annotations'
    } where
        annotations' = M.filter (view #confirm) annotations

$(deriveSafeCopy 1 'base ''Document1)
$(deriveSafeCopy 2 'extension ''Document2)
$(deriveSafeCopy 3 'extension ''Document3)
$(deriveSafeCopy 4 'extension ''Document4)
$(deriveSafeCopy 5 'extension ''Document5)


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
    where shape = BoxShape bounds

$(deriveSafeCopy 0 'base ''Detection0)


data Annotation1 = Annotation1 { shape :: Shape, label :: ClassId }
    deriving (Generic, Show, Eq)

data Annotation2 = Annotation2
  { shape :: Shape, label :: ClassId, detection :: Maybe Detection}
    deriving (Generic, Show, Eq)

instance Migrate Annotation2 where
  type MigrateFrom Annotation2 = Annotation1
  migrate Annotation1{..} = Annotation2{shape, label, detection = Nothing}

instance Migrate Annotation where
  type MigrateFrom Annotation = Annotation2
  migrate Annotation2{..} = Annotation{shape, label, detection, confirm = True}

$(deriveSafeCopy 1 'base ''Annotation1)
$(deriveSafeCopy 2 'extension ''Annotation2)

$(deriveSafeCopy 0 'base ''Hash32)


$(deriveSafeCopy 0 'base ''V2)
$(deriveSafeCopy 0 'base ''Box)
$(deriveSafeCopy 0 'base ''Circle)
$(deriveSafeCopy 0 'base ''Polygon)
$(deriveSafeCopy 0 'base ''WideLine)

$(deriveSafeCopy 0 'base ''Extents)

$(deriveSafeCopy 3 'extension ''Annotation)
$(deriveSafeCopy 0 'base ''Shape)

$(deriveSafeCopy 1 'extension ''Detection)

$(deriveSafeCopy 0 'base ''ImageCat)
$(deriveSafeCopy 6 'extension ''Document)

$(deriveSafeCopy 0 'base ''NaturalKey)
$(deriveSafeCopy 3 'extension ''DocInfo)
$(deriveSafeCopy 0 'base ''Config)
$(deriveSafeCopy 0 'base ''ClassConfig)
$(deriveSafeCopy 0 'base ''ShapeConfig)

$(deriveSafeCopy 0 'base ''HistoryEntry)
$(deriveSafeCopy 0 'base ''Edit)
$(deriveSafeCopy 0 'base ''EditAction)

$(deriveSafeCopy 2 'extension ''Store)
$(deriveSafeCopy 0 'base ''Command)

$(deriveSafeCopy 0 'base ''TrainerState)
$(deriveSafeCopy 0 'base ''ModelState)
$(deriveSafeCopy 0 'base ''Preferences)
$(deriveSafeCopy 0 'base ''DetectionParams)
$(deriveSafeCopy 0 'base ''ImageOrdering)



docInfo :: DocName -> Traversal' Store DocInfo
docInfo k = #images . ix k . #info

updateInfo :: Document -> UTCTime -> (Store -> Store)
updateInfo doc time = over (docInfo k) $ \info ->
  info & #modified .~ Just time & #numAnnotations .~ length (doc ^. #annotations)
    where k = view #name doc


updateDocument :: Document -> (Store -> Store)
updateDocument doc = #images . at (doc ^. #name) .~ Just doc

emptyDoc :: DocName -> DocInfo -> Document
emptyDoc k info = Document k info mempty Nothing []


instance Persistable Store where
  type Update Store = Command

  update (CmdSubmit doc time) = updateInfo doc time . updateDocument doc
  update (CmdImages new)        = over #images (M.union new') where
    new' = M.mapWithKey emptyDoc (M.fromList new)


  update (CmdCategory k cat)      = docInfo k . #category .~ cat
  update (CmdClass k conf)  = over (#config . #classes) (M.alter (const conf) k)
  update (CmdSetRoot path)  = #config . #root .~ path

  update (CmdCheckpoint netId score best) = over #trainer $
    checkpoint netId score best

  update (CmdPreferences user preferences) = over #preferences (M.insert user preferences)

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f = f
applyWhen False _ = id

checkpoint :: (RunId, Epoch) -> Float -> Bool -> TrainerState -> TrainerState
checkpoint (run, epoch) score isBest state = if run /= state ^. #run then reset else update
    where
      update = state & #current .~ model
                     & applyWhen isBest (#best .~ model)

      reset = TrainerState model model run
      model = ModelState Nothing epoch score


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
    & #annotations .~ M.fromList (zip [0..]  annotations)
    & #validArea   .~ validArea
  info :: DocInfo = (defaultInfo imageSize imageFile)
    {modified = Nothing, category = category, numAnnotations = length annotations}

exportImage :: Document -> TrainImage
exportImage Document{..} = TrainImage
  { imageFile = name
  , imageSize = info ^. #imageSize
  , category  = info ^. #category
  , annotations = filter (view #confirm) (M.elems annotations)
  , validArea = validArea
  }
