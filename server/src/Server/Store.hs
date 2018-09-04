module Server.Store where


import Annotate.Prelude
import Server.Common

import qualified Data.Map as M
import Data.SafeCopy

import Control.Concurrent.Log

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

instance Migrate Document where
  type MigrateFrom Document = Document3
  migrate Document3{..} = Document {..}
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
$(deriveSafeCopy 2 'base ''Document2)
$(deriveSafeCopy 3 'base ''Document3)




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

$(deriveSafeCopy 0 'base ''V2)
$(deriveSafeCopy 0 'base ''Box)
$(deriveSafeCopy 0 'base ''Circle)
$(deriveSafeCopy 0 'base ''Polygon)
$(deriveSafeCopy 0 'base ''WideLine)

$(deriveSafeCopy 0 'base ''Extents)

$(deriveSafeCopy 3 'extension ''Annotation)
$(deriveSafeCopy 0 'base ''Shape)

$(deriveSafeCopy 0 'base ''Detection)

$(deriveSafeCopy 0 'base ''ImageCat)
$(deriveSafeCopy 4 'extension ''Document)
$(deriveSafeCopy 0 'base ''DocInfo)
$(deriveSafeCopy 0 'base ''Config)
$(deriveSafeCopy 0 'base ''ClassConfig)
$(deriveSafeCopy 0 'base ''ShapeConfig)

$(deriveSafeCopy 0 'base ''HistoryEntry)
$(deriveSafeCopy 0 'base ''Edit)
$(deriveSafeCopy 0 'base ''EditAction)


$(deriveSafeCopy 0 'base ''Store)

$(deriveSafeCopy 0 'base ''Command)


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


initialStore :: Config -> Store
initialStore config = Store
  { config = config
  , images = M.empty
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
  }

importImage :: TrainImage -> (DocName, Document)
importImage TrainImage{..} = (imageFile, document) where
  document = emptyDoc imageFile info
    & #annotations .~ M.fromList (zip [0..]  annotations)
    & #validArea   .~ validArea
  info = DocInfo {modified = Nothing, imageSize = imageSize, category = category, numAnnotations = 0}

exportImage :: Document -> TrainImage
exportImage Document{..} = TrainImage
  { imageFile = name
  , imageSize = info ^. #imageSize
  , category  = info ^. #category
  , annotations = filter (view #confirm) (M.elems annotations)
  , validArea = validArea
  }
