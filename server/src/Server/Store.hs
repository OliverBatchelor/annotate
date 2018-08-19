module Server.Store where


import Annotate.Prelude
import Server.Common

import qualified Data.Map as M
import Data.SafeCopy

import Control.Concurrent.Log
import Annotate.Editor

-- data Annotation0 = Annotation0 { shape :: Shape, label :: ClassId, predictions :: [(ClassId, Float)] }
--     deriving (Generic, Show, Eq)
--
-- instance Migrate Annotation where
--   type MigrateFrom Annotation = Annotation0
--   migrate Annotation0{..} = Annotation{..}
--
--
-- $(deriveSafeCopy 0 'base ''Annotation0)



$(deriveSafeCopy 0 'base ''V2)
$(deriveSafeCopy 0 'base ''Box)
$(deriveSafeCopy 0 'base ''Circle)
$(deriveSafeCopy 0 'base ''Polygon)
$(deriveSafeCopy 0 'base ''WideLine)

$(deriveSafeCopy 0 'base ''Extents)

$(deriveSafeCopy 1 'base ''Annotation)
$(deriveSafeCopy 0 'base ''Shape)


$(deriveSafeCopy 0 'base ''ImageCat)
$(deriveSafeCopy 1 'base ''Document)
$(deriveSafeCopy 0 'base ''DocInfo)
$(deriveSafeCopy 0 'base ''Config)
$(deriveSafeCopy 0 'base ''ClassConfig)
$(deriveSafeCopy 0 'base ''ShapeConfig)

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
emptyDoc k info = Document k info mempty


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
  info = DocInfo {modified = Nothing, imageSize = imageSize, category = category, numAnnotations = 0}

exportImage :: Document -> TrainImage
exportImage Document{..} = TrainImage
  { imageFile = name
  , imageSize = info ^. #imageSize
  , category  = info ^. #category
  , annotations = M.elems annotations
  }
