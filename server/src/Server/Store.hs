module Server.Store where


import Annotate.Common
import Server.Common

import qualified Data.Map as M
import Data.SafeCopy

import Annotate.Document (emptyDoc, applyCmd)
import Control.Concurrent.Log


$(deriveSafeCopy 0 'base ''V2)
$(deriveSafeCopy 0 'base ''Box)
$(deriveSafeCopy 0 'base ''Circle)
$(deriveSafeCopy 0 'base ''Polygon)
$(deriveSafeCopy 0 'base ''WideLine)

$(deriveSafeCopy 0 'base ''Extents)

$(deriveSafeCopy 0 'base ''Annotation)
$(deriveSafeCopy 0 'base ''Shape)

$(deriveSafeCopy 0 'base ''Edit)
$(deriveSafeCopy 0 'base ''DocCmd)
$(deriveSafeCopy 0 'base ''ImageCat)
$(deriveSafeCopy 0 'base ''Document)
$(deriveSafeCopy 0 'base ''DocInfo)
$(deriveSafeCopy 0 'base ''Config)
$(deriveSafeCopy 0 'base ''ClassConfig)
$(deriveSafeCopy 0 'base ''ShapeConfig)

$(deriveSafeCopy 0 'base ''Store)

$(deriveSafeCopy 0 'base ''Command)


docInfo :: DocName -> Traversal' Store DocInfo
docInfo k = #images . ix k . #info

updateModified :: DocName -> UTCTime -> (Store -> Store)
updateModified k time = docInfo k . #modified .~ Just time

updateDocument :: Document -> (Store -> Store)
updateDocument doc = #images . at (doc ^. #name) .~ Just doc

instance Persistable Store where
  type Update Store = Command

  update (CmdSubmit doc time) = updateModified (doc ^. #name) time . updateDocument doc    
  update (CmdImages new)        = over #images (M.union new') 
    where new' = M.mapWithKey emptyDoc (M.fromList new)
    
  
  update (CmdCategory k cat)    = docInfo k . #category .~ cat


initialStore :: Config -> Store
initialStore config = Store
  { config = config
  , images = M.empty
  }




exportCollection :: Store -> TrainCollection
exportCollection Store{..} = TrainCollection
  { config = config
  , images = M.elems (trainerImage <$> images)
  } where
    trainerImage Document{..} = TrainImage
      { imageFile = name
      , imageSize = info ^. #imageSize
      , category  = info ^. #category
      , annotations = M.elems annotations
      }

importCollection :: TrainCollection -> Store
importCollection TrainCollection{..} = Store
  { config = config
  , images = M.fromList (importImage <$> images)
  }     
    
importImage :: TrainImage -> (DocName, Document)
importImage TrainImage{..} = (imageFile, document) where
  document = emptyDoc imageFile info
    & #annotations .~ M.fromList (zip [0..] annotations)
  info = DocInfo {modified = Nothing, imageSize = imageSize, category = category}

    

