module AppState where


import Annotate.Common

import qualified Data.Map as M

import Annotate.Types
import Data.Generics.Product.Subtype

import Control.Concurrent.Log
import Data.SafeCopy

import Annotate.Document (emptyDoc, applyCmd)

import Types

$(deriveSafeCopy 0 'base ''V2)
$(deriveSafeCopy 0 'base ''Box)
$(deriveSafeCopy 0 'base ''Circle)

$(deriveSafeCopy 0 'base ''Extents)

$(deriveSafeCopy 0 'base ''Object)
$(deriveSafeCopy 0 'base ''Shape)

$(deriveSafeCopy 0 'base ''Edit)
$(deriveSafeCopy 0 'base ''DocCmd)
$(deriveSafeCopy 0 'base ''ImageCat)
$(deriveSafeCopy 0 'base ''Document)
$(deriveSafeCopy 0 'base ''DocInfo)
$(deriveSafeCopy 0 'base ''Config)
$(deriveSafeCopy 0 'base ''AppState)

$(deriveSafeCopy 0 'base ''Command)




docInfo :: DocName -> Traversal' AppState DocInfo
docInfo k = #images . at k . traverse


updateModified k time = docInfo k . #modified .~ Just time
applyDocCmd k cmd = over (#documents . at k) $ \maybeDoc ->
    Just $ applyCmd cmd (fromMaybe emptyDoc maybeDoc)

updateDoc k doc = #documents . at k .~ Just doc

instance Persistable AppState where
  type Update AppState = Command

  update (CmdDoc k cmd time) = updateModified k time . applyDocCmd k cmd
  update (CmdSubmit k doc time) = updateModified k time . updateDoc k doc

  update (CmdImages new) = over #images (M.union (M.fromList new))
  update (CmdCategory k cat) = docInfo k . #category .~ cat



initialState :: Config -> AppState
initialState config = AppState
  { config = config
  , images = M.empty
  , documents = M.empty
  }


lookupDoc :: DocName -> AppState -> (Maybe DocInfo, Maybe Document)
lookupDoc k AppState{..} = (M.lookup k images, M.lookup k documents)

getCollection :: AppState -> Collection
getCollection = upcast



toExport :: AppState -> Export
toExport AppState{..} = Export
  { config = config
  , images = M.elems $ M.intersectionWithKey exportImage images documents
  } where
    exportImage k info doc = ExportImage
      { imageFile = k
      , imageSize = info ^. #imageSize
      , category  = info ^. #category
      , instances = M.elems $ doc ^. #instances
      }

fromExport :: Export -> AppState
fromExport Export {..} = AppState
  { config = config
  , images = M.fromList (toInfo <$> images)
  , documents = M.fromList (toDoc <$> images)
  } where
    toDoc  ExportImage{..} = (imageFile, emptyDoc & #instances .~ M.fromList (zip [0..] instances))
    toInfo ExportImage{..} = (imageFile, DocInfo {modified = Nothing, imageSize = imageSize, category = category})
