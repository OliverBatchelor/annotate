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

$(deriveSafeCopy 0 'base ''Extents)

$(deriveSafeCopy 0 'base ''Object)
$(deriveSafeCopy 0 'base ''Shape)

$(deriveSafeCopy 0 'base ''Edit)
$(deriveSafeCopy 0 'base ''DocCmd)
$(deriveSafeCopy 0 'base ''ImageCat)
$(deriveSafeCopy 0 'base ''Document)
$(deriveSafeCopy 0 'base ''DocInfo)
$(deriveSafeCopy 0 'base ''Config)
$(deriveSafeCopy 0 'base ''Store)

$(deriveSafeCopy 0 'base ''Command)


docInfo :: DocName -> Traversal' Store DocInfo
docInfo k = #images . at k . traverse


updateModified k time = docInfo k . #modified .~ Just time
applyDocCmd k cmd = over (#documents . at k) $ \maybeDoc ->
    Just $ applyCmd cmd (fromMaybe emptyDoc maybeDoc)

updateDoc k doc = #documents . at k .~ Just doc

instance Persistable Store where
  type Update Store = Command

  update (CmdDoc k cmd time) = updateModified k time . applyDocCmd k cmd
  update (CmdSubmit k doc time) = updateModified k time . updateDoc k doc

  update (CmdImages new) = over #images (M.union (M.fromList new))
  update (CmdCategory k cat) = docInfo k . #category .~ cat



initialStore :: Config -> Store
initialStore config = Store
  { config = config
  , images = M.empty
  , documents = M.empty
  }




toExport :: Store -> Export
toExport Store{..} = Export
  { config = config
  , images = M.elems $ M.intersectionWithKey trainerImage images documents
  } where
    trainerImage k info doc = ImageE
      { imageFile = k
      , imageSize = info ^. #imageSize
      , category  = info ^. #category
      , instances = M.elems $ doc ^. #instances
      }

fromExport :: Export -> Store
fromExport Export {..} = Store
  { config = config
  , images = M.fromList (toInfo <$> images)
  , documents = M.fromList (toDoc <$> images)
  } where
    toDoc  ImageE{..} = (imageFile, emptyDoc & #instances .~ M.fromList (zip [0..] instances))
    toInfo ImageE{..} = (imageFile, DocInfo {modified = Nothing, imageSize = imageSize, category = category})
