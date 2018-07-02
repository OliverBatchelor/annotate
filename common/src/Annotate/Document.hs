module Annotate.Document where

import Annotate.Common

import qualified Data.Map as M
import qualified Data.Set as S

import Annotate.Types

import Data.List (uncons)
import Data.Maybe (catMaybes)

emptyDoc ::  Document
emptyDoc = Document
  { undos = []
  , redos = []
  , instances = M.empty
  }


editTargets :: Edit -> [ObjId]
editTargets  (Add objs)  = fst <$> objs
editTargets  (Delete ids) = ids
editTargets  (Transform ids _ _) = ids
-- editTargets  (Many edits) = concatMap editTargets edits

allObjects :: Document -> [ObjId]
allObjects Document{instances} = M.keys instances

lookupObjects :: [ObjId] -> Document -> ObjectMap
lookupObjects objs Document{instances} = M.fromList $ catMaybes $ fmap lookup' objs
    where lookup' k = (k, ) <$> M.lookup k instances


maxEdits :: [Edit] -> Maybe ObjId
maxEdits = maximumId . catMaybes . fmap maxEdit


maxEdit :: Edit -> Maybe ObjId
maxEdit (Add objs)  = maximumId (fst <$> objs)
maxEdit (Delete ids) = maximumId ids
maxEdit (Transform ids _ _) = maximumId ids
-- maxEdit (Many edits) = maxEdits edits

maximumId :: [ObjId] -> Maybe ObjId
maximumId [] = Nothing
maximumId xs = Just $ maximum xs


maxId :: Document -> Maybe ObjId
maxId Document{..} = maximumId $ catMaybes
  [ maxEdits undos
  , maxEdits redos
  , maximumId (M.keys instances)
  ]

lookupTargets :: Document -> [ObjId] -> Map ObjId (Maybe Object)
lookupTargets Document{instances} targets = M.fromList modified where
  modified = lookup' instances <$> targets
  lookup' m k = (k, M.lookup k m)

applyCmd :: DocCmd -> Document -> Document
applyCmd cmd doc = fromMaybe doc (snd <$> applyCmd' cmd doc)

applyCmdPatch :: DocCmd -> Document -> Maybe (Document, Map ObjId (Maybe Object))
applyCmdPatch cmd doc = do
  ((e, _), doc') <- applyCmd' cmd doc
  return (doc', lookupTargets doc' (editTargets e))

editPatch :: ((Edit, Edit), Document) -> Map ObjId (Maybe Object)
editPatch ((e, _), doc') = lookupTargets doc' (editTargets e)

applyCmd' :: DocCmd -> Document -> Maybe ((Edit, Edit), Document)
applyCmd' DocUndo doc = applyUndo doc
applyCmd' DocRedo doc = applyRedo doc
applyCmd' (DocEdit e) doc = applyEdit e doc

applyEdit :: Edit -> Document -> Maybe ((Edit, Edit), Document)
applyEdit e doc = do
  (inverse, doc') <- applyEdit' e doc
  return ((e, inverse), doc'
    & #redos .~ mempty & #undos %~ (inverse :))

applyUndo :: Document -> Maybe ((Edit, Edit), Document)
applyUndo doc = do
  (e, undos) <- uncons (doc ^. #undos)
  (inverse, doc') <- applyEdit' e doc
  return ((e, inverse), doc'
    & #undos .~ undos & #redos %~ (inverse :))

applyRedo :: Document -> Maybe ((Edit, Edit), Document)
applyRedo doc = do
  (e, redos) <- uncons (doc ^. #redos)
  (inverse, doc') <- applyEdit' e doc
  return ((e, inverse), doc'
    & #redos .~ redos & #undos %~ (inverse :))


applyEdit' :: Edit -> Document -> Maybe (Edit, Document)
applyEdit' e doc = do
   (inverse, objectMap) <- patchEdit e (doc ^. #instances)
   return (inverse, doc & #instances .~ objectMap)


accumEdits :: Edit -> ([Edit], ObjectMap) -> Maybe ([Edit], ObjectMap)
accumEdits edit (inverses, objectMap) = do
    (inv, objectMap') <- patchEdit edit objectMap
    return (inv : inverses, objectMap')


transformObj :: Float -> Vec -> Object -> Object
transformObj s t = over #shape $ \case
  CircleShape (Circle p r) -> CircleShape (Circle (p + t) (r * s))
  BoxShape b      -> BoxShape $ b & boxExtents %~
    (\Extents{..} -> Extents (centre + t) (extents ^* s))

patchEdit :: Edit -> ObjectMap -> Maybe (Edit, ObjectMap)
patchEdit edit objectMap =  case edit of
  Add objs -> return (Delete (fst <$> objs), foldr (uncurry M.insert) objectMap objs)
  Delete ks     -> do
    objs <- forM ks (\k -> (k,) <$> M.lookup k objectMap)
    return (Add objs, foldr M.delete objectMap ks)

  Transform ks s v -> return
    ( Transform ks (1/s) (negate v)
    , foldr (\k -> over (at k . traverse) (transformObj s v)) objectMap ks)

  -- Many edits ->  do
  --   (edits, objectMap') <- foldM (flip accumEdits) ([], objectMap) edits
  --   return (Many edits, objectMap')
