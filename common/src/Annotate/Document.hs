module Annotate.Document where

import Annotate.Prelude

import qualified Data.Map as M
import qualified Data.Set as S

import Annotate.Common

import Data.List (uncons)
import Data.Maybe (catMaybes)

emptyDoc ::  DocName -> DocInfo -> Document
emptyDoc name info = Document
  { undos = []
  , redos = []
  , name = name
  , info = info
  , annotations = M.empty
  }


editTargets :: Edit -> [AnnotationId]
editTargets  (Add objs)  = fst <$> objs
editTargets  (Delete ids) = ids
editTargets  (Transform ids _ _) = ids
-- editTargets  (Many edits) = concatMap editTargets edits

allAnnotations :: Document -> [AnnotationId]
allAnnotations Document{annotations} = M.keys annotations

lookupAnnotations :: [AnnotationId] -> Document -> AnnotationMap
lookupAnnotations objs Document{annotations} = M.fromList $ catMaybes $ fmap lookup' objs
    where lookup' k = (k, ) <$> M.lookup k annotations


maxEdits :: [Edit] -> Maybe AnnotationId
maxEdits = maximumId . catMaybes . fmap maxEdit


maxEdit :: Edit -> Maybe AnnotationId
maxEdit (Add objs)  = maximumId (fst <$> objs)
maxEdit (Delete ids) = maximumId ids
maxEdit (Transform ids _ _) = maximumId ids
-- maxEdit (Many edits) = maxEdits edits

maximumId :: [AnnotationId] -> Maybe AnnotationId
maximumId [] = Nothing
maximumId xs = Just $ maximum xs


maxId :: Document -> Maybe AnnotationId
maxId Document{..} = maximumId $ catMaybes
  [ maxEdits undos
  , maxEdits redos
  , maximumId (M.keys annotations)
  ]

lookupTargets :: Document -> [AnnotationId] -> Map AnnotationId (Maybe Annotation)
lookupTargets Document{annotations} targets = M.fromList modified where
  modified = lookup' annotations <$> targets
  lookup' m k = (k, M.lookup k m)

applyCmd :: DocCmd -> Document -> Document
applyCmd cmd doc = fromMaybe doc (snd <$> applyCmd' cmd doc)

applyCmdPatch :: DocCmd -> Document -> Maybe (Document, Map AnnotationId (Maybe Annotation))
applyCmdPatch cmd doc = do
  ((e, _), doc') <- applyCmd' cmd doc
  return (doc', lookupTargets doc' (editTargets e))

editPatch :: ((Edit, Edit), Document) -> Map AnnotationId (Maybe Annotation)
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
   (inverse, annotationMap) <- patchEdit e (doc ^. #annotations)
   return (inverse, doc & #annotations .~ annotationMap)


-- accumEdits :: Edit -> ([Edit], AnnotationMap) -> Maybe ([Edit], AnnotationMap)
-- accumEdits edit (inverses, annotationMap) = do
--     (inv, annotationMap') <- patchEdit edit annotationMap
--     return (inv : inverses, annotationMap')


transformObj :: Float -> Vec -> Annotation -> Annotation
transformObj s t = over #shape $ \case
  BoxShape b      -> BoxShape $ b & boxExtents %~
    (\Extents{..} -> Extents (centre + t) (extents ^* s))
  
  PolygonShape poly -> PolygonShape $ poly & over #points (fmap f)
    where f = scalePoint (boxCentre $ getBounds poly) s . (+ t)
    
  LineShape line -> LineShape $ line & over #points (fmap f)
    where 
      f (Circle p r) = Circle (scale p + t) (r * s)
      scale = scalePoint (boxCentre $ getBounds line) s
      
scalePoint :: Vec -> Float -> (Vec -> Vec)
scalePoint c s = (+ c) . (^* s) . subtract c
  

patchEdit :: Edit -> AnnotationMap -> Maybe (Edit, AnnotationMap)
patchEdit edit annotationMap =  case edit of
  Add objs -> return (Delete (fst <$> objs), foldr (uncurry M.insert) annotationMap objs)
  Delete ks     -> do
    objs <- forM ks (\k -> (k,) <$> M.lookup k annotationMap)
    return (Add objs, foldr M.delete annotationMap ks)

  Transform ks s v -> return
    ( Transform ks (1/s) (negate v)
    , foldr (\k -> over (at k . traverse) (transformObj s v)) annotationMap ks)

  -- Many edits ->  do
  --   (edits, annotationMap') <- foldM (flip accumEdits) ([], annotationMap) edits
  --   return (Many edits, annotationMap')
