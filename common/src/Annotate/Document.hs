module Annotate.Document where

import Annotate.Prelude

import qualified Data.Map as M
import qualified Data.Set as S

import Annotate.Common

import Data.List (uncons)
import Data.Maybe (catMaybes)

import Control.Lens hiding (uncons, without)
import Data.List.NonEmpty (nonEmpty)

import Data.Align
import Data.These

import Control.Lens (makePrisms)

import Debug.Trace


type DocPart = (AnnotationId, Maybe Int)

mergeParts :: DocParts -> DocParts -> DocParts
mergeParts = M.unionWith mappend


-- DocumentPatch' is a non invertible version of DocumentPatch (less details)
-- used for patching the AnnotationMap to update the view.
data DocumentPatch'
  = PatchAnns' (Map AnnotationId (Maybe Annotation))
  | PatchArea' (Maybe Box)
     deriving (Show, Eq, Generic)


data EditorDocument = EditorDocument
  { undos :: [DocumentPatch]
  , redos :: [DocumentPatch]
  , name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  , validArea :: Maybe Box
  , history :: [(UTCTime, HistoryEntry)]
  } deriving (Generic, Show, Eq)

makePrisms ''DocumentPatch'
makePrisms ''EditCmd
makePrisms ''DocumentPatch

isModified :: EditorDocument -> Bool
isModified = not . null . view #undos

fromDetections :: AnnotationId -> [Detection] -> AnnotationMap
fromDetections i detections = M.fromList (zip [i..] $ toAnnotation <$> detections) where
  toAnnotation detection@Detection{..} = Annotation
    {shape, label, detection = Just (Detected, detection) }


editorDocument :: Document -> EditorDocument
editorDocument Document{..} = EditorDocument
    {name, info, annotations = fromBasic <$> annotations
    , validArea, history
    , undos = [], redos = []
    } 
    
toDocument :: Float -> EditorDocument ->  Document
toDocument t EditorDocument{..} = Document
  { name, info, validArea
  , history, detections = Nothing
  , annotations = toBasic <$> M.filter (thresholdDetection t) annotations
  } 
        

thresholdDetection :: Float -> Annotation -> Bool
thresholdDetection t Annotation{detection} = case detection of 
  Just (tag, Detection{confidence}) -> 
          tag == Detected && confidence >= t 
          || tag == Confirmed
          || tag == Positive

  Nothing -> True

  

allAnnotations :: EditorDocument -> [AnnotationId]
allAnnotations EditorDocument{annotations} = M.keys annotations

lookupAnnotations :: [AnnotationId] -> EditorDocument -> AnnotationMap
lookupAnnotations objs EditorDocument{annotations} = M.fromList $ catMaybes $ fmap lookup' objs
    where lookup' k = (k, ) <$> M.lookup k annotations


maxEdits :: [DocumentPatch] -> Maybe AnnotationId
maxEdits =  maybeMaximum . fmap maxEdit

maxEdit :: DocumentPatch -> Maybe AnnotationId
maxEdit (PatchAnns e) = maxKey e
maxEdit _        = Nothing


maybeMaximum :: (Ord k) => [Maybe k] -> Maybe k
maybeMaximum = fmap maximum . nonEmpty . catMaybes

maxId :: EditorDocument -> Maybe AnnotationId
maxId EditorDocument{..} = maybeMaximum
  [ maxEdits undos
  , maxEdits redos
  , maxKey annotations
  ]

nextId :: EditorDocument -> AnnotationId
nextId = fromMaybe 0 .  fmap (+1) . maxId


subParts :: EditorDocument -> AnnotationId -> Set Int
subParts doc k = fromMaybe mempty $  do
  ann <- M.lookup k (doc ^. #annotations)
  return $ shapeParts (ann ^. #shape)

documentParts :: EditorDocument -> DocParts
documentParts = fmap (shapeParts . view #shape) . view #annotations

shapeParts :: Shape -> Set Int
shapeParts = \case
    BoxShape _                  -> S.fromList [0..3]
    CircleShape _               -> S.singleton 0
    LineShape (WideLine points)   -> S.fromList [0..length points]
    PolygonShape (Polygon points) -> S.fromList [0..length points]


lookupTargets :: EditorDocument -> [AnnotationId] -> Map AnnotationId (Maybe Annotation)
lookupTargets EditorDocument{annotations} targets = M.fromList modified where
  modified = lookup' annotations <$> targets
  lookup' m k = (k, M.lookup k m)

applyCmd :: EditCmd -> EditorDocument -> EditorDocument
applyCmd cmd doc = fromMaybe doc (snd <$> applyCmd' cmd doc)

applyCmd' :: EditCmd -> EditorDocument -> Maybe (DocumentPatch', EditorDocument)
applyCmd' DocUndo doc = applyUndo doc
applyCmd' DocRedo doc = applyRedo doc
applyCmd' (DocEdit e) doc = applyEdit e doc


maybePatchDocument :: Maybe DocumentPatch' -> EditorDocument -> EditorDocument
maybePatchDocument Nothing  = id
maybePatchDocument (Just p) = patchDocument p

patchDocument :: DocumentPatch' -> EditorDocument -> EditorDocument
patchDocument (PatchAnns' p)  = over #annotations (patchMap p)
patchDocument' (PatchArea' b) = #validArea .~ b


applyEdit :: Edit -> EditorDocument -> Maybe (DocumentPatch', EditorDocument)
applyEdit e doc = do
  (inverse, patch) <- patchInverse doc (editPatch e doc)
  return (patch, patchDocument patch doc
    & #redos .~ mempty
    & #undos %~ (inverse :))


applyUndo :: EditorDocument -> Maybe (DocumentPatch', EditorDocument)
applyUndo doc = do
  (e, undos) <- uncons (doc ^. #undos)
  (inverse, patch) <- patchInverse doc e
  return (patch, patchDocument patch doc
    & #undos .~ undos
    & #redos %~ (inverse :))


applyRedo :: EditorDocument -> Maybe (DocumentPatch', EditorDocument)
applyRedo doc = do
  (e, redos) <- uncons (doc ^. #redos)
  (inverse, patch) <- patchInverse doc e
  return (patch, patchDocument patch doc
    & #redos .~ redos
    & #undos %~ (inverse :))




toEnumSet :: forall a. (Bounded a, Enum a, Ord a) => Set Int -> Set a
toEnumSet = S.mapMonotonic toEnum . S.filter (\i -> i >= lower && i <= upper)
  where (lower, upper) = (fromEnum (minBound :: a), fromEnum (maxBound :: a))


transformBoxParts :: Rigid -> Set Int -> Box -> Box
transformBoxParts rigid parts = transformCorners rigid (toEnumSet parts)

sides :: Set Corner -> (Bool, Bool, Bool, Bool)
sides corners =
    (corner TopLeft     || corner BottomLeft
    , corner TopRight    || corner BottomRight
    , corner TopLeft     || corner TopRight
    , corner BottomLeft  || corner BottomRight
    ) where corner = flip S.member corners

transformCorners :: Rigid -> Set Corner -> Box -> Box
transformCorners (s, V2 tx ty) corners = translateBox  . scaleBox scale where
  scale = V2 (if left && right then s else 1)
             (if top && bottom then s else 1)

  translateBox (Box (V2 lx ly) (V2 ux uy)) = getBounds
    [ V2 (lx + mask left * tx) (ly + mask top * ty)
    , V2 (ux + mask right * tx) (uy + mask bottom * ty)
    ]

  mask b = if b then 1 else 0
  (left, right, top, bottom) = sides corners


_subset indexes = traversed . ifiltered (const . flip S.member indexes)

_without indexes = traversed . ifiltered (const . not . flip S.member indexes)


subset :: Traversable f => Set Int -> f a -> [a]
subset indexes f = toListOf (_subset indexes) f

without :: Traversable f => Set Int -> f a -> [a]
without indexes f = toListOf (_without indexes) f



transformPoint :: Position -> Rigid -> Position -> Position
transformPoint centre (s, t) =  (+ t) . (+ centre) . (^* s) . subtract centre


transformVertices :: Rigid -> NonEmpty Position -> NonEmpty Position
transformVertices  rigid points = transformPoint centre rigid <$> points
    where centre = boxCentre (getBounds points)


transformPolygonParts :: Rigid -> Set Int -> Polygon -> Polygon
transformPolygonParts rigid indexes (Polygon points) = Polygon $ fromMaybe points $ do
  centre <- boxCentre . getBounds <$> nonEmpty (subset indexes points)
  return  (over (_subset indexes) (transformPoint centre rigid) points)


transformLineParts :: Rigid -> Set Int -> WideLine -> WideLine
transformLineParts rigid indexes (WideLine circles) =  WideLine $
  over (_subset indexes) (transformCircle rigid) circles


transformCircle :: Rigid -> Circle -> Circle
transformCircle (s, t) (Circle p r) = Circle (p + t) (r * s)

transformBox :: Rigid -> Box -> Box
transformBox (s, t) = over boxExtents
  (\Extents{..} -> Extents (centre + t) (extents ^* s))


transformShape :: Rigid -> Shape -> Shape
transformShape rigid = \case
  CircleShape c      -> CircleShape       $ transformCircle rigid c
  BoxShape b      -> BoxShape       $ transformBox rigid b
  PolygonShape poly -> PolygonShape $ poly & over #points (transformVertices rigid)
  LineShape line -> LineShape       $ line & over #points (fmap (transformCircle rigid))


transformParts :: Rigid -> Set Int -> Shape -> Shape
transformParts rigid parts = \case
  CircleShape c      -> CircleShape $ transformCircle rigid c
  BoxShape b        -> BoxShape     $ transformBoxParts rigid parts b
  PolygonShape poly -> PolygonShape $ transformPolygonParts rigid parts poly
  LineShape line    -> LineShape    $ transformLineParts rigid parts line



deleteParts :: Set Int -> Shape -> Maybe Shape
deleteParts parts = \case
  CircleShape _     -> Nothing
  BoxShape _        -> Nothing
  PolygonShape (Polygon points)  -> PolygonShape . Polygon <$> nonEmpty (without parts points)
  LineShape    (WideLine points) -> LineShape . WideLine   <$> nonEmpty (without parts points)



modifyShapes :: (a -> Shape -> Maybe Shape) ->  Map AnnotationId a -> EditorDocument ->  DocumentPatch
modifyShapes f m doc = PatchAnns $ M.intersectionWith f' m (doc ^. #annotations) where
  f' a annot  = case f a (annot ^. #shape) of
    Nothing    -> Delete
    Just shape -> Modify (annot & #shape .~ shape)


setClassEdit ::  ClassId -> Set AnnotationId -> EditorDocument -> DocumentPatch
setClassEdit classId parts doc = PatchAnns $ M.intersectionWith f (setToMap' parts) (doc ^. #annotations) where
  f _ annot = Modify (annot & #label .~ classId)

deletePartsEdit ::   DocParts -> EditorDocument -> DocumentPatch
deletePartsEdit = modifyShapes deleteParts


setAreaEdit :: Maybe Box -> EditorDocument -> DocumentPatch
setAreaEdit b _ = PatchArea b


transformPartsEdit ::  Rigid -> DocParts -> EditorDocument ->  DocumentPatch
transformPartsEdit  rigid = modifyShapes (\parts -> Just . transformParts rigid parts)

transformEdit ::  Rigid -> Set AnnotationId -> EditorDocument ->  DocumentPatch
transformEdit  rigid ids  = modifyShapes (const $ Just . transformShape rigid) (setToMap' ids)

clearAllEdit :: EditorDocument -> DocumentPatch
clearAllEdit doc = PatchAnns $ const Delete <$> doc ^. #annotations


detectionEdit :: [Detection] -> EditorDocument -> DocumentPatch
detectionEdit detections doc = replace (fromDetections (nextId doc) detections) doc


replace :: AnnotationMap -> EditorDocument -> DocumentPatch
replace anns  EditorDocument{annotations}  = PatchAnns $ changes <$> align annotations anns where
  changes (These _ ann) = Modify ann
  changes (This _)   = Delete
  changes (That ann) = Add ann


confirmDetectionEdit :: Set AnnotationId -> EditorDocument -> DocumentPatch
confirmDetectionEdit ids doc = PatchAnns $ M.intersectionWith f (setToMap' ids) (doc ^. #annotations) where
  f = const (Modify . confirmDetection)

confirmDetection :: Annotation -> Annotation
confirmDetection = #detection . traverse . _1 .~ Confirmed


addEdit :: [BasicAnnotation] -> EditorDocument ->  DocumentPatch
addEdit anns doc = PatchAnns $ Add <$> anns' where
  anns' = M.fromList (zip [nextId doc..] $ fromBasic <$> anns)

patchMap :: (Ord k) =>  Map k (Maybe a) -> Map k a -> Map k a
patchMap patch m = m `diff` patch <> M.mapMaybe id adds where
  adds = patch `M.difference` m
  diff = M.differenceWith (flip const)

editPatch :: Edit -> EditorDocument -> DocumentPatch
editPatch = \case
  SetClassEdit i ids          -> setClassEdit i ids
  DeletePartsEdit parts       -> deletePartsEdit parts
  TransformPartsEdit t parts  -> transformPartsEdit t parts
  ClearAllEdit                -> clearAllEdit
  DetectionEdit detections    -> detectionEdit detections
  AddEdit anns                -> addEdit anns
  SetAreaEdit area            -> setAreaEdit area
  ConfirmDetectionEdit ids    -> confirmDetectionEdit ids

patchInverse :: EditorDocument -> DocumentPatch -> Maybe (DocumentPatch, DocumentPatch')
patchInverse doc (PatchAnns e) = do
  undoPatch  <- itraverse (patchAnnotations (doc ^. #annotations)) e
  return (PatchAnns (fst <$> undoPatch), PatchAnns' $ snd <$> undoPatch)

patchInverse doc (PatchArea b) =
  return (PatchArea (doc ^. #validArea), PatchArea' b)


patchAnnotations :: AnnotationMap -> AnnotationId -> AnnotationPatch ->  Maybe (AnnotationPatch, Maybe Annotation)
patchAnnotations anns k action  =  case action of
  Add ann   -> return (Delete, Just ann)
  Delete    -> do
    ann <- M.lookup k anns
    return (Add ann, Nothing)
  Modify ann -> do
    ann' <- M.lookup k anns
    return (Modify ann', Just ann)
