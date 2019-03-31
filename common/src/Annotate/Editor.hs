module Annotate.Editor where

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


data EditError = MissingKey AnnotationId | EmptyUndos | EmptyRedos | NoDetection AnnotationId
  deriving (Show, Generic)


type DocPart = (AnnotationId, Maybe Int)

mergeParts :: DocParts -> DocParts -> DocParts
mergeParts = M.unionWith mappend


-- DocumentPatch' is a non invertible version of DocumentPatch (less details)
-- used for patching the AnnotationMap to update the view.
data DocumentPatch'
  = PatchAnns' (Map AnnotationId (Maybe Annotation))
     deriving (Show, Eq, Generic)

data Editor = Editor
  { name  :: DocName
  , undos :: [DocumentPatch]
  , redos :: [DocumentPatch]
  , annotations :: AnnotationMap
  , session :: Session
  } deriving (Generic, Show)

makePrisms ''DocumentPatch'
makePrisms ''EditCmd
makePrisms ''DocumentPatch

isNew :: ImageCat -> Bool
isNew cat = cat == CatNew || cat == CatDiscard

isModified :: Editor -> Bool
isModified = not . null . view #undos

newDetection :: Detection -> Annotation
newDetection d@Detection{..} = Annotation{..}
    where detection = Just (Detected, d) 

fromDetections :: AnnotationId -> [Detection] -> AnnotationMap
fromDetections i detections = M.fromList (zip [i..] $ newDetection <$> detections) where
  
missedDetection :: Detection -> Annotation
missedDetection d@Detection{..} = Annotation {..}
    where detection = Just (Missed, d) 

reviewDetections :: [Detection] -> AnnotationMap -> AnnotationMap 
reviewDetections = flip (foldr addReview) where

  addReview d = case d ^. #match of
    Just i  -> M.adjust (setReview d) i
    Nothing -> insertAuto (missedDetection d)

  setReview d = #detection .~ Just (Review, d)
    


openSession :: DocName -> Session ->  Editor 
openSession name session = Editor 
  { name
  , annotations = initialAnnotations session
  , session
  , undos = []
  , redos = []
  } 

initialAnnotations :: Session -> AnnotationMap
initialAnnotations Session{initial, open} = case open of
    OpenNew d        ->  fromDetections 0 (d ^. #instances)
    OpenReview d     ->  reviewDetections (d ^. #instances) (fromBasic <$> initial)
    OpenDisconnected ->  fromBasic <$> initial



insertAuto :: (Ord k, Num k) => a -> Map k a -> Map k a
insertAuto a m = M.insert k a m
    where k = 1 + fromMaybe 0 (maxKey m)  
       

thresholdDetection :: Float -> Annotation -> Bool
thresholdDetection t Annotation{detection} = case detection of 
  Just (tag, Detection{confidence}) -> 
          tag == Detected && confidence >= t 
          || has _Confirmed tag
          || tag == Review

  Nothing -> True

thresholdDetections :: Float -> AnnotationMap -> BasicAnnotationMap
thresholdDetections t = fmap toBasic . M.filter (thresholdDetection t) 
  

allAnnotations :: Editor -> [AnnotationId]
allAnnotations Editor{annotations} = M.keys annotations

lookupAnnotations :: [AnnotationId] -> Editor -> AnnotationMap
lookupAnnotations objs Editor{annotations} = M.fromList $ catMaybes $ fmap lookup' objs
    where lookup' k = (k, ) <$> M.lookup k annotations


maxEdits :: [DocumentPatch] -> Maybe AnnotationId
maxEdits =  maybeMaximum . fmap maxEdit

maxEdit :: DocumentPatch -> Maybe AnnotationId
maxEdit (PatchAnns e) = maxKey e


maybeMaximum :: (Ord k) => [Maybe k] -> Maybe k
maybeMaximum = fmap maximum . nonEmpty . catMaybes

maxId :: Editor -> Maybe AnnotationId
maxId Editor{..} = maybeMaximum
  [ maxEdits undos
  , maxEdits redos
  , maxKey annotations
  ]

nextId :: Editor -> AnnotationId
nextId = fromMaybe 0 .  fmap (+1) . maxId


subParts :: Editor -> AnnotationId -> Set Int
subParts doc k = fromMaybe mempty $  do
  ann <- M.lookup k (doc ^. #annotations)
  return $ shapeParts (ann ^. #shape)

documentParts :: Editor -> DocParts
documentParts = fmap (shapeParts . view #shape) . view #annotations

shapeParts :: Shape -> Set Int
shapeParts = \case
    ShapeBox _                  -> S.fromList [0..3]
    ShapeCircle _               -> S.singleton 0
    ShapeLine (WideLine points)   -> S.fromList [0..length points]
    ShapePolygon (Polygon points) -> S.fromList [0..length points]


lookupTargets :: Editor -> [AnnotationId] -> Map AnnotationId (Maybe Annotation)
lookupTargets Editor{annotations} targets = M.fromList modified where
  modified = lookup' annotations <$> targets
  lookup' m k = (k, M.lookup k m)

applyCmd :: EditCmd -> Editor -> Editor
applyCmd cmd doc = case (snd <$> applyCmd' cmd doc) of
  Left err      -> doc --error (show cmd <> ": " <> show err) 
  Right editor  -> editor



applyCmd' :: EditCmd -> Editor -> Either EditError (DocumentPatch', Editor)
applyCmd' DocUndo doc = applyUndo doc
applyCmd' DocRedo doc = applyRedo doc
applyCmd' (DocEdit e) doc = applyEdit e doc


maybePatchDocument :: Maybe DocumentPatch' -> Editor -> Editor
maybePatchDocument Nothing  = id
maybePatchDocument (Just p) = patchDocument p

patchDocument :: DocumentPatch' -> Editor -> Editor
patchDocument (PatchAnns' p)  = over #annotations (patchMap p)
-- patchDocument (PatchArea' b) = #validArea .~ b


applyEdit :: Edit -> Editor -> Either EditError  (DocumentPatch', Editor)
applyEdit e doc = do
  (inverse, patch) <- patchInverse doc (editPatch e doc)
  return (patch, patchDocument patch doc
    & #redos .~ mempty
    & #undos %~ (inverse :))


takeUndo :: Editor -> Either EditError (DocumentPatch, [DocumentPatch])
takeUndo doc = maybeError EmptyUndos $ uncons (doc ^. #undos)

takeRedo :: Editor -> Either EditError (DocumentPatch, [DocumentPatch])
takeRedo doc = maybeError EmptyRedos $ uncons (doc ^. #redos)



applyUndo :: Editor -> Either EditError  (DocumentPatch', Editor)
applyUndo doc = do
  (e, undos) <- takeUndo doc
  (inverse, patch) <- patchInverse doc e
  return (patch, patchDocument patch doc
    & #undos .~ undos
    & #redos %~ (inverse :))


applyRedo :: Editor -> Either EditError  (DocumentPatch', Editor)
applyRedo doc = do
  (e, redos) <- takeRedo doc 
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
transformVertices  t points = transformPoint centre t <$> points
    where centre = boxCentre (getBounds points)


transformPolygonParts :: Rigid -> Set Int -> Polygon -> Polygon
transformPolygonParts t indexes (Polygon points) = Polygon $ fromMaybe points $ do
  centre <- boxCentre . getBounds <$> nonEmpty (subset indexes points)
  return  (over (_subset indexes) (transformPoint centre t) points)


transformLineParts :: Rigid -> Set Int -> WideLine -> WideLine
transformLineParts t indexes (WideLine circles) =  WideLine $
  over (_subset indexes) (transformCircle t) circles


transformCircle :: Rigid -> Circle -> Circle
transformCircle (s, t) (Circle p r) = Circle (p + t) (r * s)

transformBox :: Rigid -> Box -> Box
transformBox (s, t) = over boxExtents
  (\Extents{..} -> Extents (centre + t) (extents ^* s))


invert :: Rigid -> Rigid
invert (s, t) = (1/s, -t)


transformShape :: Rigid -> Shape -> Shape
transformShape t = \case
  ShapeCircle c     -> ShapeCircle  $ transformCircle t c
  ShapeBox b        -> ShapeBox     $ transformBox t b
  ShapePolygon poly -> ShapePolygon $ poly & over #points (transformVertices t)
  ShapeLine line    -> ShapeLine    $ line & over #points (fmap (transformCircle t))


transformParts :: Rigid -> Set Int -> Shape -> Shape
transformParts t parts = \case
  ShapeCircle c      -> ShapeCircle $ transformCircle t c
  ShapeBox b        -> ShapeBox     $ transformBoxParts t parts b
  ShapePolygon poly -> ShapePolygon $ transformPolygonParts t parts poly
  ShapeLine line    -> ShapeLine    $ transformLineParts t parts line



-- deleteParts :: Set Int -> Shape -> Maybe Shape
-- deleteParts parts = \case
--   ShapeCircle _     -> Nothing
--   ShapeBox _        -> Nothing
--   ShapePolygon (Polygon points)  -> ShapePolygon . Polygon <$> nonEmpty (without parts points)
--   ShapeLine    (WideLine points) -> ShapeLine . WideLine   <$> nonEmpty (without parts points)


deleteParts :: Set Int -> Shape -> AnnotationPatch
deleteParts parts = \case
  ShapeCircle _     -> Delete
  ShapeBox _        -> Delete
  ShapePolygon (Polygon points)  -> error "TODO: deleteParts"
  ShapeLine    (WideLine points) -> error "TODO: deleteParts"






addEdit :: [BasicAnnotation] -> Editor ->  DocumentPatch
addEdit anns doc = PatchAnns $ Add <$> anns' where
  anns' = M.fromList (zip [nextId doc..] $ fromBasic <$> anns)

patchMap :: (Ord k) =>  Map k (Maybe a) -> Map k a -> Map k a
patchMap patch m = m `diff` patch <> M.mapMaybe id adds where
  adds = patch `M.difference` m
  diff = M.differenceWith (flip const)


editAnnotations :: (a -> Annotation -> AnnotationPatch) ->  Map AnnotationId a -> Editor ->  DocumentPatch
editAnnotations f m doc = PatchAnns $ M.intersectionWith f m (doc ^. #annotations)

editShapes :: (a -> Shape -> AnnotationPatch) ->  Map AnnotationId a -> Editor ->  DocumentPatch
editShapes f  = editAnnotations f' 
  where f' a = f a . view #shape
  
setClassEdit ::  ClassId -> Set AnnotationId -> Editor -> DocumentPatch
setClassEdit classId parts = editAnnotations (const (const (SetClass classId))) (setToMap' parts)

deletePartsEdit ::   DocParts -> Editor -> DocumentPatch
deletePartsEdit = editShapes deleteParts

transformPartsEdit ::  Rigid -> DocParts -> Editor ->  DocumentPatch
transformPartsEdit t = editAnnotations (\parts _ -> Transform t parts)

clearAllEdit :: Editor -> DocumentPatch
clearAllEdit doc = PatchAnns $ const Delete <$> doc ^. #annotations

  
confirmDetectionEdit :: Map AnnotationId Bool -> Editor -> DocumentPatch
confirmDetectionEdit = editAnnotations $ \b -> const (SetTag (Confirmed b))

editPatch :: Edit -> Editor -> DocumentPatch
editPatch = \case
  EditSetClass i ids          -> setClassEdit i ids
  EditDeleteParts parts       -> deletePartsEdit parts
  EditTransformParts t parts  -> transformPartsEdit t parts
  EditClearAll                -> clearAllEdit
  EditAdd anns                -> addEdit anns
  EditConfirmDetection ids    -> confirmDetectionEdit ids

maybeError :: err -> Maybe a -> Either err a
maybeError _ (Just a) = Right a
maybeError err _      = Left err

lookupAnn :: AnnotationId -> Map AnnotationId a -> Either EditError a
lookupAnn k m  = maybeError (MissingKey k) (M.lookup k m)




patchInverse :: Editor -> DocumentPatch -> Either EditError (DocumentPatch, DocumentPatch')
patchInverse doc (PatchAnns e) = do
  undoPatch  <- itraverse (patchAnnotations (doc ^. #annotations)) e
  return (PatchAnns (fst <$> undoPatch), PatchAnns' $ snd <$> undoPatch)

-- patchInverse doc (PatchArea b) =
--   return (PatchArea (doc ^. #validArea), PatchArea' b)




patchAnnotations :: AnnotationMap -> AnnotationId -> AnnotationPatch -> Either EditError (AnnotationPatch, Maybe Annotation)
patchAnnotations anns k action  =  case action of
  Add ann   -> return (Delete, Just ann)
  Delete    -> do
    ann <- lookupAnn k anns
    return (Add ann, Nothing)
  Transform t parts -> do
    ann <- lookupAnn k anns
    return  (Transform (invert t) parts, 
      Just (over #shape (transformParts t parts) ann))

  SetClass c -> do
    ann <- lookupAnn k anns
    return (SetClass (ann ^. #label), Just (ann & #label .~ c))
  SetTag tag -> do
    ann <- lookupAnn k anns
    (tag', _) <- maybeError (NoDetection k) (ann ^. #detection)
    return (SetTag tag', Just (ann & #detection . traverse . _1 .~ tag))
  

  

lastThreshold :: Session -> Float
lastThreshold Session{threshold, history} = fromMaybe threshold (maybeLast thresholds) where
  thresholds = catMaybes $ preview (_2 . _HistoryThreshold) <$> history
  maybeLast xs = preview (_Cons . _1) (reverse xs)


editCmd :: HistoryPair -> Maybe EditCmd
editCmd (_, HistoryUndo) = Just DocUndo
editCmd (_, HistoryRedo) = Just DocRedo
editCmd (_, HistoryEdit e) = Just (DocEdit e)
editCmd _ = Nothing

replay :: Session -> Editor
replay session = foldl (flip applyCmd) editor cmds where
  editor = openSession "" session
  cmds   = catMaybes (editCmd <$> session ^. #history)

-- replays :: Session -> (Editor, [(EditCmd, Editor)])
-- replays (t, open, entries) = (editor, zip cmds editors) where
--   editor = openSession "" t open 
--   cmds   = catMaybes $ editCmd <$> entries
--   editors = drop 1 $ scanl (flip applyCmd) editor cmds

checkReplay :: (Session, BasicAnnotationMap) -> Bool
checkReplay (session, result) = replayed ~= result where
  replayed  =  thresholdDetections t (view #annotations (replay session))
  t = lastThreshold session


sessionResults :: [Session] -> BasicAnnotationMap -> [(Session, BasicAnnotationMap)]
sessionResults sessions final = zip sessions (drop 1 results <> [final])
  where results  = view #initial <$> sessions 


checkReplays :: Document -> Bool
checkReplays doc = all checkReplay results
  where results = sessionResults (doc ^. #sessions) (view #annotations doc)

