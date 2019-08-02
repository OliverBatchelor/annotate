module Annotate.Editor 
  ( module Annotate.DeepPatchMap
  , module Annotate.Editor

  ) where

import Annotate.Prelude
import Annotate.Common
import Annotate.DeepPatchMap

import qualified Data.Map as Map
import qualified Data.Set as S

import Data.List (uncons)

import Control.Lens hiding (uncons, without)
import Data.List.NonEmpty (nonEmpty)

import Data.Align
import Data.These

import Control.Lens (makePrisms)

import Debug.Trace


type EditError = Text
type DocPart = (AnnotationId, Maybe Int)

mergeParts :: DocParts -> DocParts -> DocParts
mergeParts = Map.unionWith mappend


data AnnotationPatch 
  = Transform Rigid (Set Int)
  | SetTag DetectionTag
  | SetClass ClassId
  deriving (Generic, Show, Eq)

data DocumentPatch
  = PatchAnns (DeepPatchMap AnnotationId AnnotationPatch)
  -- | PatchArea (Maybe Box)
  deriving (Eq, Show, Generic)

-- DocumentPatch' is a non invertible version of DocumentPatch (less details)
-- used for patching the AnnotationMap to update the view.
data DocumentPatch'
  = PatchAnns' (DeepPatchMap AnnotationId (Identity Annotation))
     deriving (Show, Eq, Generic)

data Editor = Editor
  { name  :: DocName
  , undos :: [DocumentPatch]
  , redos :: [DocumentPatch]
  , annotations :: AnnotationMap
  , session :: Session
  } deriving (Generic, Show)


instance Patch AnnotationPatch where
  type PatchTarget AnnotationPatch = Annotation
  apply p ann = preview _Right $ fmap snd $ patchAnnotation ann p 
  -- $ preview _Right $ fmap snd $ patchAnnotation a p


isAnnotated :: Document -> Bool
isAnnotated = not . isNew . view (#info . #category)


isNew :: ImageCat -> Bool
isNew cat = cat == CatNew || cat == CatDiscard

isModified :: Editor -> Bool
isModified = not . null . view #undos

newDetection :: Detection -> Annotation
newDetection d@Detection{..} = Annotation{..}
    where detection = Just (Detected, d) 

fromDetections :: AnnotationId -> [Detection] -> AnnotationMap
fromDetections i detections = Map.fromList (zip [i..] $ newDetection <$> detections) where
  
missedDetection :: Detection -> Annotation
missedDetection d@Detection{..} = Annotation {..}
    where detection = Just (Missed, d) 

reviewDetections :: [Detection] -> AnnotationMap -> AnnotationMap 
reviewDetections = flip (foldr addReview) where

  addReview d = case d ^. #match of
    Just i  -> Map.adjust (setReview d) i
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
insertAuto a m = Map.insert k a m
    where k = 1 + fromMaybe 0 (maxKey m)  
       

thresholdDetection :: Float -> Annotation -> Bool
thresholdDetection t Annotation{detection} = case detection of 
  Just (tag, Detection{confidence}) -> 
          tag == Detected && confidence >= t 
          || has _Confirmed tag
          || tag == Review

  Nothing -> True

thresholdDetections :: Float -> AnnotationMap -> BasicAnnotationMap
thresholdDetections t = fmap toBasic . Map.filter (thresholdDetection t) 
  

allAnnotations :: Editor -> [AnnotationId]
allAnnotations Editor{annotations} = Map.keys annotations

lookupAnnotations :: [AnnotationId] -> Editor -> AnnotationMap
lookupAnnotations objs Editor{annotations} = Map.fromList $ catMaybes $ fmap lookup' objs
    where lookup' k = (k, ) <$> Map.lookup k annotations


maxEdits :: [DocumentPatch] -> Maybe AnnotationId
maxEdits =  maybeMaximum . fmap maxEdit

maxEdit :: DocumentPatch -> Maybe AnnotationId
maxEdit (PatchAnns e) = maxKey $ unDeepPatchMap e


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
  ann <- Map.lookup k (doc ^. #annotations)
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
lookupTargets Editor{annotations} targets = Map.fromList modified where
  modified = lookup' annotations <$> targets
  lookup' m k = (k, Map.lookup k m)

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
patchDocument (PatchAnns' p)  = over #annotations (\anns -> fromMaybe anns $ apply p anns)
-- patchDocument (PatchArea' b) = #validArea .~ b


applyEdit :: Edit -> Editor -> Either EditError  (DocumentPatch', Editor)
applyEdit e doc = do
  (inverse, patch) <- patchInverse doc (editPatch e doc)
  return (patch, patchDocument patch doc
    & #redos .~ mempty
    & #undos %~ (inverse :))


takeUndo :: Editor -> Either EditError (DocumentPatch, [DocumentPatch])
takeUndo doc = maybeError "empty undos" $ uncons (doc ^. #undos)

takeRedo :: Editor -> Either EditError (DocumentPatch, [DocumentPatch])
takeRedo doc = maybeError "empty redos" $ uncons (doc ^. #redos)


applyPatch :: DocumentPatch -> Editor -> Either EditError (DocumentPatch, Editor)
applyPatch e editor = do
  (inverse, patch) <- patchInverse editor e
  return (inverse, patchDocument patch editor)


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


transformBoxParts :: Rigid -> Set Int -> Box -> (Box, Set Int)
transformBoxParts t parts box = (box', S.mapMonotonic fromEnum corners) where
  (box', corners) = transformCorners t (toEnumSet parts) box

sides :: Set Corner -> (Bool, Bool, Bool, Bool)
sides corners =
    (corner TopLeft     || corner BottomLeft
    , corner TopRight    || corner BottomRight
    , corner TopLeft     || corner TopRight
    , corner BottomLeft  || corner BottomRight
    ) where corner = flip S.member corners

flipH :: Corner -> Corner
flipH TopLeft = TopRight
flipH TopRight = TopLeft
flipH BottomLeft = BottomRight
flipH BottomRight = BottomLeft

flipV :: Corner -> Corner
flipV TopLeft = BottomLeft
flipV TopRight = BottomRight
flipV BottomLeft = TopLeft
flipV BottomRight = TopRight



remapCorners :: Set Corner -> Box -> (Box, Set Corner)
remapCorners corners (Box l u) = (getBounds [l, u], remapped) where
  remapped = S.fromList (mapCorner <$> S.toList corners)

  (V2 fx fy) = liftI2 (>) l u
  mapCorner = if fx then flipH else id . if fy then flipV else id


transformCorners :: Rigid -> Set Corner -> Box -> (Box, Set Corner)
transformCorners (s, V2 tx ty) corners = remapCorners corners . translateBox  . scaleBox scale where
  scale = V2 (if left && right then s else 1)
             (if top && bottom then s else 1)

  translateBox (Box (V2 lx ly) (V2 ux uy)) = Box
      (V2 (lx + mask left * tx) (ly + mask top * ty))
      (V2 (ux + mask right * tx) (uy + mask bottom * ty))

  mask b = if b then 1 else 0
  (left, right, top, bottom) = sides corners

_subset indexes = traversed . ifiltered (const . flip S.member indexes)

_without indexes = traversed . ifiltered (const . not . flip S.member indexes)


subset :: Traversable f => Set Int -> f a -> [a]
subset indexes f = toListOf (_subset indexes) f

without :: Traversable f => Set Int -> f a -> [a]
without indexes f = toListOf (_without indexes) f



transformPoint :: Point -> Rigid -> Point -> Point
transformPoint centre (s, t) =  (+ t) . (+ centre) . (^* s) . subtract centre


transformVertices :: Rigid -> NonEmpty Point -> NonEmpty Point
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


transformParts :: Rigid -> Set Int -> Shape -> (Shape, Set Int)
transformParts t parts = \case
  ShapeCircle c      -> (ShapeCircle $ transformCircle t c, parts)
  ShapeBox b        -> (ShapeBox b', parts')
    where (b', parts') = transformBoxParts t parts b

  ShapePolygon poly -> (ShapePolygon $ transformPolygonParts t parts poly, parts)
  ShapeLine line    -> (ShapeLine    $ transformLineParts t parts line, parts)



-- deleteParts :: Set Int -> Shape -> Maybe Shape
-- deleteParts parts = \case
--   ShapeCircle _     -> Nothing
--   ShapeBox _        -> Nothing
--   ShapePolygon (Polygon points)  -> ShapePolygon . Polygon <$> nonEmpty (without parts points)
--   ShapeLine    (WideLine points) -> ShapeLine . WideLine   <$> nonEmpty (without parts points)


deleteParts :: Set Int -> Shape -> Modify AnnotationPatch
deleteParts parts = \case
  ShapeCircle _     -> Delete
  ShapeBox _        -> Delete
  ShapePolygon (Polygon points)  -> error "TODO: deleteParts"
  ShapeLine    (WideLine points) -> error "TODO: deleteParts"




addEdit :: [BasicAnnotation] -> Editor ->  DocumentPatch
addEdit anns doc = patchAnns $ Add <$> anns' where
  anns' = Map.fromList (zip [nextId doc..] $ fromBasic <$> anns)

patchMap :: (Ord k) =>  Map k (Maybe a) -> Map k a -> Map k a
patchMap patch m = m `diff` patch <> Map.mapMaybe id adds where
  adds = patch `Map.difference` m
  diff = Map.differenceWith (flip const)


editAnnotations :: (a -> Annotation -> Modify AnnotationPatch) ->  Map AnnotationId a -> Editor ->  DocumentPatch
editAnnotations f m doc = patchAnns $ Map.intersectionWith f m (doc ^. #annotations)

editShapes :: (a -> Shape -> Modify AnnotationPatch) ->  Map AnnotationId a -> Editor ->  DocumentPatch
editShapes f  = editAnnotations f' 
  where f' a = f a . view #shape
  
setClassEdit ::  ClassId -> Set AnnotationId -> Editor -> DocumentPatch
setClassEdit classId parts = editAnnotations (\_ _ -> Modify $ SetClass classId) (setToMap' parts)

deletePartsEdit ::   DocParts -> Editor -> DocumentPatch
deletePartsEdit = editShapes deleteParts

transformPartsEdit ::  Rigid -> DocParts -> Editor ->  DocumentPatch
transformPartsEdit t = editAnnotations (\parts _ -> Modify $ Transform t parts)

clearAllEdit :: Editor -> DocumentPatch
clearAllEdit doc = patchAnns $ const Delete <$> doc ^. #annotations

  
confirmDetectionEdit :: Map AnnotationId Bool -> Editor -> DocumentPatch
confirmDetectionEdit = editAnnotations $ \b -> const (Modify $ SetTag (Confirmed b))

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
lookupAnn k m  = maybeError "missing annotation key" (Map.lookup k m)



patchAnns :: Map AnnotationId (Modify AnnotationPatch) -> DocumentPatch
patchAnns = PatchAnns . DeepPatchMap

patchAnns' :: Map AnnotationId (Modify (Identity Annotation)) -> DocumentPatch'
patchAnns' = PatchAnns' . DeepPatchMap


patchInverse :: Editor -> DocumentPatch -> Either EditError (DocumentPatch, DocumentPatch')
patchInverse doc (PatchAnns e) = do
  undoPatch  <- itraverse (patchAnnotationMap (doc ^. #annotations)) (unDeepPatchMap e)
  return (patchAnns (fst <$> undoPatch), patchAnns' (snd <$> undoPatch))

-- patchInverse doc (PatchArea b) =
--   return (PatchArea (doc ^. #validArea), PatchArea' b)

replace :: a -> Modify (Identity a)
replace = Modify . Identity


patchAnnotationMap :: AnnotationMap -> AnnotationId -> Modify AnnotationPatch -> Either EditError (Modify AnnotationPatch, Modify (Identity Annotation))
patchAnnotationMap anns k action  =  case action of
  Add ann   -> return (Delete, Add ann)
  Delete    -> do
    ann <- lookupAnn k anns
    return (Add ann, Delete)
  Modify p -> do
    ann <- lookupAnn k anns
    (p', ann') <- patchAnnotation ann p
    return (Modify p', replace ann')


patchAnnotation :: Annotation -> AnnotationPatch -> Either EditError (AnnotationPatch, Annotation)
patchAnnotation ann  = \case 
  Transform t parts -> do
    let (shape', parts') = transformParts t parts (ann ^. #shape)
    return  (Transform (invert t) parts', ann & #shape .~ shape')
  SetClass c -> do
    return (SetClass (ann ^. #label), ann & #label .~ c)
  SetTag tag -> do
    (tag', _) <- maybeError "set tag: annotation has no detection" (ann ^. #detection)
    return (SetTag tag', ann & #detection . traverse . _1 .~ tag)
  

  

lastThreshold :: Session -> Float
lastThreshold Session{threshold, history} = fromMaybe threshold (maybeLast thresholds) where
  thresholds = catMaybes $ preview (_2 . _HistoryThreshold) <$> history
  maybeLast xs = preview (_Cons . _1) (reverse xs)


editCmd :: HistoryPair -> Maybe EditCmd
editCmd (_, HistoryUndo) = Just DocUndo
editCmd (_, HistoryRedo) = Just DocRedo
editCmd (_, HistoryEdit e) = Just (DocEdit e)
editCmd _ = Nothing

replay :: Session -> (Editor, BasicAnnotationMap)
replay session = (editor', result) where
  cmds   = catMaybes (editCmd <$> session ^. #history)
  editor = openSession "" session
  editor' = foldl (flip applyCmd) editor cmds
  result = thresholdDetections (lastThreshold session) (editor' ^. #annotations)


replays :: Session -> (Editor, [(EditCmd, Editor)])
replays session = (editor, zip cmds editors) where
  editor = openSession "" session
  cmds   = catMaybes (editCmd <$> session ^. #history)
  editors = drop 1 $ scanl (flip applyCmd) editor cmds

checkReplay :: (Session, BasicAnnotationMap) -> Bool
checkReplay (session, result) = snd (replay session) ~= result 


sessionResults :: [Session] -> BasicAnnotationMap -> [(Session, BasicAnnotationMap)]
sessionResults sessions final = zip sessions (drop 1 results <> [final])
  where results  = view #initial <$> sessions 

documentSessions :: Document -> [(Session, BasicAnnotationMap)]
documentSessions Document{sessions, annotations} = sessionResults sessions annotations


checkReplays :: Document -> Bool
checkReplays = all checkReplay . documentSessions



makePrisms ''AnnotationPatch  
makePrisms ''DocumentPatch  
makePrisms ''DocumentPatch'
makePrisms ''EditCmd
