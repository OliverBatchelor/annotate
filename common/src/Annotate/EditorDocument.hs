module Annotate.EditorDocument where

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


type DocParts = Map AnnotationId (Set Int)
type DocPart = (AnnotationId, Maybe Int)

mergeParts :: DocParts -> DocParts -> DocParts
mergeParts = M.unionWith mappend


type DocumentPatch = Map AnnotationId (Maybe Annotation)

data EditAction
  = Add Annotation
  | Delete
  | Modify Annotation
  deriving (Generic, Show, Eq)

newtype Edit = Edit { unEdit :: Map AnnotationId EditAction }
  deriving (Eq, Show, Generic)


data EditorDocument = EditorDocument
  { undos :: [Edit]
  , redos :: [Edit]
  , name  :: DocName
  , info  :: DocInfo
  , annotations :: AnnotationMap
  } deriving (Generic, Show, Eq)


data EditCmd = DocEdit Edit | DocUndo | DocRedo
  deriving (Show, Eq, Generic)

fromDocument :: Document -> EditorDocument
fromDocument Document{..} = EditorDocument
    { undos = []
    , redos = []
    , name = name
    , info = info
    , annotations = annotations
    }

toDocument :: EditorDocument -> Document
toDocument EditorDocument{..} = Document{..}


makePrisms ''EditCmd
makePrisms ''Edit

allAnnotations :: EditorDocument -> [AnnotationId]
allAnnotations EditorDocument{annotations} = M.keys annotations

lookupAnnotations :: [AnnotationId] -> EditorDocument -> AnnotationMap
lookupAnnotations objs EditorDocument{annotations} = M.fromList $ catMaybes $ fmap lookup' objs
    where lookup' k = (k, ) <$> M.lookup k annotations


maxEdits :: [Edit] -> Maybe AnnotationId
maxEdits =  maybeMaximum . fmap maxEdit

maxEdit :: Edit -> Maybe AnnotationId
maxEdit = maxKey . unEdit

maybeMaximum :: (Ord k) => [Maybe k] -> Maybe k
maybeMaximum = fmap maximum . nonEmpty . catMaybes

maxId :: EditorDocument -> Maybe AnnotationId
maxId EditorDocument{..} = maybeMaximum
  [ maxEdits undos
  , maxEdits redos
  , maxKey annotations
  ]

lookupTargets :: EditorDocument -> [AnnotationId] -> Map AnnotationId (Maybe Annotation)
lookupTargets EditorDocument{annotations} targets = M.fromList modified where
  modified = lookup' annotations <$> targets
  lookup' m k = (k, M.lookup k m)

applyCmd :: EditCmd -> EditorDocument -> EditorDocument
applyCmd cmd doc = fromMaybe doc (snd <$> applyCmd' cmd doc)

applyCmd' :: EditCmd -> EditorDocument -> Maybe (DocumentPatch, EditorDocument)
applyCmd' DocUndo doc = applyUndo doc
applyCmd' DocRedo doc = applyRedo doc
applyCmd' (DocEdit e) doc = applyEdit e doc

applyEdit :: Edit -> EditorDocument -> Maybe (DocumentPatch, EditorDocument)
applyEdit e doc = do
  (inverse, patch) <- patchEdit (doc ^. #annotations) e
  return (patch, doc
    & #undos %~ (inverse :)
    & #annotations %~ patchMap patch)


applyUndo :: EditorDocument -> Maybe (DocumentPatch, EditorDocument)
applyUndo doc = do
  (e, undos) <- uncons (doc ^. #undos)
  (inverse, patch) <- patchEdit (doc ^. #annotations) e
  return (patch, doc
    & #undos .~ undos
    & #redos %~ (inverse :)
    & #annotations %~ patchMap patch)


applyRedo :: EditorDocument -> Maybe (DocumentPatch, EditorDocument)
applyRedo doc = do
  (e, redos) <- uncons (doc ^. #redos)
  (inverse, patch) <- patchEdit (doc ^. #annotations) e
  return (patch, doc
    & #redos .~ redos
    & #undos %~ (inverse :)
    & #annotations %~ patchMap patch)




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


type Rigid = (Float, Vec)

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
  BoxShape b      -> BoxShape       $ transformBox rigid b
  PolygonShape poly -> PolygonShape $ poly & over #points (transformVertices rigid)
  LineShape line -> LineShape       $ line & over #points (fmap (transformCircle rigid))


transformParts :: Rigid -> Set Int -> Shape -> Shape
transformParts rigid parts = \case
  BoxShape b        -> BoxShape     $ transformBoxParts rigid parts b
  PolygonShape poly -> PolygonShape $ transformPolygonParts rigid parts poly
  LineShape line    -> LineShape    $ transformLineParts rigid parts line



deleteParts :: Set Int -> Shape -> Maybe Shape
deleteParts parts = \case
  BoxShape b        -> Nothing
  PolygonShape (Polygon points)  -> PolygonShape . Polygon <$> nonEmpty (without parts points)
  LineShape    (WideLine points) -> LineShape . WideLine   <$> nonEmpty (without parts points)



modifyShapes :: (a -> Shape -> Maybe Shape) -> Map AnnotationId a -> EditorDocument -> Edit
modifyShapes f m doc = Edit $ M.intersectionWith f' m (doc ^. #annotations) where
  f' a annot  = case f a (annot ^. #shape) of
    Nothing    -> Delete
    Just shape -> Modify (annot & #shape .~ shape)


deletePartsEdit :: DocParts -> EditorDocument -> Edit
deletePartsEdit = modifyShapes deleteParts


transformPartsEdit :: Rigid -> DocParts -> EditorDocument -> Edit
transformPartsEdit rigid = modifyShapes (\parts -> Just . transformParts rigid parts)


transformEdit :: Rigid -> Set AnnotationId -> EditorDocument -> Edit
transformEdit rigid ids = modifyShapes (const $ Just . transformShape rigid) (setToMap ids)


clearAllEdit :: EditorDocument -> Edit
clearAllEdit doc = Edit $ const Delete <$> doc ^. #annotations


replaceAllEdit :: EditorDocument -> AnnotationMap -> Edit
replaceAllEdit EditorDocument{annotations} new = Edit $ changes <$> align annotations new where
  changes (These _ ann) = Modify ann
  changes (This _)   = Delete
  changes (That ann) = Add ann


addEdit :: AnnotationId -> Annotation -> Edit
addEdit k ann = Edit $ M.singleton k (Add ann)


patchMap :: (Ord k) =>  Map k (Maybe a) -> Map k a -> Map k a
patchMap patch m = m `diff` patch <> M.mapMaybe id adds where
  adds = patch `M.difference` m
  diff = M.differenceWith (flip const)



patchEdit :: AnnotationMap -> Edit -> Maybe (Edit, DocumentPatch)
patchEdit anns (Edit e) = do
  undoPatch <- itraverse (patchEdit' anns) e
  return (Edit (fst <$> undoPatch), snd <$> undoPatch)


patchEdit' :: AnnotationMap -> AnnotationId -> EditAction ->  Maybe (EditAction, Maybe Annotation)
patchEdit' anns k action  =  case action of
  Add ann   -> return (Delete, Just ann)
  Delete    -> do
    ann <- M.lookup k anns
    return (Add ann, Nothing)
  Modify ann -> do
    ann' <- M.lookup k anns
    return (Modify ann', Just ann)
