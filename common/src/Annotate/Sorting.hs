module Annotate.Sorting where

import Annotate.Prelude
import Annotate.Common

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.Fuzzy as Fuzzy

import Data.List (splitAt, elemIndex,  dropWhile, sortBy, findIndex)
import Data.Ord (comparing)



filterImage :: FilterOption -> Bool -> (DocName, DocInfo) -> Bool
filterImage opt inverse (_, DocInfo{reviews, category, modified}) = (case opt of
  FilterCat CatDiscard -> category == CatDiscard
  _ -> category /= CatDiscard && (inverse `xor` cond))
    where

  usedInTraining = category == CatTrain || category == CatTest || category == CatValidate
  cond = case opt of
    FilterAll     -> True
    FilterEdited  ->  isJust modified
    FilterCat cat ->  category == cat
    FilterForReview -> reviews == 0 && usedInTraining
    FilterReviewed -> reviews > 0 && usedInTraining





reverseComparing :: Ord b => (a -> b) -> a -> a -> Ordering
reverseComparing f x y = reverseOrdering $ compare (f x) (f y)

reverseOrdering :: Ordering -> Ordering
reverseOrdering LT = GT
reverseOrdering GT = LT
reverseOrdering EQ = EQ

detectionScore :: DocInfo -> Maybe Float
detectionScore = preview (#detections . traverse . #score)

variationScore :: DocInfo -> Maybe Float
variationScore = join . preview (#detections . traverse . #frameVariation)

totalCounts :: DocInfo -> Maybe Count
totalCounts = join . preview (#detections . traverse . #counts)

countVariation :: DocInfo -> Maybe Int
countVariation = fmap range . join . preview (#detections . traverse . #counts)
    where range Count{upper, lower} = abs (snd lower - snd upper)



compareWith ::  Bool -> SortKey -> (DocName, DocInfo) -> (DocName, DocInfo) -> Ordering
compareWith rev key = (case key of
  SortCategory     -> compares (view #category)
  SortAnnotations  -> compares (view #numAnnotations)
  SortModified     -> compares (view #modified)

  SortDetections   -> compares detectionScore
  SortFrameVariation    -> compares variationScore

  SortCountVariation    -> compares countVariation

  SortCounts    -> compares (fmap (view (#middle . _2)) . totalCounts)
  SortCreation     -> compares (preview (#image . #creation))

  SortLossMean    -> compares (negate . view (#training . #lossMean))
  SortLossRunning -> compares (negate . view (#training . #lossRunning))

  SortName         -> compares (view #naturalKey)
  SortRandom       -> compares (view #hashedName))
  where
    compares :: forall a. Ord a => (DocInfo -> a) -> (DocName, DocInfo) -> (DocName, DocInfo) -> Ordering
    compares f = if rev then reverseComparing (f' . snd) else comparing (f' . snd)
      where f' = f &&& view #naturalKey





minSet :: Ord a => Set a -> Maybe a
minSet s
  | null s        = Nothing
  | otherwise     = Just (S.elemAt 0 s)

lookupNext :: Ord a => Set a -> a -> Maybe a
lookupNext s x = S.lookupGT x s <|> minSet s

maybeNext :: Ord a => Set a -> Maybe a -> Maybe a
maybeNext s = \case
    Nothing -> minSet s
    Just x  -> lookupNext (S.delete x s) x


nextSet :: Ord a => Set a -> Maybe a -> [a]
nextSet s = \case
    Nothing -> S.toList s
    Just x  -> S.toList ys <> S.toList xs
      where (xs, _, ys) = S.splitMember x s

prevSet :: Ord a => Set a -> Maybe a -> [a]
prevSet s = reverse . nextSet s


rotate :: Int -> [a] -> [a]
rotate n xs = bs <> as
  where (as, bs) = splitAt n xs


rotateFrom :: Eq k => Maybe k -> [(k, a)] -> [(k, a)]
rotateFrom current ks = fromMaybe ks $ do
  k <- current
  i <- findIndex ((== k) . fst) ks
  return (drop 1 (rotate i ks))



sortForBrowsing :: SortOptions -> [(DocName, DocInfo)] -> [(DocName, DocInfo)]
sortForBrowsing SortOptions{..} = filterImages filtering search
    . searchImages search
    . sortImages sorting

filterImages :: (FilterOption, Bool) -> Text -> [(DocName, DocInfo)] -> [(DocName, DocInfo)]
filterImages (method, inverse) search = filter (filterImage method inverse) . searchImages search


filterOpts :: SortOptions ->  [(DocName, DocInfo)] -> [(DocName, DocInfo)]
filterOpts SortOptions{..} = filterImages filtering search  . searchImages search


sortImages :: (SortKey, Bool)  -> [(DocName, DocInfo)] -> [(DocName, DocInfo)]
sortImages (sortKey, reversed) = sortBy (compareWith reversed sortKey)


selectionKey :: ImageSelection -> SortKey
selectionKey = \case
    SelSequential  -> SortName
    SelRandom      -> SortRandom
    SelDetections  -> SortDetections
    SelFrameVariation   -> SortFrameVariation
    SelCountVariation   -> SortCountVariation
    SelLoss        -> SortLossRunning

selectingMax :: ImageSelection -> Bool
selectingMax = \case
    SelRandom       -> False
    SelSequential   -> False
    _               -> True

sortForSelection :: (ImageSelection, Bool) -> [(DocName, DocInfo)] -> [(DocName, DocInfo)]
sortForSelection (selection, rev) = sortImages (selectionKey selection, rev)


selectNext :: (ImageSelection, Bool) -> Maybe DocName -> [(DocName, DocInfo)] -> [(DocName, DocInfo)]
selectNext (selection, rev) current = nextFrom  . sortForSelection (selection, rev) where
   nextFrom = if selectingMax selection
     then dropWhile ((== current) . Just . fst) 
     else rotateFrom current

nextImages :: SortOptions -> Maybe DocName -> [(DocName, DocInfo)] -> [(DocName, DocInfo)]
nextImages SortOptions{..} current = selectNext (selection, revSelection) current . filterImages filtering search


searchImages :: Text ->  [(DocName, DocInfo)] -> [(DocName, DocInfo)]
searchImages ""   = id
searchImages str  = \images -> Fuzzy.original <$>
    Fuzzy.filter str images "" "" fst False
