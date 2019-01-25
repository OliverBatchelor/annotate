module Annotate.Sorting where

import Annotate.Prelude
import Annotate.Common

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.Fuzzy as Fuzzy

import Data.List (splitAt, elemIndex,  dropWhile, sortBy, findIndex)
import Data.Ord (comparing)



filterImage :: FilterOption -> (DocName, DocInfo) -> Bool
filterImage opt (_, DocInfo{category, modified}) = case opt of
  FilterAll     -> True
  FilterEdited  -> isJust modified
  FilterCat cat -> category == cat

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _        = False

reverseComparing :: Ord b => (a -> b) -> a -> a -> Ordering
reverseComparing f x y = reverseOrdering $ compare (f x) (f y)

reverseOrdering :: Ordering -> Ordering
reverseOrdering LT = GT
reverseOrdering GT = LT
reverseOrdering EQ = EQ

detectionScore :: DocInfo -> Maybe Float
detectionScore = preview (#detections . traverse . #score)

compareWith ::  Bool -> SortKey -> (DocName, DocInfo) -> (DocName, DocInfo) -> Ordering
compareWith rev key = (case key of
  SortCategory     -> compares (view #category)
  SortAnnotations  -> compares (view #numAnnotations)
  SortModified     -> compares (view #modified)

  SortDetections   -> compares detectionScore
  SortLossMean   -> compares (negate . view (#training . #lossMean))
  SortLossMax   -> compares (negate . view (#training . #lossMax))

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
filterImages (method, inverse) search 
    = filter (xor inverse . filterImage method) . searchImages search

sortImages :: (SortKey, Bool)  -> [(DocName, DocInfo)] -> [(DocName, DocInfo)]
sortImages (sortKey, reversed) = sortBy (compareWith reversed sortKey) 


selectionKey :: ImageSelection -> (SortKey, Bool)
selectionKey = \case 
    SelSequential rev -> (SortName, rev)
    SelRandom         -> (SortRandom, False)
    SelDetections rev -> (SortDetections, rev)
    SelLoss           -> (SortLossMax, False)

selectingMax :: ImageSelection -> Bool
selectingMax = \case
    SelRandom       -> False
    SelSequential _ -> False
    _             -> True

sortForSelection :: ImageSelection -> [(DocName, DocInfo)] -> [(DocName, DocInfo)]
sortForSelection selection = sortImages (selectionKey selection)


nextImages :: SortOptions -> Maybe DocName -> [(DocName, DocInfo)] -> [(DocName, DocInfo)]
nextImages SortOptions{..} current = nextFrom  . sortForSelection selection . filterImages filtering search
    where nextFrom = if selectingMax selection
            then dropWhile ((== current) . Just . fst) else rotateFrom current



searchImages :: Text ->  [(DocName, DocInfo)] -> [(DocName, DocInfo)]
searchImages ""   = id
searchImages str  = \images -> Fuzzy.original <$>
    Fuzzy.filter str images "" "" fst False

