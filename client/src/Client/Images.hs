module Client.Images where

import Annotate.Prelude hiding (div)
import Annotate.Common hiding (label)

import Client.Common
import Client.Widgets
import Client.Select

import Data.Ord (comparing, Ordering(..))
import Data.List (sortBy)

import qualified GHC.Real as Real

import Builder.Html
import qualified Builder.Html as Html

import qualified Data.Map as M

import Data.Time.Format.Human

showHeader :: Builder t m => m ()
showHeader = tr [] $ do
    th [width 60] $ text "File"
    th [width 10] $ text "Objects"
    th [width 20] $ text "Category"
      where
        width n = style_ =: [("width", showText n <> "%")]


showImage :: Builder t m => Dynamic t (Maybe (DocName, DocInfo)) -> Dynamic t Bool -> m (Event t DocName)
showImage d selected = do
  e <- tr_ [class_ ~: gated "table-active" selected, style_ ~: rowStyle <$> d] $ do
    td [width 60] $ dynText (fromMaybe "" <$> name)
    td [width 10] $ dynText (numAnnotations <$> info)
    td [width 20] $ dynText (category <$> info)

  return $ filterMaybe (current name `tag` domEvent Click e)

    where
      numAnnotations = fromMaybe "" . fmap (showText . view #numAnnotations)
      category = fromMaybe "" . fmap (showText . view #category)
      width n = style_ =: [("width", showText n <> "%")]

      rowStyle item = [("height", "40px"), ("pointer-events", if isNothing item then "none" else "inherit")]

      info = fmap snd <$> d
      name = fmap fst <$> d


data SortKey = SortCat | SortNum | SortName | SortModified
  deriving (Eq, Generic)

data FilterOption = FilterAll | FilterCat ImageCat | FilterEdited
  deriving (Eq, Generic)

instance Show FilterOption where
  show FilterAll        = "All"
  show (FilterCat cat)  = show cat
  show FilterEdited     = "Edited"

readFilter :: Text -> FilterOption
readFilter = \case
  "New" -> FilterCat New
  "Train" -> FilterCat Train
  "Test"  -> FilterCat Test
  "Discard"  -> FilterCat Discard
  "Edited"       -> FilterEdited
  _ -> FilterAll

filterImage :: FilterOption -> (DocName, DocInfo) -> Bool
filterImage opt (_, DocInfo{category}) = case opt of
  FilterAll     -> category /= Discard
  FilterEdited  -> category == Train || category == Test
  FilterCat cat -> category == cat

allFilters :: [FilterOption]
allFilters = [FilterAll, FilterEdited, FilterCat Train, FilterCat Test, FilterCat New, FilterCat Discard]

reverseComparing :: Ord b => (a -> b) -> a -> a -> Ordering
reverseComparing f x y = reverseOrdering $ compare (f x) (f y)

reverseOrdering :: Ordering -> Ordering
reverseOrdering LT = GT
reverseOrdering GT = LT
reverseOrdering EQ = EQ

compareKey ::  (Bool, SortKey) -> (DocName, DocInfo) -> (DocName, DocInfo) -> Ordering
compareKey (rev, key) = case key of
  SortCat         -> compares rev (view (_2 . #category))
  SortNum         -> compares rev (view (_2 . #numAnnotations))
  SortModified    -> compares rev (view (_2 . #modified))
  SortName        -> compares rev (view _1)


compares :: Ord b => Bool -> (a -> b) -> a -> a -> Ordering
compares rev = if rev then reverseComparing else comparing




selectOption :: Builder t m => (a -> Text) -> (Text -> a) -> [Property t] -> [a] -> a -> m (Dynamic t a)
selectOption toText fromText props options initial = fmap fromText . _selectElement_value <$> selectElem_
    props  (def & selectElementConfig_initialValue .~ toText initial)  (traverse_ makeOption options)

    where
      makeOption a = option [value_ =: toText a] $ text (toText a)


enabled_ :: Attribute Bool
enabled_ = contramap not disabled_


imagesTab :: forall t m. AppBuilder t m => m ()
imagesTab = column "h-100 p-1 v-spacing-2" $ do

  images  <- fmap (M.toList . view #images) <$> view #collection
  selected <- view #userSelected

  (searchText, filterOpt) <- row "spacing-2" $ do
    search <- div [class_ =: "input-group grow-3"] $ _inputElement_value <$>
        inputElem [ class_ =: "form-control", type_ =: "search", placeholder_ =: "Search..." ]  def
    opt <- selectOption showText readFilter [ class_ =: "custom-select grow-1" ] allFilters FilterAll
    return (search, opt)

  sortKey <- holdDyn (True, SortCat) never
  let sorted   = sortImages <$> sortKey <*> filterOpt <*> images
      searched = searchImages <$> searchText <*> sorted

  rec
    offset <- holdDyn 0 $ leftmost
      [ 0 <$ updated searched
      , attachWith (+) (current offset) updatePage
      ]

    userSelect <- table [class_ =: "table table-sm table-hover"] $ do
      thead [] $ showHeader
      tbody [class_ =: "scroll"] $ selectPaged (pure size) offset searched showImage selected


    updatePage <- row "justify-content-between align-items-center" $ do
      dec <- button_ [class_ =: "btn btn-outline", enabled_ ~: hasPrev <$> offset ] $ icon "chevron-double-left"
      span [] $ dynText (showPage <$> offset <*> searched)
      inc <- button_ [class_ =: "btn btn-outline", enabled_ ~: hasNext <$> offset <*> searched ] $ icon "chevron-double-right"

      return $ leftmost
        [ size <$ domEvent Click inc
        , -size <$ domEvent Click dec]

  command LoadCmd userSelect

  return ()
    where
      hasPrev i = (i > 0)
      hasNext i images = (i + size < length images)

      sortImages k opt = sortBy (compareKey k) . filter (filterImage opt)
      searchImages _ = id
      --searchImages t  = simpleFilter t

      showPage i images = showText (pageNum i) <> " of " <> showText (pageNum (length images))
      pageNum i = i `Real.div` size + 1

      size = 15
