module Client.Images where

import Annotate.Prelude hiding (div)
import qualified Annotate.Prelude as Prelude

import Annotate.Common hiding (label)

import Client.Common
import Client.Widgets
import Client.Select

import Data.Ord (comparing, Ordering(..))
import Data.List (sortBy, findIndex)

import qualified GHC.Real as Real

import Builder.Html
import qualified Builder.Html as Html

import qualified Data.Map as M

import Data.Time.Format.Human
import qualified Text.Fuzzy as Fuzzy

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


data SortKey = SortCat | SortNum | SortName | SortModified | SortMixed
  deriving (Eq, Generic)

data FilterOption = FilterAll | FilterCat ImageCat | FilterEdited
  deriving (Eq, Generic)

instance Show FilterOption where
  show FilterAll        = "All"
  show (FilterCat cat)  = show cat
  show FilterEdited     = "Edited"

allFilters :: [(Text, FilterOption)]
allFilters =
  [ ("All",     FilterAll)
  , ("New",     FilterCat New)
  , ("Train",   FilterCat Train)
  , ("Test",    FilterCat Test)
  , ("Discard", FilterCat Discard)
  , ("Edited",  FilterEdited)
  ]

filterImage :: FilterOption -> (DocName, DocInfo) -> Bool
filterImage opt (_, DocInfo{category}) = case opt of
  FilterAll     -> category /= Discard
  FilterEdited  -> category == Train || category == Test
  FilterCat cat -> category == cat


reverseComparing :: Ord b => (a -> b) -> a -> a -> Ordering
reverseComparing f x y = reverseOrdering $ compare (f x) (f y)

reverseOrdering :: Ordering -> Ordering
reverseOrdering LT = GT
reverseOrdering GT = LT
reverseOrdering EQ = EQ

compareWith ::  (Bool, SortKey) -> (DocName, DocInfo) -> (DocName, DocInfo) -> Ordering
compareWith (rev, key) = (case key of
  SortCat         -> compares (view #category &&& view #naturalKey)
  SortNum         -> compares (view #numAnnotations &&& view #naturalKey)
  SortModified    -> compares (view #modified &&& view #naturalKey)
  SortName        -> compares (view #naturalKey)
  SortMixed       -> compares (view #hashedName))
  where
    compares :: forall a. Ord a => (DocInfo -> a) -> (DocName, DocInfo) -> (DocName, DocInfo) -> Ordering
    compares f = if rev then reverseComparing (f . snd) else comparing (f . snd)


allSorts :: [(Text, SortKey)]
allSorts =
  [ ("Name",     SortName)
  , ("Mixed",     SortMixed)
  , ("Modified",   SortModified)
  , ("Annotations",    SortNum)
  , ("Category", SortCat)
  ]

enabled_ :: Attribute Bool
enabled_ = contramap not disabled_

toggleButton :: forall t m. AppBuilder t m => (Text, Text) -> m (Dynamic t Bool)
toggleButton icons = mdo
  e <- button_ [class_ =: "btn btn-light"] $
      icon ( (def :: IconConfig t) & #name .~ Dyn (swapping icons isOpen))
  isOpen <- toggle False (domEvent Click e)
  return isOpen


imagesTab :: forall t m. AppBuilder t m => m ()
imagesTab = column "h-100 p-1 v-spacing-2" $ do

  images  <- fmap (M.toList . view #images) <$> view #collection
  selected <- view #docSelected

  (searchText, filterOpt) <- row "spacing-2" $ do
    search <- div [class_ =: "input-group grow-3"] $ _inputElement_value <$>
        inputElem [ class_ =: "form-control", type_ =: "search", placeholder_ =: "Search..." ]  def
    opt <- selectOption [ class_ =: "custom-select grow-1" ] allFilters FilterAll never
    return (search, opt)

  sortOpts <- labelled "Order by" $ row "p-2" $ do
      key <- selectOption [ class_ =: "custom-select grow-1" ] allSorts SortName never
      rev <- toggleButton ("sort-descending", "sort-ascending")
      return $ liftA2 (,) rev key


  let sorted   = sortImages <$> sortOpts <*> filterOpt <*> images
      searched = searchImages <$> searchText <*> sorted

      findOffset :: [(DocName, DocInfo)] -> Maybe DocName -> Maybe Int
      findOffset images Nothing = Nothing
      findOffset images (Just k) = do
        i <- findIndex ((== k) . fst) images
        return $ (i `Prelude.div` size) * size

      searchOffset images k t
        | t == ""   = findOffset images k
        | otherwise = Just 0

  rec
    offset <- holdDyn 0 $ leftmost
      [ searchOffset <$> current searched <*> current selected <??> updated searchText
      , findOffset <$> current searched <??> updated selected
      , attachWith (+) (current offset) updatePage
      ]

    -- logEvent (findOffset <$> current searched <??> updated selected)

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

  command OpenCmd userSelect

  return ()
    where
      hasPrev i = (i > 0)
      hasNext i images = (i + size < length images)

      sortImages k opt = sorting k . filter (filterImage opt)

      sorting k = sortBy (compareWith k)

      searchImages ""   = id
      searchImages str  = \images -> Fuzzy.original <$>
        Fuzzy.filter str images "" "" fst False

      --searchImages t  = simpleFilter t

      showPage i images = showText (pageNum i) <> " of " <> showText (pageNum (length images))
      pageNum i = i `Real.div` size + 1

      size = 15
