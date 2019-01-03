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


showHeader :: Builder t m => m ()
showHeader = tr [] $ do
    th [width 60] $ text "File"
    th [width 10] $ text "Annotations"
    th [width 20] $ text "Category"
      where
        width n = style_ =: [("width", showText n <> "%")]


showImage :: AppBuilder t m => Dynamic t (Maybe (DocName, DocInfo)) -> Dynamic t Bool -> m (Event t DocName)
showImage d selected = do
  e <- tr_ [class_ ~: gated "table-active" selected, style_ ~: rowStyle <$> d] $ do
    td [width 60] $ dynText name'
    td [width 10] $ dynText (numAnnotations <$> info)
    td [width 20] $ dynText (category <$> info)

    preload name'

  return $ filterMaybe (current name `tag` domEvent Click e)

    where
      numAnnotations = fromMaybe "" . fmap (showText . view #numAnnotations)
      category = fromMaybe "" . fmap (showText . view #category)
      width n = style_ =: [("width", showText n <> "%")]

      rowStyle item = [("height", "40px"), ("pointer-events", if isNothing item then "none" else "inherit")]

      info = fmap snd <$> d
      name = fmap fst <$> d
      name' = fromMaybe "" <$> name



allFilters :: [(Text, FilterOption)]
allFilters =
  [ ("All",     FilterAll)
  , ("New",     FilterCat New)
  , ("Train",   FilterCat Train)
  , ("Test",    FilterCat Test)
  , ("Discard", FilterCat Discard)
  , ("Edited",  FilterEdited)
  ]





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



  
imagesTab :: forall t m. AppBuilder t m => m ()
imagesTab = sidePane $ do
  selected    <- view #docSelected
  images      <- fmap (view #images) <$> view #collection
  sortOptions <- fmap (view #sortOptions) <$> view #preferences

  let sorted = sortImages <$> sortOptions <*> (M.toList <$> images)

  groupPane "Image list" $ do
    sortView sortOptions
    imageList 10 selected sorted

    pane $ void $ do   
      dyn $ ffor (maybeLookup <$> selected <*> images) $ 
        traverse_ $ \(k, DocInfo{..}) -> do
          h5 [] $ text k       
          labelled "Annotations" $ text (showText numAnnotations)

    where 
      maybeLookup selected m = do 
        k <- selected
        info <- M.lookup k m
        return (k, info)
       

findOffset :: Int -> [(DocName, DocInfo)] -> Maybe DocName -> Int
findOffset size images selected = fromMaybe 0 $ do
  k <- selected
  i <- findIndex ((== k) . fst) images
  return $ (i `Prelude.div` size) * size


inputView' :: forall t m. Builder t m => [Property t] -> Dynamic t Text -> m (Event t Text)
inputView' props =  toView $ \setText -> _inputElement_value <$> 
  inputElem props (def & inputElementConfig_setValue .~ setText)

searchView :: forall t m. Builder t m => Dynamic t Text -> m (Event t Text)
searchView = inputView' [ class_ =: "form-control", type_ =: "search", placeholder_ =: "Search..." ]


sortView :: forall t m. AppBuilder t m => Dynamic t SortOptions -> m ()
sortView opts = do

  row "spacing-2" $ do
    searched <- div [class_ =: "input-group grow-3"] $ searchView (view #search <$> opts)
    filtered <- grow $ selectView allFilters (view #filtering <$> opts)
    
    sortCommand (SetSearch <$> searched)
    sortCommand (SetFilter <$> filtered)

  labelled "Order by" $ row "p-2" $ do
      key <- grow $ selectView allSorts (view #sortKey <$> opts)
      rev <- toggleButtonView ("sort-descending", "sort-ascending") (view #reversed <$> opts)
      sortCommand (SetSortKey <$> key)


imageList :: forall t m. AppBuilder t m => Int -> Dynamic t (Maybe DocName) -> Dynamic t [(DocName, DocInfo)] -> m ()
imageList size selected images = do   
  rec
    offset <- holdDyn 0 $ leftmost
      [ updated (findOffset size <$> images <*> selected)
      , attachWith (+) (current offset) updatePage
      ]

    userSelect <- table [class_ =: "table table-sm table-hover"] $ do
      thead [] $ showHeader
      tbody [class_ =: "scroll"] $ selectPaged (pure size) offset images showImage selected

    updatePage <- row "justify-content-between align-items-center" $ do
      dec <- button_ [class_ =: "btn btn-outline", enabled_  ~: hasPrev <$> offset ] $ icon "chevron-double-left"
      span [] $ dynText (showPage <$> offset <*> images)  
      inc <- button_ [class_ =: "btn btn-outline", enabled_ ~: hasNext <$> offset <*> images ] $ icon "chevron-double-right"

      return $ leftmost
        [ size <$ domEvent Click inc
        , -size <$ domEvent Click dec]

  command OpenCmd userSelect

    where

      hasPrev i = (i > 0)
      hasNext i images = (i + size < length images)

      --searchImages t  = simpleFilter t

      showPage i images = showText (pageNum i) <> " of " <> showText (pageNum (length images))
      pageNum i = i `Real.div` size + 1

