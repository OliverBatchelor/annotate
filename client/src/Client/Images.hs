module Client.Images where

import Annotate.Prelude hiding (div)
import qualified Annotate.Prelude as Prelude

import Annotate.Common hiding (label)
import Annotate.Sorting

import Client.Common
import Client.Widgets
import Client.Select

import Data.Ord (comparing, Ordering(..))
import Data.List (sortBy, findIndex)
import Data.Align (padZipWith)

import qualified GHC.Real as Real

import Builder.Html
import qualified Builder.Html as Html

import qualified Data.Map as M
import qualified Data.Text as T

import Data.Time.Format.Human 

fixed :: Reflex t => Int -> Property t
fixed n = style_ =: 
  [ ("width", showText n <> "%"), ("vertical-align", "middle") ]
  -- , ("padding-top", "0"), ("padding-bottom", "0"), ("border", "0")]


upcomingHeader :: AppBuilder t m => Dynamic t ImageSelection -> m ()
upcomingHeader sel = tr [] $ do
    th [fixed 60] $ text "File"
    th [fixed 40] $ dynText (selectionDesc <$> sel)
  

selectHeader :: AppBuilder t m => Dynamic t (SortKey, Bool) -> m ()
selectHeader sorting = tr [] $ do
    th [fixed 60] $ text "File"
    key <- th [fixed 40] $ 
      selectView allSorts sortKey

    rev <- toggleButtonView ("sort-descending", "sort-ascending") reversed

    sortCommand (SetSortKey <$> key)
    sortCommand (SetReverse <$> rev)      

    return ()
      where

        (sortKey, reversed) = split sorting
        width n = style_ =: [("width", showText n <> "%")]


showHeader :: AppBuilder t m => Dynamic t (SortKey, Bool) -> m ()
showHeader sorting = tr [] $ do
    th [fixed 60] $ text "File"
    key <- th [fixed 40] $ 
      dynText (showSortKey <$> sortKey)

    return ()
      where
        (sortKey, reversed) = split sorting
        width n = style_ =: [("width", showText n <> "%")]        
  
approxLocale :: HumanTimeLocale
approxLocale = defaultHumanTimeLocale 
  { justNow = "within a minute" 
  , secondsAgo = const (const "within a minute")
  }

showTime :: UTCTime -> Maybe UTCTime -> Text
showTime current = fromMaybe "never" . fmap (fromString . humanReadableTimeI18N' approxLocale current)

showModified :: AppBuilder t m => Dynamic t DocInfo -> m ()
showModified info = do
  now <- view #clock
  dynText (showTime <$> now <*> (view #modified <$> info))


showField :: AppBuilder t m => Dynamic t (DocName, DocInfo) -> SortKey ->  m ()
showField d key  = case key of
    SortName     -> return () -- dynText name
    SortRandom   -> return ()--dynText (showText . unHash . view #hashedName <$> info)
    SortCategory     -> dynText (showText . view #category <$> info)
    SortModified     -> showModified info

    SortAnnotations  -> dynText (showText . view #numAnnotations <$> info)
    SortDetections  -> dynText (fromMaybe "" . fmap printFloat . detectionScore <$> info)

    SortLossMean    -> dynText (printFloat . view (#training . #lossMean) <$> info)
    SortLossRunning     -> dynText (printFloat . view (#training . #lossRunning) <$> info)
    where
      (name, info) = split d


showImage :: AppBuilder t m =>  SortKey ->  Dynamic t (Maybe (DocName, DocInfo)) -> Dynamic t Bool -> m (Event t DocName)
showImage sortKey maybeRow selected = do
  e <- tr_ [rowClasses] $ do 
    td [fixed 60] $ centreRow $ do
      smallIcon (Dyn $ categoryIcon' . view #category <$> info)
      span [class_ =: "pt-1"] $ dynText name
      preload name

    td [fixed 40] $ do 
      showField imageInfo sortKey
    
  return (current name `tag` domEvent Click e)

    where
      rowClasses = classList [gated "table-active" selected, gated "invisible" (isNothing <$> maybeRow)]     
      imageInfo = fromMaybe def <$> maybeRow
      (name, info) = split imageInfo
      
selectionDesc :: ImageSelection -> Text
selectionDesc = \case 
  SelSequential False -> "forwards"
  SelSequential True  -> "backwards"
  SelRandom           -> "random"
  SelDetections True  -> "most detections"
  SelLoss             -> "training error"
  SelDetections False -> "least detections"
  

allSelection :: [(Text, ImageSelection)]
allSelection = withDesc <$>
  [ SelSequential False
  , SelSequential True
  , SelRandom
  , SelDetections True
  , SelLoss
  ] where withDesc s = (selectionDesc s, s)

showSortKey :: SortKey -> Text
showSortKey = \case 
  SortCategory    -> "Category"
  SortAnnotations -> "Annotations"
  SortName        -> "Name"
  SortModified    -> "Modified"
  SortRandom      -> "Random"
  SortDetections  -> "Detection Score"
  SortLossMean    -> "Mean Loss"
  SortLossRunning -> "Running Loss"

allFilters :: [(Text, FilterOption)]
allFilters =
  [ ("all",     FilterAll)
  , ("new",     FilterCat CatNew)
  , ("train",   FilterCat CatTrain)
  , ("validate",    FilterCat CatValidate)
  , ("discard", FilterCat CatDiscard)
  , ("edited",  FilterEdited)
  , ("for review",  FilterForReview)
  , ("reviewed",  FilterReviewed)
  ]


allSorts :: [(Text, SortKey)] 
allSorts = 
  [ ("name",     SortName)
  , ("category", SortCategory)
  , ("modified", SortModified)
  , ("annotations", SortAnnotations)
  , ("detection score", SortDetections)
  , ("train error", SortLossRunning)
  , ("random", SortRandom)
  ]


enabled_ :: Attribute Bool
enabled_ = contramap not disabled_


imagesTab :: forall t m. AppBuilder t m => m ()
imagesTab = sidePane $ do
  selected    <- view #docSelected
  images      <- fmap (view #images) <$> view #collection
  prefs       <- view #preferences 

  let opts  = (view #sortOptions <$> prefs :: Dynamic t SortOptions)
      selectionMethod = (view #selection <$> opts)
      filtered = filterOpts <$> opts <*> (M.toList <$> images)
      (filtering, invert) = split (view #filtering <$> opts)
  
  groupPane "Filter" $ do
    centreRow $ do
      searched <- div [class_ =: "input-group grow-3"] $ searchView (view #search <$> opts)
      neg <- toggleButtonView ("not-equal-variant", "equal") invert
      filtered <- grow $ selectView allFilters filtering
      
      sortCommand (SetSearch <$> searched)
      sortCommand (SetFilter <$> filtered)
      sortCommand (SetNegFilter <$> neg)    

  groupPane "Browse" $ do
    imageList 10 opts selected (sortImages <$> (view #sorting  <$> opts) <*> filtered)
    spacer

  groupPane "Upcoming" $ do
    labelled "Selection method" $ do
      imgSelection <- selectView allSelection selectionMethod
      sortCommand (SetImageSelection <$> imgSelection)

    previewList 2 selectionMethod (sortForSelection <$> selectionMethod <*> filtered)


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


navControl :: forall t m. Builder t m => Int -> Dynamic t Int -> Dynamic t Int -> m (Event t (Int -> Int))
navControl size numImages offset = row "justify-content-between align-items-center" $ do
    (start, dec) <- buttonGroup $ liftA2 (,)
      (navButton enablePrev $  icon "page-first")
      (navButton enablePrev $  icon "chevron-double-left")

    span [] $ dynText (showPage <$> offset <*> numImages)  

    (inc, end) <- buttonGroup $ liftA2 (,)
      (navButton enableNext $ icon "chevron-double-right")
      (navButton enableNext $ icon "page-last")

    return $ leftmost
      [ (+size) <$ inc
      , (subtract size) <$  dec
      , const 0 <$ start 
      , toEnd <$> current numImages `tag` end
      ]

  where
    toEnd n = const (n - 1)

    enablePrev = (> 0) <$> offset
    enableNext = hasNext <$> offset <*> numImages
      where hasNext i n = (i + size < n)

    navButton enabled = fmap (domEvent Click) <$> 
      button_ [class_ =: "btn btn-outline", enabled_  ~: enabled ]     

    showPage i images = showText (pageNum i) <> " of " <> showText (pageNum images)
    pageNum i = i `Real.div` size + 1    
  

imageList :: forall t m. AppBuilder t m 
          => Int 
          
          -> Dynamic t SortOptions
          -> Dynamic t (Maybe DocName) -> Dynamic t [(DocName, DocInfo)] 
          -> m ()

imageList size  opts selected images = do   
  rec
    offset <- holdDyn 0 $ leftmost
      [ updated (findOffset size <$> images <*> selected)
      , attachWith (&) (current offset) updatePage
      ]
   
    userSelect <- table [class_ =: "table table-sm table-hover m-0"] $ do
      thead [] $ selectHeader sorting

      tbody [class_ =: "scroll"] $ 
        dyn' never $ ffor sorting $ \(k, _) -> 
          selectPaged (pure size) offset images (showImage k) selected

    updatePage <- navControl size (length <$> images) offset
  command OpenCmd userSelect
    where
      sorting = view #sorting <$> opts

padNothing :: Int -> [a] -> [Maybe a]
padNothing n xs = padZipWith (liftA2 const) (take n xs) [1..n]

previewList :: forall t m. AppBuilder t m 
  => Int 
  -> Dynamic t ImageSelection -> Dynamic t [(DocName, DocInfo)] 
  -> m ()
previewList size  selectionMethod images = void $ do   
  table [class_ =: "table table-sm m-0"] $ do
    thead [] $ showHeader sorting

    tbody [] $ 
      dyn' never $ ffor sorting $ \(k, _) -> do
        dynList (showImage' k) (padNothing size <$> images)

      where 
        sorting = selectionKey <$> selectionMethod
        showImage' k _ info = void $ showImage k info (pure False) 



categoryIcon :: forall t. Reflex t => ImageCat -> IconConfig t
categoryIcon cat = (def :: IconConfig t) & #name .~ Static (categoryIcon' cat)

categoryIcon' :: ImageCat -> Text
categoryIcon' CatDiscard   = "delete-empty"
categoryIcon' CatValidate  = "clipboard-check" 
categoryIcon' CatNew       = "image-outline"
categoryIcon' CatTrain     = "book-open-page-variant" 
      