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
import Data.Time

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


showHeader :: AppBuilder t m => Dynamic t SortKey -> m ()
showHeader sortKey = tr [] $ do
    th [fixed 60] $ text "File"
    key <- th [fixed 40] $ 
      dynText (showSortKey <$> sortKey)

    return ()
      where
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


printCount :: Margins Int -> Text
printCount Margins{..} = T.concat [showText lower, ":", showText middle, ":", showText upper]

printThreshold :: (Float, Int) -> Text
printThreshold (t, n) = showText n <> "@" <> printFloat t

printDate :: UTCTime -> Text
printDate = fromString . formatTime defaultTimeLocale "%Y/%m/%d %H:%M"


showField :: AppBuilder t m =>  Dynamic t (DocName, DocInfo) -> SortKey ->  m ()
showField d key  = case key of
    SortName     -> return () -- dynText name
    SortRandom   -> return ()--dynText (showText . unHash . view #hashedName <$> info)
    SortCategory     -> dynText (showText . view #category <$> info)
    SortModified     -> showModified info

    SortCreation    -> dynText (fromMaybe "" . fmap printDate . view (#image . #creation)  <$> info)

    SortAnnotations   -> dynText (showText . view #numAnnotations <$> info)
    SortDetections    -> dynText (fromMaybe "" . fmap printFloat . detectionScore <$> info)
    SortFrameVariation    -> dynText (fromMaybe "" . fmap printFloat . variationScore <$> info)

    SortCountVariation    -> dynText (fromMaybe "" . fmap showText . countVariation <$> info)

    SortCounts    -> dynText (fromMaybe "" . fmap printCount . totalCounts <$> info)

    SortLossMean        -> dynText (printFloat . view (#training . #lossMean) <$> info)
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
  SelSequential   -> "in sequence"
  SelRandom       -> "pseudo random"
  SelDetections   -> "most detections"
  SelLoss         -> "training error"
  SelCountVariation    -> "count variation"      
  SelFrameVariation    -> "frame variation"      


allSelection :: [(Text, ImageSelection)]
allSelection = withDesc <$>
  [ SelSequential 
  , SelRandom
  , SelDetections 
  , SelLoss
  , SelCountVariation
  , SelFrameVariation
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
  SortFrameVariation   -> "Frame Variation"
  SortCountVariation   -> "Count Variation"
  SortCounts      -> "Detection Count"
  SortCreation   -> "Creation Time"


allFilters :: [(Text, FilterOption)]
allFilters =
  [ ("all",     FilterAll)
  , ("new",     FilterCat CatNew)
  , ("train",   FilterCat CatTrain)
  , ("validate",    FilterCat CatValidate)
  , ("test",    FilterCat CatTest)
  , ("discard", FilterCat CatDiscard)
  , ("edited",  FilterEdited)
  , ("for review",  FilterForReview)
  , ("reviewed",  FilterReviewed)
  ]


allSorts :: [(Text, SortKey)] 
allSorts = 
  [ ("name",     SortName)
  , ("category", SortCategory)
  , ("creation time", SortCreation)
  , ("modified", SortModified)
  , ("annotations", SortAnnotations)
  , ("detection score", SortDetections)
  , ("detection count", SortCounts)
  , ("frame variation", SortFrameVariation)
  , ("count variation", SortCountVariation)
  , ("train error", SortLossRunning)
  , ("random", SortRandom)
  ]


enabled_ :: Attribute Bool
enabled_ = contramap not disabled_

selectOpts :: SortOptions -> (ImageSelection, Bool)
selectOpts SortOptions{selection, revSelection} = (selection, revSelection)

imagesTab :: forall t m. AppBuilder t m => m ()
imagesTab = sidePane $ do
  selected    <- view #docSelected
  images      <- fmap (view #images) <$> view #collection
  prefs       <- view #preferences 

  opts <- holdUniqDyn $ view #sortOptions <$> prefs
  selectionMethod <- holdUniqDyn $ selectOpts <$> opts

  let filtered = filterOpts <$> opts <*> (M.toList <$> images)
      (filtering, invert) = split (view #filtering <$> opts)
  
  groupPane "Filter" $ do
    centreRow $ do
      searched <- div [class_ =: "input-group grow-3"] $ searchView (view #search <$> opts)
      neg <- toggleButtonView ("not-equal-variant", "equal") invert
      filtered <- grow $ selectView allFilters filtering
      
      sortCommand (SetSearch <$> searched)
      sortCommand (SetFilter <$> filtered)
      sortCommand (SetNegFilter <$> neg)    

  let 
    browseTab = do
      imageList 12 opts selected (sortImages <$> (view #sorting  <$> opts) <*> filtered)
        
    selectionTab = column "expand" $ do 
      previewList 12 (fst <$> selectionMethod) (selectNext <$> selectionMethod <*> selected <*> filtered)
      spacer

      row "justify-content-between align-items-center" $ do
        grow $ text "Selection method" 
        imgSelection <- grow $  selectView allSelection (fst <$> selectionMethod)
        sortCommand (SetImageSelection <$> imgSelection)
        
        rev <- toggleButtonView ("sort-descending", "sort-ascending") (snd <$> selectionMethod)     
        sortCommand (SetReverseSelection <$> rev)
    
      return ()

  tabs 0
    [ (browseTab,    tab "image-search"   "Browse")
    , (selectionTab,     tab "sort" "Upcoming")
    ]




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

navButton :: forall t m. Builder t m => Text -> Dynamic t Bool -> m () -> m (Event t ())
navButton tooltip enabled = fmap (domEvent Click) <$> 
  button_ [title_ =: tooltip, class_ =: "btn btn-outline", enabled_  ~: enabled ] 

navControl :: forall t m. Builder t m => Int -> Dynamic t Int -> Dynamic t Int -> m (Event t (Int -> Int))
navControl size numImages offset = row "justify-content-between align-items-center" $ do
    (start, dec) <- buttonGroup $ liftA2 (,)
      (navButton "To beginning" enablePrev $  icon "page-first")
      (navButton "Next page" enablePrev $  icon "chevron-double-left")

    span [] $ dynText (showPage <$> offset <*> numImages)  

    (inc, end) <- buttonGroup $ liftA2 (,)
      (navButton "To end" enableNext $ icon "chevron-double-right")
      (navButton "Previous page" enableNext $ icon "page-last")

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

    showPage i images = showText (pageNum i) <> " of " <> showText (pageNum images)
    pageNum i = i `Real.div` size + 1    

openRequest :: forall t m. AppBuilder t m => Event t DocName -> m ()
openRequest userSelect = do 
  doc <- view #document
  modified <- view #modified
  let saveInfo = liftA2 needsSave (current doc) (current modified)

  command id (attachWith makeCmd saveInfo userSelect)
    where
      makeCmd Nothing k          = OpenCmd k Nothing
      makeCmd (Just save) k  = DialogCmd (SaveDialog save k)

      needsSave :: Maybe Document -> Bool -> Maybe (DocName, ImageCat)
      needsSave (Just doc) True = Just (doc ^. #name, doc ^. #info . #category)
      needsSave _  _ = Nothing

imageList :: forall t m. AppBuilder t m 
          => Int 
          
          -> Dynamic t SortOptions
          -> Dynamic t (Maybe DocName) -> Dynamic t [(DocName, DocInfo)] 
          -> m ()

imageList size  opts selected images = column "expand" $ do   
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

    spacer
    updatePage <- navControl size (length <$> images) offset

  openRequest userSelect
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
      dyn' never $ ffor sorting $ \k -> do
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
categoryIcon' CatTest     = "clipboard-check-outline" 
      
