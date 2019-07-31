{-# LANGUAGE CPP #-}

module Client.Main where

import Annotate.Prelude hiding (div)
import Annotate.Sorting (sortImages, totalCounts)

import Client.Common
import Client.Collection

import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Data.Set as S

import Text.Printf
import Data.Text.Encoding

import qualified Network.URI.Encode as URI

import Data.Default
import Data.FileEmbed

import Control.Monad.Reader
import Control.Monad.State
import Control.Lens (notNullOf, Getting, firstOf, cons)

import Scene.Viewport
import Scene.Controller
import Scene.Types
import Scene.Events
import Scene.Drawing


import Reflex.Classes
import qualified Reflex.Classes as R

import Language.Javascript.JSaddle (JSM)
import Data.Functor.Misc (Const2(..))

import Builder.Html hiding (select)
import qualified Builder.Html as Html
import qualified Builder.Svg as Svg

import Input.Window
import Input.Events

import Client.Widgets
import Client.Class
import Client.Images
import Client.Select

import qualified Client.Dialog as Dialog

import Annotate.Common
import Annotate.Editor

import qualified Reflex.Dom.Main as Main

import Linear.V3 (V3(..))


main :: Text -> JSM ()
main host = Main.mainWidgetWithHead headWidget (bodyWidget host)


headWidget :: forall t m. Builder t m => m ()
headWidget = do

   Html.style [] $ text appStyle
   Html.style [] $ text bootstrap
   Html.style [] $ text icons

  where
    stylesheet url = link_ [href_ =: url, rel_ =: ["stylesheet"], crossorigin_ =: "anonymous"]
    appStyle = decodeUtf8 $(embedFile $ "../html/css/style.css")
    bootstrap = decodeUtf8 $(embedFile $ "../html/css/bootstrap.min.css")
    icons = decodeUtf8 $(embedFile $ "../html/css/materialdesignicons.min.css")





viewControls :: GhcjsBuilder t m => Event t [AppCommand] -> Dynamic t Dim -> m (Dynamic t Viewport)
viewControls cmds dim = do
  windowDim  <- windowDimensions
  rec
    controls <- holdDyn  (1, V2 0 0) (updateView <$> viewCmds <#> current viewport)
    let viewport = makeViewport <$> controls <*> dim <*> windowDim

  return viewport

  where
    viewCmds = oneOf _ViewCmd cmds

    updateView cmd vp = getControls $ case cmd of
      ZoomView zoom pos -> zoomView zoom pos vp
      PanView localOrign page -> panView localOrign page vp

    getControls (Viewport _ _ pan zoom) = (zoom, pan)
    makeViewport (zoom, pan) image window =
        Viewport image window pan zoom

errorDialog :: Builder t m => ErrCode -> m (Event t ())
errorDialog err = Dialog.ok title (Dialog.iconText ("text-danger", "alert-circle") msg)
  where  (title, msg) = errorMessage err

errorMessage :: ErrCode -> (Text, Text)
errorMessage (ErrDecode msg) = ("Decode error", msg)
errorMessage (ErrNotFound _ doc) = ("File not found", "File \"" <> doc <> "\" not found on server.")
errorMessage ErrNotRunning = ("Trainer error", "Trainer process not started.")
errorMessage (ErrTrainer msg) = ("Trainer error", msg)
errorMessage (ErrEnd _) = ("Finished", "No more new images available.")
errorMessage (ErrSubmit msg) = ("Consistency check", msg)


printLog :: (MonadIO m, Show a) => a -> m ()
printLog = liftIO . putStrLn . truncate . show where
  truncate str = if length str > 460
    then take 460 str <> "..."
    else str


isLogged :: ServerMsg -> Bool
isLogged (ServerStatus _) = False
isLogged _ = True

network :: GhcjsBuilder t m => Text -> Event t [ClientMsg] -> m (Event t (), Event t (Maybe Text), Event t ServerMsg, Event t ErrCode)
network host send = do
  socket <- webSocket ("ws://" <> host <> "/clients") $ def
    & webSocketConfig_send  .~ (fmap encode <$> send)

  let (errs, decoded) = splitEither (eitherDecodeStrict <$> socket ^. webSocket_recv)
      errors = leftmost
        [ ErrDecode . fromString <$>  errs
        , preview _ServerError <?> decoded
        ]

  performEvent_ (printLog <$> send)
  performEvent_ (printLog <$> ffilter isLogged decoded)


  return
    ( socket ^. webSocket_open
    , close <$> socket ^. webSocket_close
    , decoded
    , errors)


  where
    close (True,  _, _)       = Nothing
    close (False, _, reason)  = Just reason




  

sceneWidget :: forall t m. (GhcjsAppBuilder t m)
            => Event t [AppCommand]
            -> Event t Document
            -> m (Event t (DMap Shortcut Identity), Dynamic t Action
                 , Dynamic t (Maybe Editor), Dynamic t DocParts)

sceneWidget cmds loaded = do
  
  dim <- holdDyn (800, 600) (view (#info . #image . #size) <$> loaded)
  viewport <- viewControls cmds dim

  rec 
    input <- holdInputs (current viewport) sceneEvents =<< windowInputs element
     
    -- element' <- canvas_ [class_ =: "expand"] 
    -- logEvent (updated $ input ^. #mouse)
    


        -- sceneDefines viewport =<< view #preferences
    
    (action, maybeDoc, sceneEvents, selection, rendering) <- replaceHold' 
      (documentEditor viewport input cmds <$> loaded)

    element <- sceneCanvas (view #window <$> viewport) rendering            
    focusOn element (oneOf _SubmitCmd cmds)


  return (matchShortcuts input, action, maybeDoc, selection)

 
annotationsPatch :: DocumentPatch' -> Maybe (PatchMap AnnotationId Annotation)
annotationsPatch = fmap PatchMap . preview _PatchAnns'

maybePatch :: Maybe DocumentPatch' -> PatchMap AnnotationId Annotation
maybePatch maybePatch = fromMaybe mempty (maybePatch >>= annotationsPatch)


historyEntry :: EditCmd -> HistoryEntry
historyEntry = \case
      DocUndo   -> HistoryUndo
      DocRedo   -> HistoryRedo
      DocEdit e -> HistoryEdit e
 

setClassCommand :: Editor -> (Set AnnotationId, ClassId) -> Maybe EditCmd
setClassCommand doc (selection, classId) =
  guard (S.size selection > 0) $> DocEdit (EditSetClass classId selection)


 
loadedDocument :: Preferences -> Document -> UTCTime -> Editor
loadedDocument prefs Document{..} time = openSession name $ Session 
  { open = case detections of 
      Just d -> if isNew (info ^. #category) then (OpenNew d) else (OpenReview d)
      Nothing -> OpenDisconnected
  , threshold = prefs ^. #thresholds . _1
  , initial = annotations
  , history = []
  , time
  }
 
    

appendHistory :: (UTCTime, HistoryEntry) -> Editor -> Editor
appendHistory e = over (#session . #history) (appendEntry e)

appendEntry ::  (UTCTime, HistoryEntry) -> [(UTCTime, HistoryEntry)] -> [(UTCTime, HistoryEntry)]
appendEntry e@(time, HistoryThreshold t) = \case 
    ((_, HistoryThreshold _):es) -> (e:es)
    es                      -> (e:es)
appendEntry e = cons e



documentEditor :: forall t m. (GhcjsAppBuilder t m)
            => Dynamic t Viewport 
            -> SceneInputs t
            -> Event t [AppCommand]
            -> Document
            -> m (Dynamic t Action, Dynamic t (Maybe Editor)
                 , Event t (DocPart, SceneEvent), Dynamic t DocParts, Dynamic t (Render ()))

documentEditor viewport input cmds document = do

  loadTime <- liftIO getCurrentTime
  preferences  <- view #preferences

  thresholds    <- holdUniqDyn (view #thresholds <$> preferences)

  images0 <- sample =<< (fmap (view #images) <$> view #collection)
  prefs0 <- sample preferences

  let editor0 = loadedDocument prefs0 document loadTime

  rec
    editor <- holdDyn editor0 $ leftmost 
      [ attachWith (flip appendHistory) (current editor) entry
      , edited
      ]


    entry <- withTime $ leftmost 
      [ historyEntry <$> editCmd 
      , HistoryThreshold . fst <$> updated thresholds
      ]

    env   <- ask
              
    let (errors, patches) = splitEither $ flip applyCmd' <$> current editor <@> editCmd
        (patch, edited)   = split patches

        clearCmd      = clearAnnotations <$ oneOf _ClearCmd cmds
        setClassCmd   = attachWithMaybe setClassCommand (current editor) (oneOf _ClassCmd cmds)
        editCmd   = leftmost [oneOf _EditCmd cmds, clearCmd, setClassCmd]
        

  -- Set selection to the last added annotations (including undo/redo etc.)
  selection <- holdDyn mempty $ oneOf _SelectCmd cmds
  logEvent errors

  showing     <- slideShow input (neighbourhood document images0)
  loadedImage <- loadImage showing

  rec
    let pending = applyEdits <$> action <*> editor 
        render = drawAnnotations <$> viewport <*> pure loadedImage <*> pending <*> (view #overlay <$> action)

    
    action <- controller $ Scene
      { input       = input
      , editor      = editor
      , selection   = selection

      , thresholds

      , currentClass = view #currentClass env
      , config       = view #config env
      , shortcut     = view #shortcut env
      , preferences  = view #preferences env
      , viewport     = viewport
      }
  
      
 
  return (action, Just <$> editor, never, selection, render)
      -- withHistory editor entries = Just (editor & #session . #history .~ entries)

 
-- applyEdit' :: Edit -> Editor -> Either EditError (DocumentPatch', Editor)
applyEdit' :: Edit -> StateT Editor (Either EditError) DocumentPatch'
applyEdit' edit = StateT (applyEdit edit)

patches :: Editor -> Action -> Maybe [DocumentPatch']
patches doc action = do
  edits   <- nonEmptyList (action ^. #edit)
  preview _Right 
    (evalStateT (traverse applyEdit' edits) doc)


applyEdits :: Action -> Editor -> Editor
applyEdits action doc = fromMaybe doc $ preview _Right 
    (execStateT (traverse_ applyEdit' (action ^. #edit)) doc)


oneOf :: Reflex t => Getting (First a) s a -> Event t [s] -> Event t a
oneOf getter = fmapMaybe (preview (traverse . getter))

nonEmptyList :: [a] -> Maybe [a]
nonEmptyList = \case
  [] -> Nothing
  xs -> Just xs

someOf :: Reflex t => Getting (First a) s a -> Event t [s] -> Event t [a]
someOf getter cmds = nonEmptyList . fmapMaybe (preview getter) <?> cmds



docFragment :: Text -> Maybe DocName
docFragment = maybeDocName . URI.decodeText . T.drop 1 where
  maybeDocName ""  = Nothing
  maybeDocName t   = Just t

handleLocation :: GhcjsBuilder t m => Event t DocName -> m (Dynamic t (Maybe DocName))
handleLocation userUpdate = mdo

  history <- manageHistory update
  let update     = filterMaybe $ updateHistory <$> current history <@> userUpdate

  return (uriDocument <$> history)


   where
     uriDocument = docFragment . T.pack . view #uriFragment . _historyItem_uri

     updateHistory item k
        | previous == Nothing = Just $ HistoryCommand_ReplaceState $ updateFrag item k
        | previous /= Just k  = Just $ HistoryCommand_PushState $ updateFrag item k
        | otherwise           = Nothing
          where previous = uriDocument item

     updateFrag (HistoryItem state uri) k = HistoryStateUpdate
        { _historyStateUpdate_state = state
        , _historyStateUpdate_title = ""
        , _historyStateUpdate_uri   = Just $ uri & #uriFragment .~ T.unpack ("#" <> URI.encodeText k)
        }



manageDocument :: forall t m. GhcjsBuilder t m
               => Event t () -> Event t ServerMsg
               -> (Event t (), Event t DocName)
               -> m (Dynamic t (Maybe DocName), Event t Document, Event t ClientMsg)
manageDocument hello serverMsg (userNext, userTo) = mdo

  selected <- handleLocation $ leftmost [userTo, view #name <$> validLoad]
  navId    <- R.count request

  loaded <- hold Nothing (Just . view #name <$> validLoad)
  let validLoad = isLatest <$> current navId <??> loadedMsg

      request = leftmost
        [ NavNext <$ userNext
        , NavTo <$> attachWithMaybe needsLoad loaded (updated selected)
        , initialNav <$> current selected <@ hello
        ]

      clientMsg = ClientNav <$> current navId <@> request


  return (selected, validLoad, clientMsg)
    where
      initialNav selected = fromMaybe NavNext (NavTo <$> selected)
      loadedMsg  = preview _ServerDocument <?> serverMsg

      needsLoad k k' = if k /= k' then k' else Nothing
      isLatest latest (navId, doc)  = doc <$ guard (navId == latest - 1)



navigationCmd :: Reflex t => Event t [AppCommand] -> (Event t (), Event t DocName)
navigationCmd cmds = (void $ oneOf _SubmitCmd cmds, fst <$> oneOf _OpenCmd cmds)

  
saveCmd :: Reflex t => Event t [AppCommand] -> Event t SubmitType
saveCmd cmds = leftmost [oneOf _SubmitCmd cmds, snd <?> oneOf _OpenCmd cmds]

submission :: Preferences -> (UTCTime, SubmitType) -> Editor -> Submission
submission prefs (time, method) Editor{..} = Submission
  { name
  , session     = over #history (reverse . cons (time, HistoryClose)) session
  , annotations = thresholdDetections (prefs ^. #thresholds . _1) annotations 
  , method 
  } 
    

withTime :: GhcjsBuilder t m => Event t a -> m (Event t (UTCTime, a))
withTime e = performEvent $ ffor e $ \a -> do
  time <- liftIO getCurrentTime
  return (time, a) 
    

saveDocument :: GhcjsBuilder t m => Dynamic t Preferences -> Dynamic t (Maybe Editor)
                        -> Event t SubmitType -> m (Event t ClientMsg)
saveDocument preferences editor submit = do 
  
  submitAt <- withTime submit 
  return $ fmap ClientSubmit <?> 
    (f <$> current editor <*> current preferences <@> submitAt)

  where
    f doc prefs cat = submission prefs cat <$> doc


connectingDialog :: Builder t m => (Event t (), Event t ()) -> m ()
connectingDialog (opened, closed) = void $ workflow disconnected where
  disconnected = makeWorkflow' $ Dialog.connecting >> return (connected <$ opened)
  connected    = makeWorkflow' $ return (disconnected <$ closed)


filterDocument :: Reflex t => Dynamic t (Maybe Document) -> Event t (DocName, a) -> Event t a
filterDocument d = attachWithMaybe f (current d) where
  f doc (k, a) = a <$ guard (fmap (view #name) doc == Just k)


makeTitle :: Maybe Document -> Bool -> Text
makeTitle maybeDoc modified = "Annotate - " <> fromMaybe "no document" (titleText <$> maybeDoc) where
  titleText Document{name, info} = T.concat 
    [ name
    , "(", showText (info ^. #category), ") "
    ,  if modified then " - modified" else ""
    ]

makeClock :: forall t m. Builder t m => m (Dynamic t UTCTime)
makeClock = do
  time <- liftIO getCurrentTime 
  clock <- clockLossy 10 time
  return (_tickInfo_lastUTC <$> clock)


bodyWidget :: forall t m. GhcjsBuilder t m => Text -> m ()
bodyWidget host = mdo
 
  (opened, closed, serverMsg, serverErrors) <- network host clientMsg
  let clientMsg = toList <$> mergeList
        [ clientNav
        , clientConfig
        , ClientCollection <$ hello
        , ClientPreferences <$> prefsChanged
        , clientSaves
        , ClientCommand <$> oneOf _TrainerCmd cmds
        ]

      clientConfig = ClientConfig <$> oneOf _ConfigCmd cmds

  clientSaves <- saveDocument preferences editor (saveCmd cmds)
  (docSelected, loaded, clientNav) <- manageDocument (void hello) serverMsg (navigationCmd cmds)

  document <- holdDyn Nothing (Just <$> loaded)
  clickOut <- clickAnywhere
  
  let classSelected = oneOf _ClassCmd cmds
      (hello, initPrefs, initConfig, initialStatus)  = split4 (preview _ServerHello <?> serverMsg)

      shortcut   = fan shortcuts
      cancel     = leftmost [select shortcut ShortCancel, clickOut]
      detections = view #instances <$> (filterDocument document $ preview _ServerDetection <?> serverMsg)

      modified = maybe False isModified <$> editor

  trainerStatus <- holdDyn StatusDisconnected $ 
    leftmost [preview _ServerStatus <?> serverMsg, initialStatus]


  config <- holdDyn def $ leftmost [initConfig, preview _ServerConfig <?> serverMsg]
  preferences <- foldDyn updatePrefs def $
    mconcat [pure . SetPrefs <$> initPrefs, someOf _PrefCmd cmds]

  prefsChanged <- debounce 0.5 (updated preferences)
  collection <- holdCollection serverMsg

  currentClass <- foldDyn ($) 0 $ mergeWith (.)
    [ validClass <$> updated config
    , const . snd <$> classSelected
    ]  
  
  clock <- makeClock
  let basePath = "http://" <> host
      env = AppEnv{..}

  setTitle $ makeTitle <$> document <*> modified
    

  connectingDialog (opened, void closed)
  ((shortcuts, action, editor, selection), cmds) <- flip runReaderT env $ runEventWriterT $ do

    runWithClose $ leftmost
      [ runDialog <$> oneOf _DialogCmd cmds
      , errorDialog <$> serverErrors
      ]

    cursorLock action $ do
      div [class_ =: "scene expand"] $
        const <$> sceneWidget cmds loaded
              <*> mainInterface env

  return ()

runDialog :: AppBuilder t m => Dialog -> m (Event t ())
runDialog (ClassDialog selection) = selectClassDialog (Map.keysSet selection)
runDialog (SaveDialog modified next) = saveDialog modified next
runDialog (ErrorDialog code) = errorDialog code


updatePrefs :: [PrefCommand] -> Preferences -> Preferences
updatePrefs cmds = flip (foldr updatePref) cmds where
  updatePref :: PrefCommand -> Preferences -> Preferences
  updatePref (ZoomBrush delta)          = #display . #brushSize %~ clamp (2, 400) . (* wheelZoom delta)
  updatePref (SetOpacity opacity)       = #display . #opacity .~ opacity
  updatePref (SetBorder border)         = #display . #border .~ border
  updatePref (SetGamma gamma)           = #display . #gamma .~ gamma
  updatePref (SetBrightness brightness) = #display . #brightness .~ brightness
  updatePref (SetContrast contrast)     = #display . #contrast .~ contrast

  updatePref (SetInstanceColors b)      = #display . #instanceColours .~ b
  updatePref (SetShowConfidence b)      = #display . #showConfidence .~ b

  updatePref (SetControlSize size)      = #display . #controlSize .~ size
  updatePref (SetFontSize size)      = #display . #fontSize .~ size
  updatePref (ShowClass (classId, shown)) = #display . #hiddenClasses .
    at classId .~  if shown then Just () else Nothing

  updatePref (SetMinThreshold t)  = #detection . #threshold .~ t
  updatePref (SetNms nms)         = #detection . #nms .~ nms
  updatePref (SetDetections d)    = #detection . #detections .~ d

  updatePref (SetThreshold t1)       = #thresholds . _1 .~ t1
  updatePref (SetLowerThreshold t2)  = #thresholds . _2 .~ t2

  updatePref (SetSort sortCmd)  = #sortOptions %~ updateSort sortCmd
  updatePref (SetAutoDetect b)  = #autoDetect .~ b

  updatePref (SetAssignMethod m) = #assignMethod .~ m
  updatePref (SetTrainRatio r)   = #trainRatio .~ r

  updatePref (SetReviewing b)  = #reviewing .~ b
  updatePref (SetPrefs prefs) = const prefs




updateSort :: SortCommand -> SortOptions -> SortOptions
updateSort (SetSortKey k)  = #sorting . _1 .~ k
updateSort (SetReverse b)  = #sorting . _2 .~ b
updateSort (SetFilter opt) = #filtering . _1 .~ opt
updateSort (SetNegFilter opt) = #filtering . _2 .~ opt

updateSort (SetSearch t)   = #search .~ t
updateSort (SetImageSelection s)   = #selection .~ s
updateSort (SetReverseSelection b)   = #revSelection .~ b

  --updatePref _ = id


validClass :: Config -> ClassId -> ClassId
validClass config classId = if (classId `Map.member` classes)
  then classId
  else fromMaybe 0 (minKey classes)
    where classes = view #classes config


cursorLock :: Builder t m => Dynamic t Action -> m a -> m a
cursorLock action child =
  div [class_ =: "expand", draggable_ =: False, style_ ~: cursorStyle <$> cursor] $
    div [classes_ ~: lockClass <$> lock] child
  where
    (cursor, lock) = split (fromMaybe defaultCursor . getLast <$> view #cursor <$> action)
    
    cursorStyle cursor = [("cursor", cursor)]
    lockClass lock = ["expand"] <> ["cursor-lock" | lock]

    defaultCursor = ("default", False)

--holdCollection :: (Reflex t, MonadHold t m, MonadFix m) => Event t ServerMsg -> m (Dynamic t Collection)
holdCollection :: Builder t m => Event t ServerMsg -> m (Dynamic t Collection)
holdCollection serverMsg = mdo
  collection <- holdDyn emptyCollection $ leftmost
    [full, attachWith updateImages (current collection) update]

  return collection

  where
    full = preview _ServerCollection <?> serverMsg

    update = leftmost 
      [ uncurry Map.insert <$> (preview _ServerUpdateInfo <?> serverMsg)
      , updateWith (set #training)   <$> (preview _ServerUpdateTraining <?> serverMsg) 
      , updateWith (set #detections . Just) <$> 
        (preview _ServerUpdateDetections <?> serverMsg)
      ]

    updateImages = flip (over #images)
    updateWith f updates m = Map.union (Map.intersectionWith f updates m) m


percentage :: (Int, Int) -> Text
percentage (i, n) = showText t <> "%" where 
  t = if n == 0 then 0 else 100 * (clamp (0, 100) (fromIntegral i / fromIntegral n))

progressBar :: Builder t m => Dynamic t (Int, Int) -> m ()
progressBar progress = do
  div [class_ =: "progress"] $ 
    div properties $
      dynText $ progressText <$> progress

  where
    (current, total) = split progress
    progressText (i, n) = T.concat [showText i, " of ", showText n]
    width p = [("width", percentage p)]

    properties = [ class_ =: "progress-bar", role_ =: "progressbar", aria_valuenow_ ~: current
                 , aria_valuemin_ =: 0, aria_valuemax_ ~: total, style_ ~: width <$> progress ]

hasKey :: GEq k => k a -> DSum k v -> Bool
hasKey k (k' :=> _) = isJust (geq k k')

trainerTab' :: forall t m. AppBuilder t m => DSum StatusKey (Dynamic t) -> m ()
trainerTab' status = do
  groupPane "Status" $ do   
    showStatus status

  spacer
  buttonGroup $ do
    if isPaused
      then do
        resume <- toolButton  (pure isConnected) "Train" "play" "Start training"
        trainerCommand (UserResume <$ resume)
      else do
        stop    <- toolButton  (pure isConnected) "Pause" "pause" "Pause training"
        trainerCommand (UserPause <$ stop)    

    -- review  <- toolButton   (pure isConnected) "Review" "rotate-left" "Review all"
    detect  <- toolButton   (pure isConnected) "Detect" "auto-fix" "Evaluate detections across all images"
    trainerCommand $ leftmost [UserDetect <$ detect]

  where
    isConnected = not (hasKey DisconnectedKey status)
    isPaused = hasKey PausedKey status

    showStatus :: DSum StatusKey (Dynamic t) -> m ()
    showStatus  (DisconnectedKey :=> _) = void $ Dialog.iconText ("text-danger", "alert-circle") "Disconnected"
    showStatus  (PausedKey       :=> _) = void $ Dialog.iconText ("text-warning", "pause-circle") "Paused"
    showStatus  (TrainingKey     :=> progress) = do
      void $ Dialog.iconRow ("text-success", "play-circle") $  
        dynText (showText . view #activity <$> progress)

      progressBar (view #progress <$> progress)

trainerTab :: forall t m. AppBuilder t m => m () 
trainerTab = sidePane $ do

  status <- view #trainerStatus
  tag <- factorDyn' (trainerKey <$> status)

  dyn_ $ (trainerTab' <$> tag) 



settingsTab :: AppBuilder t m => m ()
settingsTab = sidePane $ do
  (prefs :: Dynamic t Preferences) <- view #preferences
  doc   <- view #editor

  let display = view #display <$> prefs

  groupPane "Interface settings" $ do

    centreRow $ do 

      instanceCols <- grow $ checkboxLabel "instance-cols" "Instance colours" (view #instanceColours <$> display)
      prefCommand (SetInstanceColors <$> instanceCols)

      showConfidence <- grow $ checkboxLabel "show-confidence" "Show confidence" (view #showConfidence <$> display)
      prefCommand (SetShowConfidence <$> showConfidence)

      grow $ return ()

    labelled "Mask opacity" $ do
        inp <- rangePreview printFloat (0.0, 1.0) 0.01 (view #opacity <$> display)
        prefCommand (SetOpacity <$> inp)

    labelled "Border opacity" $ do
      inp <- rangePreview printFloat (0.0, 1.0) 0.01 (view #border <$> display)
      prefCommand (SetBorder <$> inp)
  
    labelled "Control size" $ do
      inp <- rangePreview (printFloat0) (5.0, 50.0) 1 (view #controlSize <$> display)
      prefCommand (SetControlSize <$> inp)

    labelled "Font size" $ do      
      inp <- rangePreview showText (4, 32) 1 (view #fontSize <$> display)
      prefCommand (SetFontSize <$> inp)
    

    return ()


  groupPane "Image adjustment" $ do
    labelled "Gamma" $ do
        inp <- rangePreview printFloat (0.25, 4.0) 0.01 (view #gamma <$> display)
        prefCommand (SetGamma <$> inp)

    labelled "Brightness" $ do
        inp <- rangePreview printFloat (-1.0, 1.0) 0.01 (view #brightness <$> display)
        prefCommand (SetBrightness <$> inp)

    labelled "Contrast" $ do
        inp <- rangePreview printFloat (0.0, 10.0) 0.01 (view #contrast <$> display)
        prefCommand (SetContrast <$> inp)

  groupPane "Detection settings" $ do

    labelled "Non maxima suppression" $ do
      inp <- rangePreview printFloat (0.0, 1.0) 0.01 (view (#detection . #nms) <$> prefs)
      prefCommand (SetNms <$> inp)

    labelled "Minimum threshold" $ do
      inp <- rangePreview printFloat (0.0, 0.5) 0.01 (view (#detection . #threshold) <$> prefs)
      prefCommand (SetMinThreshold <$> inp)

    labelled "Maximum detections" $ do
      inp <- rangePreview (T.pack . show) (0, 1000) 1 (view (#detection . #detections) <$> prefs)
      prefCommand (SetDetections <$> inp)

  groupPane "Active detections" $ do

    let showCounts (V3 confident weak total) = showText confident <> "/" <> showText weak <> "/" <> showText total
        counts = detectionSummary <$> doc <*> (view #thresholds <$> prefs)

    labelled "Confident/weak/total" $ dynText (showCounts <$> counts)

    let (higher, lower) = split (view #thresholds <$> prefs)

    labelled "Visible threshold" $ do
      inp <- rangePreview printFloat (0.0, 1.0) 0.01 higher
      prefCommand (SetThreshold <$> inp)

    labelled "Weak threshold" $ do
      inp <- rangePreview printFloat (0.0, 1.0) 0.01 lower
      prefCommand (SetLowerThreshold <$> inp)      

  groupPane "Image submission" $ do

    labelled "Image assignment" $ do
      inp <- selectView assignOptions (view #assignMethod <$> prefs)
      prefCommand (SetAssignMethod <$> inp)


    labelled "Train to validation" $ do
      let showRatio n = showText n <> " to 1"

      inp <- rangePreview showRatio (1, 9) 1 (view #trainRatio <$> prefs)
      prefCommand (SetTrainRatio <$> inp)

  spacer



assignOptions :: [(Text, AssignmentMethod)]
assignOptions = 
  [ ("auto", AssignAuto)
  , ("train", AssignCat CatTrain)
  , ("validate", AssignCat CatValidate)
  , ("test", AssignCat CatTest)
  ]

detectionSummary :: Maybe Editor -> (Float, Float) -> V3 Int
detectionSummary Nothing    _         = V3 0 0 0
detectionSummary (Just doc) (t1, t2) = sum $ summary <$> detections where
  summary Detection{..} = V3 (fromEnum (confidence > t1)) (fromEnum (confidence > t2)) 1
  detections = Map.mapMaybe getDetection (doc ^. #annotations)

getDetection :: Annotation -> Maybe Detection
getDetection Annotation{detection} = snd <$> detection


sidebar :: forall t m. AppBuilder t m => m ()
sidebar = mdo

  isOpen <- div [classList ["enable-cursor sidebar bg-white p-2", swapping ("closed", "open") isOpen]] $ do
    isOpen <- toggler

    tabs 0
      [ (classesTab,    tab "tag-multiple"   "Classes")
      , (imagesTab,     tab "folder-multiple-image" "Images")
      , (settingsTab,  tab "settings"  "Settings")
      , (trainerTab,  tab "brain"  "Trainer")
      ]

    return isOpen
  return ()

    where
      toggler = mdo
        e <- a_ [class_ =: "toggler p-2"] $
          icon ((def :: IconConfig t)  & #name .~ Dyn (swapping ("chevron-right", "chevron-left") isOpen))
        isOpen <- toggle False (domEvent Click e)
        return isOpen


-- Main interface


selectedClass :: Reflex t => AppEnv t -> Dynamic t (Maybe ClassConfig)
selectedClass AppEnv{config, currentClass} = Map.lookup <$> currentClass <*> fmap (view #classes) config


fileInfo :: forall t m. AppBuilder t m => Dynamic t Bool -> Document ->  m ()
fileInfo isModified Document{name, info} = do 
  centreRow $ do
    div [ hidden_ ~: not <$>  isModified ] $ tinyIcon "lead-pencil"
    h6 [ class_ =: "text-white font-weight-bold m-1 mt-1 " ] $ text name
    
  
  centreRow $ do
    feild [tinyIcon $ Static (categoryIcon' category), text (showText category)]
    feild [tinyIcon "shape", text (showText numAnnotations)]

    forM_ (info ^. #image . #creation) $ \count -> 
      fixed "200px" [tinyIcon "camera-outline", text (printDate count)]

    forM_ (totalCounts info) $ \count -> 
      feild [tinyIcon "auto-fix", text (printCount count)]
    

    div [ hidden_ =: isNothing modified ] $
      fixed "200px" [tinyIcon "lead-pencil", showModified (pure info)]
    
    -- feild $ do

  where
    DocInfo{..} = info
    feild = fixed "80px"

    fixed width = div [class_ =: "text-light", style_ =: [("width", width)]] . sequence_

submitButtons :: forall t m. AppBuilder t m => ImageCat ->  m ()
submitButtons cat = if isNew cat then new else confirm
  where
   
    new = buttonGroup $ do
      category <- categorySelect True

      discard     <- toolButton' "Discard" (categoryIcon CatDiscard)  "Mark image as discarded"
      submit      <- toolButton' "Submit " "check" "Submit image"

      command SubmitCmd $ 
        leftmost [ SubmitDiscard <$ discard, SubmitNew <$ submit ]

    confirm = buttonGroup $ do
      category <- categorySelect False

      discard  <- toolButton' "Discard" (categoryIcon CatDiscard)  "Mark image as discarded"
      confirm  <- toolButton' "Confirm" "check" "Confirm review" 

      command SubmitCmd $ 
        leftmost [  SubmitDiscard <$ discard, SubmitConfirm . Just <$> current category `tag` confirm  ]   
        
    categorySelect hidden = selectOption' 
      [class_ =: "custom-select bg-secondary border-0", style_ =: [("height", "100%"), ("visibility", if hidden then "hidden" else "visible")]] 
      commitCategories cat never

navButtons :: forall t m. AppBuilder t m => DocName ->  m ()
navButtons k = buttonGroup $ do
    prev     <- toolButton' "Prev" "chevron-left" "View previous image"
    -- reload   <- toolButton' "Reload" "reload" "Reload current image"
    next     <- toolButton' "Next" "chevron-right" "View next image"

    images <- fmap (view #images) <$> view #collection

    let request = leftmost 
          [ nextFrame k <?> current images `tag` next
          -- , const k <$> reload
          , prevFrame k <?> current images `tag` prev
          ]

    logEvent request
    openRequest request


commitCategories :: [(Text, ImageCat)]
commitCategories = f <$> [CatTrain, CatValidate, CatTest]
    where f cat = (showText cat, cat)


mainInterface :: forall t m. AppBuilder t m => AppEnv t -> m ()
mainInterface AppEnv{shortcut, document, editor, modified, preferences, collection} = row "expand  disable-cursor" $ do
  sidebar
  column "expand" $
    sequence_ [header, spacer, footer]

  where
    header = buttonRow $ do
      clickSelect <- asks selectedClass >>= classToolButton
      selection   <- view #selection
      command (DialogCmd . ClassDialog) $
        current selection `tag` leftmost [clickSelect, select shortcut ShortClass]

      spacer

      -- detect <- toolButton docOpen "Detect" "auto-fix" "Detect annotations using current trained model"
      -- command (const DetectCmd) detect

      review <- toolButtonToggle docOpen (view #reviewing <$> preferences) "Review" "magnify" "Toggle review mode"
      prefCommand (SetReviewing <$> review)

      buttonGroup $ do
        docCommand  (const DocUndo)  =<< toolButton canUndo "Undo" "undo" "Undo last edit"
        docCommand  (const DocRedo)  =<< toolButton canRedo "Redo" "redo" "Re-do last undo"
        command     (const ClearCmd) =<< toolButton' "Clear" "eraser" "Clear all annotations"

    fromDocument a f = fromMaybe a . fmap f <$> editor

    canUndo = fromDocument False (not . null . view #undos)
    canRedo = fromDocument False (not . null . view #redos)

    footer = buttonRow $ do
      dyn_ (traverse_ footer' <$> document)
      -- buttonGroup $ do

      --   prev   <- toolButton' "Prev" "chevron-left" "View previous frame"
      --   next   <- toolButton' "Next" "chevron-right" "View next frame"

      --   command NavCmd $ leftmost [ NavBackward <$ prev, NavForward <$ next ]

      --   return ()
       
    footer' document = do 
      div [ class_ =: "grow" ] $  
        fileInfo modified document 

      navButtons (document ^. #name)
      submitButtons (document ^. #info . #category)


    buttonRow = row "p-0 spacing-4"
    docOpen = isJust <$> editor


