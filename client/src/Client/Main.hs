{-# LANGUAGE CPP #-}

module Client.Main where

import Annotate.Prelude hiding (div)
import Client.Common

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Printf
import Data.Text.Encoding

import qualified Data.List as L
import qualified Network.URI.Encode as URI

import Data.Default
import Data.FileEmbed

import Control.Monad.Reader
import Control.Lens (notNullOf, Getting, firstOf, cons)

import Scene.Viewport
import Scene.View
import Scene.Types
import Scene.Events

import Scene.Events

import Reflex.Classes
import qualified Reflex.Classes as R

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
import Annotate.Document

import Language.Javascript.JSaddle
import qualified Reflex.Dom.Main as Main

import qualified Web.KeyCode as Key

import Stitch
import Stitch.Combinators



main :: JSM ()
main = Main.mainWidgetWithHead' (headWidget, bodyWidget)



headWidget :: forall t m. GhcjsBuilder t m => AppEnv t -> m Text
headWidget env = do

#ifdef DEBUG
   let host = "localhost:3000"
#else
   host <- getLocationHost
#endif

   Html.style [] $ text appStyle
   Html.style [] $ text bootstrap
   Html.style [] $ text icons

   Html.style [] $ dynText (prefsCss <$> env ^. #preferences)


   return host

  where
    stylesheet url = link_ [href_ =: url, rel_ =: ["stylesheet"], crossorigin_ =: "anonymous"]

    appStyle = decodeUtf8 $(embedFile $ "../html/css/style.css")
    bootstrap = decodeUtf8 $(embedFile $ "../html/css/bootstrap.min.css")
    icons = decodeUtf8 $(embedFile $ "../html/css/materialdesignicons.min.css")

prefsCss :: Preferences -> Text
prefsCss Preferences{opacity, border} = renderCSS $ do
  ".shape" ? do
    "stroke-opacity" .= showText border
    "fill-opacity" .= showText opacity


nonEmptyList :: [a] -> Maybe [a]
nonEmptyList = \case
  [] -> Nothing
  xs -> Just xs


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
        Viewport (fromDim image) (fromDim window) pan zoom

errorDialog :: Builder t m => ErrCode -> m (Event t ())
errorDialog err = Dialog.ok title (Dialog.iconText ("text-danger", "alert-circle") msg)
  where  (title, msg) = errorMessage err

errorMessage :: ErrCode -> (Text, Text)
errorMessage (ErrDecode msg) = ("Decode error", msg)
errorMessage (ErrNotFound _ doc) = ("File not found", "File \"" <> doc <> "\" not found on server.")
errorMessage ErrNotRunning = ("Trainer error", "Trainer process not started.")
errorMessage (ErrTrainer msg) = ("Trainer error", msg)
errorMessage (ErrEnd _) = ("Finished", "No more new images available.")


printLog :: (MonadIO m, Show a) => a -> m ()
printLog = liftIO . putStrLn . truncate . show where
  truncate str = if length str > 460
    then take 460 str <> "..."
    else str



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
  -- performEvent_ (printLog <$> decoded)
  performEvent_ (printLog <$> decoded)


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
                 , Dynamic t (Maybe EditorDocument), Dynamic t DocParts)

sceneWidget cmds loaded = do

  dim <- holdDyn (800, 600) (view (#info . #imageSize) <$> loaded)
  viewport <- viewControls cmds dim

  rec
    input <- holdInputs (current viewport) sceneEvents =<< windowInputs element
    let (action, maybeDoc, sceneEvents, selection) = r


    (element, r) <- Svg.svg' [class_ =: "expand enable-cursor", version_ =: "2.0"] $ do
        sceneDefines viewport =<< view #preferences

        inViewport viewport $ replaceHold'
            (documentEditor input cmds  <$> loaded)

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
 

setClassCommand :: EditorDocument -> (Set AnnotationId, ClassId) -> Maybe EditCmd
setClassCommand doc (selection, classId) =
  guard (S.size selection > 0) $> DocEdit (EditSetClass classId selection)

openNew :: UTCTime -> [Detection] -> EditorDocument -> EditorDocument
openNew time detections doc = doc
  & #history      %~  mappend [(time, HistoryOpenNew detections)]
  & #annotations  .~ fromDetections 0 detections
 
openReview :: UTCTime -> [Detection] -> EditorDocument -> EditorDocument
openReview time detections doc = doc
  & #history      %~  mappend [(time, HistoryOpenReview detections)]

openDocument :: UTCTime -> EditorDocument -> EditorDocument
openDocument time doc = doc
  & #history      %~  mappend [(time, HistoryOpen)]

loadedDocument :: Document -> UTCTime -> EditorDocument
loadedDocument doc@Document{info, annotations, detections} time = maybeOpen (editorDocument doc) where

    f = if info ^. #category == CatNew && null annotations
      then openNew time
      else openReview time

    maybeOpen = fromMaybe (openDocument time) (f . view #detections <$> detections)
    


documentEditor :: forall t m. (GhcjsAppBuilder t m)
            => SceneInputs t
            -> Event t [AppCommand]
            -> Document
            -> m (Dynamic t Action, Dynamic t (Maybe EditorDocument)
                 , Event t (DocPart, SceneEvent), Dynamic t DocParts)

documentEditor input cmds loaded'  = do

  rec
    document <- holdDyn loaded edited
    env <- ask
    time <- liftIO getCurrentTime

        
    let (patch, edited) = split (attachWithMaybe (flip applyCmd') (current document) editCmd)
        loaded = loadedDocument loaded' time

        clearCmd      = clearAnnotations <$ oneOf _ClearCmd cmds
        setClassCmd   = attachWithMaybe setClassCommand (current document) (oneOf _ClassCmd cmds)
        detectionsCmd = DocEdit . EditDetection <$> env ^. #detections

        editCmd   = leftmost [oneOf _EditCmd cmds, clearCmd, setClassCmd,  detectionsCmd]

  -- Set selection to the last added annotations (including undo/redo etc.)
  selection <- holdDyn mempty $ oneOf _SelectCmd cmds

  entry <- withTime (historyEntry <$> editCmd)
  history <- foldDyn (:) [] entry

  rec
    annotations  <- holdIncremental (loaded ^. #annotations) (annotationsPatch <?> patch)
    let patched = patchIncremental annotations (maybePatch <$> pending)
        pending = pendingEdit <$> document <*> action

    -- logEvent (updated (pending <$> document <*> action))

    (action, sceneEvents) <- sceneView $ Scene
      { image    = (loaded ^. #name, loaded ^. #info . #imageSize)
      -- , viewport = viewport
      , input       = input
      , document    = document
      , currentEdit = maybePatchDocument <$> pending <*> document

      , selection   = selection
      , annotations = patched

      , currentClass = view #currentClass env
      , config       = view #config env
      , shortcut     = view #shortcut env
      , preferences  = view #preferences env
      }

  return (action, (withHistory <$> document <*> history), sceneEvents, selection)
    where 
      withHistory doc entries = Just (doc & over #history (entries <>))



pendingEdit :: EditorDocument -> Action -> Maybe DocumentPatch'
pendingEdit doc action = do
  edit <- action ^. #edit
  fst <$> applyEdit edit doc


oneOf :: Reflex t => Getting (First a) s a -> Event t [s] -> Event t a
oneOf getter = fmapMaybe (preview (traverse . getter))

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
  navId    <- count request

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


data SaveCommand = AutoSave | SubmitSave ImageCat 
  deriving (Show, Eq, Generic)


navigationCmd :: Reflex t => Event t [AppCommand] -> (Event t (), Event t DocName)
navigationCmd cmds = (void $ oneOf _SubmitCmd cmds, oneOf _OpenCmd cmds)

  
saveCmd :: Reflex t => Event t [AppCommand] -> Dynamic t (Maybe EditorDocument) -> Event t SaveCommand
saveCmd cmds doc = leftmost [submit, autoSave] where
  submit = SubmitSave <$> oneOf _SubmitCmd cmds
  autoSave = (maybe False isModified <$> current doc) `gate` (AutoSave <$ oneOf _OpenCmd cmds)

    
   

save :: Preferences -> (UTCTime, SaveCommand) -> EditorDocument -> Document
save prefs (time, cmd) doc = toDocument (prefs ^. #threshold) $ doc
      & setCat cmd
      & #history %~ cons (time, HistoryClose)

  where 
    setCat AutoSave         = id
    setCat (SubmitSave cat) = #info . #category .~ cat

withTime :: GhcjsBuilder t m => Event t a -> m (Event t (UTCTime, a))
withTime e = performEvent $ ffor e $ \a -> do
  time <- liftIO getCurrentTime
  return (time, a) 
    

saveDocument :: GhcjsBuilder t m => Dynamic t Preferences -> Dynamic t (Maybe EditorDocument)
                        -> Event t SaveCommand -> m (Event t ClientMsg)
saveDocument preferences document submit = do 
  
  submitAt <- withTime submit 
  return $ fmap ClientSubmit <?> 
    (f <$> current document <*> current preferences <@> submitAt)

  where
    f doc prefs cat = save prefs cat <$> doc


connectingDialog :: Builder t m => (Event t (), Event t ()) -> m ()
connectingDialog (opened, closed) = void $ workflow disconnected where
  disconnected = workflow' $ Dialog.connecting >> return (connected <$ opened)
  connected    = workflow' $ return (disconnected <$ closed)

clientDetect :: Reflex t => Dynamic t (Maybe DocName) -> Event t [AppCommand] -> Event t ClientMsg
clientDetect docSelected cmds = fmap ClientDetect <?> (current docSelected `tag` oneOf _DetectCmd cmds)


filterDocument :: Reflex t => Dynamic t (Maybe EditorDocument) -> Event t (DocName, a) -> Event t a
filterDocument d = attachWithMaybe f (current d) where
  f doc (k, a) = a <$ guard (fmap (view #name) doc == Just k)


bodyWidget :: forall t m. GhcjsBuilder t m => Text -> m (AppEnv t)
bodyWidget host = mdo
 
  (opened, closed, serverMsg, serverErrors) <- network host clientMsg
  let clientMsg = toList <$> mergeList
        [ clientDetect docSelected cmds
        , clientNav
        , clientConfig
        , ClientCollection <$ hello
        , ClientPreferences <$> prefsChanged
        , clientSaves
        , ClientCommand <$> oneOf _TrainerCmd cmds
        ]

      clientConfig = ClientConfig <$> oneOf _ConfigCmd cmds

  clientSaves <- saveDocument preferences document (saveCmd cmds document)
  (docSelected, loaded, clientNav) <- manageDocument (void hello) serverMsg (navigationCmd cmds)

  
  let classSelected = oneOf _ClassCmd cmds
      (hello, initPrefs, initConfig, initialStatus)  = split4 (preview _ServerHello <?> serverMsg)

      shortcut   = fan shortcuts
      detections = view #detections <$> (filterDocument document $ preview _ServerDetection <?> serverMsg)

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


  let basePath = "http://" <> host
      env = AppEnv{..}

  setTitle $ ffor docSelected $ \doc ->
    "Annotate - " <> fromMaybe ("no document") doc

  connectingDialog (opened, void closed)
  ((shortcuts, action, document, selection), cmds) <- flip runReaderT env $ runEventWriterT $ do

    runWithClose $ leftmost
      [ runDialog <$> oneOf _DialogCmd cmds
      , errorDialog <$> serverErrors
      ]


    cursorLock action $ do
      div [class_ =: "scene expand"] $
        const <$> sceneWidget cmds loaded
              <*> overlay shortcut document preferences
  return env

runDialog :: AppBuilder t m => Dialog -> m (Event t ())
runDialog (ClassDialog selection) = selectClassDialog (M.keysSet selection)
runDialog (ErrorDialog code) = errorDialog code



updatePrefs :: [PrefCommand] -> Preferences -> Preferences
updatePrefs cmds = flip (foldr updatePref) cmds where
  updatePref :: PrefCommand -> Preferences -> Preferences
  updatePref (ZoomBrush delta) = #brushSize %~ clamp (2, 400) . (* wheelZoom delta)
  updatePref (SetOpacity opacity) = #opacity .~ opacity
  updatePref (SetBorder border) = #border .~ border
  updatePref (SetGamma gamma) = #gamma .~ gamma
  updatePref (SetBrightness brightness) = #brightness .~ brightness
  updatePref (SetContrast contrast) = #contrast .~ contrast

  updatePref (SetInstanceColors b) = #instanceColours .~ b
  updatePref (SetControlSize size) = #controlSize .~ size
  updatePref (ShowClass (classId, shown)) = #hiddenClasses .
    at classId .~  if shown then Just () else Nothing

  updatePref (SetMinThreshold t)  = #detection . #threshold .~ t
  updatePref (SetNms nms)         = #detection . #nms .~ nms
  updatePref (SetDetections d)    = #detection . #detections .~ d

  updatePref (SetThreshold t)   = #threshold .~ t
  updatePref (SetMargin m)      = #margin .~ m

  updatePref (SetPrefs prefs) = const prefs

  updatePref (SetSort sortCmd)  = #sortOptions %~ updateSort sortCmd
  updatePref (SetAutoDetect b)  = #autoDetect .~ b


updateSort :: SortCommand -> SortOptions -> SortOptions
updateSort (SetSortKey k)  = #sortKey .~ k
updateSort (SetReverse b)  = #reversed .~ b
updateSort (SetFilter opt) = #filtering .~ opt
updateSort (SetSearch t)   = #search .~ t

  --updatePref _ = id


validClass :: Config -> ClassId -> ClassId
validClass config classId = if (classId `M.member` classes)
  then classId
  else fromMaybe 0 (minKey classes)
    where classes = view #classes config


cursorLock :: Builder t m => Dynamic t Action -> m a -> m a
cursorLock action child =
  div [class_ =: "expand", draggable_ =: False, style_ ~: cursorStyle <$> action] $
    div [classes_ ~: lockClass <$> action] child
  where
    cursorStyle Action{..} = [("cursor", cursor)]
    lockClass Action{..} = ["expand"] <> ["cursor-lock" | lock]



holdCollection :: (Reflex t, MonadHold t m, MonadFix m) => Event t ServerMsg -> m (Dynamic t Collection)
holdCollection serverMsg = mdo
  collection <- holdDyn emptyCollection $ leftmost
    [ full
    , attachWithMaybe applyUpdate (current collection) update
    ]
  return collection

  where
    full = preview _ServerCollection <?> serverMsg
    update = preview _ServerUpdateInfo <?> serverMsg

    applyUpdate collection (k, info) = case (== info) <$> M.lookup k (collection ^. #images) of
      Just True -> Nothing
      _         -> Just $ over #images (M.insert k info) collection


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

    review  <- toolButton   (pure isConnected) "Review" "rotate-left" "Review all"
    detect  <- toolButton   (pure isConnected) "Detect" "auto-fix" "Detect new"

    trainerCommand $ leftmost [UserReview <$ review, UserDetect <$ detect]
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
trainerTab = sidePane $ void $ do

  status <- view #trainerStatus
  tag <- factorDyn' (trainerKey <$> status)

  dyn $ (trainerTab' <$> tag) 



settingsTab :: AppBuilder t m => m ()
settingsTab = sidePane $ do
  prefs <- view #preferences
  doc   <- view #document

  groupPane "Interface settings" $ do

    instanceCols <- checkboxLabel "instance-cols" "Instance colours" (view #instanceColours <$> prefs)
    prefCommand (SetInstanceColors <$> instanceCols)

    labelled "Mask opacity" $ do
        inp <- rangePreview printFloat (0.0, 1.0) 0.01 (view #opacity <$> prefs)
        prefCommand (SetOpacity <$> inp)

    labelled "Border opacity" $ do
      inp <- rangePreview printFloat (0.0, 1.0) 0.01 (view #border <$> prefs)
      prefCommand (SetBorder <$> inp)
  

    labelled "Control size" $ do
      inp <- rangePreview (printFloat0) (5.0, 50.0) 1 (view #controlSize <$> prefs)
      prefCommand (SetControlSize <$> inp)

    return ()


  groupPane "Image adjustment" $ do
    labelled "Gamma" $ do
        inp <- rangePreview printFloat (0.25, 4.0) 0.01 (view #gamma <$> prefs)
        prefCommand (SetGamma <$> inp)

    labelled "Brightness" $ do
        inp <- rangePreview printFloat (-1.0, 1.0) 0.01 (view #brightness <$> prefs)
        prefCommand (SetBrightness <$> inp)

    labelled "Contrast" $ do
        inp <- rangePreview printFloat (0.0, 10.0) 0.01 (view #contrast <$> prefs)
        prefCommand (SetContrast <$> inp)

  groupPane "Detection settings" $ do

    labelled "Non maxima suppression" $ do
      inp <- rangePreview printFloat (0.0, 1.0) 0.01 (view (#detection . #nms) <$> prefs)
      prefCommand (SetNms <$> inp)

    labelled "Minimum threshold" $ do
      inp <- rangePreview printFloat (0.0, 0.5) 0.01 (view (#detection . #threshold) <$> prefs)
      prefCommand (SetThreshold <$> inp)

    labelled "Maximum detections" $ do
      inp <- rangePreview (T.pack . show) (0, 1000) 1 (view (#detection . #detections) <$> prefs)
      prefCommand (SetDetections <$> inp)

  groupPane "Active detections" $ do

    let showCounts (V2 n total) = showText n <> "/" <> showText total
        counts = detectionSummary <$> doc <*> (view #threshold <$> prefs)

    labelled "Visible/detected" $ dynText (showCounts <$> counts)


    labelled "Visible threshold" $ do
      inp <- rangePreview printFloat (0.0, 1.0) 0.01 (view #threshold <$> prefs)
      prefCommand (SetThreshold <$> inp)

    labelled "Threshold margin" $ do
      inp <- rangePreview printFloat (0.0, 1.0) 0.01 (view #margin <$> prefs)
      prefCommand (SetMargin <$> inp)      

  spacer


detectionSummary :: Maybe EditorDocument -> Float -> V2 Int
detectionSummary Nothing    _         = V2 0 0
detectionSummary (Just doc) threshold = sum $ summary <$> detections where
  summary Detection{..} = V2 (if confidence > threshold then 1 else 0) 1
  detections = M.mapMaybe getDetection (doc ^. #annotations)

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
selectedClass AppEnv{config, currentClass} = M.lookup <$> currentClass <*> fmap (view #classes) config

overlay :: forall t m. AppBuilder t m => EventSelector t Shortcut -> Dynamic t (Maybe EditorDocument) -> Dynamic t Preferences -> m ()
overlay shortcut document prefs = row "expand  disable-cursor" $ do
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

      detect <- toolButton docOpen "Detect" "auto-fix" "Detect annotations using current trained model"
      command (const DetectCmd) detect

      buttonGroup $ do
        docCommand  (const DocUndo)  =<< toolButton canUndo "Undo" "undo" "Undo last edit"
        docCommand  (const DocRedo)  =<< toolButton canRedo "Redo" "redo" "Re-do last undo"
        command     (const ClearCmd) =<< toolButton' "Clear" "eraser" "Clear all annotations"

    canUndo = fromDocument False (not . null . view #undos)
    canRedo = fromDocument False (not . null . view #redos)

    footer = buttonRow $ do
      spacer
      buttonGroup $ do
        discard   <- toolButton docOpen "Discard" "delete-empty" "Discard image from the collection"
        test      <- toolButton docOpen "Test" "teach" "Submit image for testing"
        train     <- toolButton docOpen "Train" "book-open-page-variant" "Submit image for training"

        command SubmitCmd $
          leftmost [ CatDiscard <$ discard, CatTest <$ test, CatTrain <$ train ]

    buttonRow = row "p-2 spacing-4"
    --nonEmpty label = notNullOf (_Just . label . traverse)
    fromDocument a f = fromMaybe a . fmap f <$> document
    docOpen = isJust <$> document
