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
import Control.Lens (notNullOf, Getting, firstOf)

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
      DocUndo   -> HistUndo
      DocRedo   -> HistRedo
      DocEdit e -> HistEdit e
 

setClassCommand :: EditorDocument -> (Set AnnotationId, ClassId) -> Maybe EditCmd
setClassCommand doc (selection, classId) =
  guard (S.size selection > 0) $> DocEdit (SetClassEdit classId selection)


loadDetections :: UTCTime -> [Detection] -> EditorDocument -> EditorDocument
loadDetections time detections doc = doc
  & #history      %~  mappend [(time, HistEdit (DetectionEdit detections))]
  & #annotations  .~ fromDetections 0 detections


loadReview :: UTCTime -> [Detection] -> EditorDocument -> EditorDocument
loadReview time detections doc = doc

 
loadedDocument :: Document -> UTCTime -> EditorDocument
loadedDocument doc@Document{info, annotations, detections} time = maybeLoad (editorDocument doc) where

    f = if info ^. #category == New && null annotations
      then loadDetections time
      else loadReview time

    maybeLoad = fromMaybe id (f . fst <$> detections)
    


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
        detectionsCmd = DocEdit . DetectionEdit <$> env ^. #detections

        editCmd   = leftmost [oneOf _EditCmd cmds, clearCmd, setClassCmd,  detectionsCmd]

  -- Set selection to the last added annotations (including undo/redo etc.)
  selection <- holdDyn mempty $ oneOf _SelectCmd cmds

  entry <- withTime (historyEntry <$> editCmd)
  history <- foldDyn (:) [(time, HistOpen)] entry

  rec
    annotations  <- holdIncremental (loaded ^. #annotations) (annotationsPatch <?> patch)
    let patched = patchIncremental annotations (maybePatch <$> pending)
        pending = pendingEdit <$> document <*> action

    -- logEvent (updated (pending <$> document <*> action))

    (action, sceneEvents) <- sceneView $ Scene
      { image    = ("images/" <> loaded ^. #name, loaded ^. #info . #imageSize)
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


navigationCmd :: Reflex t => Event t [AppCommand]
                          -> ((Event t (), Event t DocName), Event t (Maybe ImageCat))
navigationCmd cmds = (navigation, save) where
    (submit, open) = (oneOf _SubmitCmd cmds, oneOf _OpenCmd cmds)
    save = leftmost [ Just <$> submit, Nothing <$ open ]
    navigation = (void submit, open)


save :: Preferences -> (UTCTime, Maybe ImageCat) -> EditorDocument -> Document
save prefs (time, maybeCat) doc = toDocument (prefs ^. #threshold) $ doc
  &  maybe id (\cat -> #info . #category .~ cat) maybeCat
  & #history %~ ((time, HistClose) :) 



withTime :: GhcjsBuilder t m => Event t a -> m (Event t (UTCTime, a))
withTime e = performEvent $ ffor e $ \a -> do
  time <- liftIO getCurrentTime
  return (time, a) 
    

saveDocument :: GhcjsBuilder t m => Dynamic t Preferences -> Dynamic t (Maybe EditorDocument)
                        -> Event t (Maybe ImageCat) -> m (Event t ClientMsg)
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
        ]

      (navigations, saves) = navigationCmd cmds
      clientConfig = ClientConfig <$> oneOf _ConfigCmd cmds

  clientSaves <- saveDocument preferences document saves
  (docSelected, loaded, clientNav) <- manageDocument (void hello) serverMsg navigations

  collection <- holdCollection serverMsg

  let shortcut   = fan shortcuts
      detections = filterDocument document $ preview _ServerDetection <?> serverMsg

      env = AppEnv
        { basePath = "http://" <> host
        , shortcut
        , selection
        , document
        , config
        , preferences
        , currentClass
        , docSelected
        , collection

        , loaded
        , detections
        }

  config <- holdDyn def $ leftmost [initConfig, preview _ServerConfig <?> serverMsg]
  preferences <- foldDyn updatePrefs def $
    mconcat [pure . SetPrefs <$> initPrefs, someOf _PrefCmd cmds]

  prefsChanged <- debounce 0.5 (updated preferences)

  let classSelected = oneOf _ClassCmd cmds
      (hello, initPrefs, initConfig)  = split3 (preview _ServerHello <?> serverMsg)

  currentClass <- foldDyn ($) 0 $ mergeWith (.)
    [ validClass <$> updated config
    , const . snd <$> classSelected
    ]


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

  updatePref (SetThreshold t)  = #threshold .~ t
  updatePref (SetMargin m)  = #margin .~ m

  updatePref (SetImageOrder order)  = #ordering .~ order

  updatePref (SetPrefs prefs) = const prefs
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





rangeSlider :: (Builder t m, Read a, Show a, Num a) => (a, a) -> a -> a -> Event t a -> m (Dynamic t a)
rangeSlider (l, u) step initial setter = do

  rec
    slider <- inputElem
        [ type_ =: "range", showA "min" =: l, showA "max" =: u
        , showA "step" =: step, class_ =: "custom-range"] $ def

          & inputElementConfig_setValue      .~ (textValue <$> setter)
          & inputElementConfig_initialValue  .~ (textValue initial)

  holdDyn initial (read . T.unpack <$> _inputElement_input slider)

    where
      textValue = T.pack . show

rangeView :: (Builder t m, Read a, Show a, Num a, Eq a) => (a, a) -> a -> Dynamic t a -> m (Event t a)
rangeView range step = toView (rangeSlider range step (fst range))

-- selectOption :: Builder t m => [Property t] -> [(Text, a)] -> a -> Event t a -> m (Dynamic t a)
selectView :: (Builder t m, Eq a) => [(Text, a)] -> Dynamic t a -> m (Event t a)
selectView options = toView (selectOption [class_ =: "custom-select"] options option)
  where option = snd (L.head options)

rangePreview :: (Builder t m, Read a, Show a, Num a, Eq a) => (a -> Text) -> (a, a) -> a -> Dynamic t a -> m (Event t a)
rangePreview showValue range step value = row "spacing-3 align-items-center" $ do
  inp <- rangeView range step value
  span [] $ dynText $ (showValue <$> value)
  return inp

printFloat :: Float -> Text
printFloat = T.pack . printf "%.2f"

printFloat0 :: Float -> Text
printFloat0 = T.pack . printf "%.0f"


toView :: (Builder t m, Eq a) => (Event t a -> m (Dynamic t a)) -> Dynamic t a -> m (Event t a)
toView makeWidget value = do
    postBuild <- getPostBuild

    rec
      value' <- makeWidget $ leftmost
        [ (attachPromptlyDynWithMaybe filterEq value' (updated value))
        , current value `tag` postBuild
        ]

    return (updated value')

filterEq :: (Eq a) => a -> a -> Maybe a
filterEq x y = if x == y then Nothing else Just y


checkboxLabel :: Builder t m => Text -> Text -> Dynamic t Bool -> m (Event t Bool)
checkboxLabel i t value = div [class_ =: "custom-control custom-checkbox"] $ do
    let attrs = M.fromList [("class", "custom-control-input"), ("id", i)]

    inp <- checkboxView (pure attrs) value
    Html.label [class_ =: "custom-control-label", Html.for_ =: i] $ text t

    return inp

settingsPane :: AppBuilder t m => Text -> m a -> m a
settingsPane title children = column "v-spacing-2 p-2 border" $ do
    h5 [] $ text "Interface settings"
    children

settingsTab :: AppBuilder t m => m ()
settingsTab = column "h-100 v-spacing-2" $ do
  prefs <- view #preferences
  doc   <- view #document

  settingsPane "Interface settings" $ do

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

  settingsPane "Trainer settings" $ do

    labelled "Image order" $ do
        inp <- selectView [("Mixed", OrderMixed), ("Sequential", OrderSequential)] (view #ordering <$> prefs)
        prefCommand (SetImageOrder <$> inp)


  settingsPane "Image adjustment" $ do
    labelled "Gamma" $ do
        inp <- rangePreview printFloat (0.25, 4.0) 0.01 (view #gamma <$> prefs)
        prefCommand (SetGamma <$> inp)

    labelled "Brightness" $ do
        inp <- rangePreview printFloat (-1.0, 1.0) 0.01 (view #brightness <$> prefs)
        prefCommand (SetBrightness <$> inp)

    labelled "Contrast" $ do
        inp <- rangePreview printFloat (0.0, 10.0) 0.01 (view #contrast <$> prefs)
        prefCommand (SetContrast <$> inp)

  settingsPane "Detection settings" $ do

    labelled "Non maxima suppression" $ do
      inp <- rangePreview printFloat (0.0, 1.0) 0.01 (view (#detection . #nms) <$> prefs)
      prefCommand (SetNms <$> inp)

    labelled "Minimum threshold" $ do
      inp <- rangePreview printFloat (0.0, 0.5) 0.01 (view (#detection . #threshold) <$> prefs)
      prefCommand (SetThreshold <$> inp)

    labelled "Maximum detections" $ do
      inp <- rangePreview (T.pack . show) (0, 1000) 1 (view (#detection . #detections) <$> prefs)
      prefCommand (SetDetections <$> inp)

  settingsPane "Active detections" $ do

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
          leftmost [ Discard <$ discard, Test <$ test, Train <$ train ]

    buttonRow = row "p-2 spacing-4"
    --nonEmpty label = notNullOf (_Just . label . traverse)
    fromDocument a f = fromMaybe a . fmap f <$> document
    docOpen = isJust <$> document
