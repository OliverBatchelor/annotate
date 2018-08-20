{-# LANGUAGE CPP #-}

module Client.Main where

import Annotate.Prelude hiding (div)
import Client.Common

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Printf
import Data.Text.Encoding

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

import Builder.Html
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
   let host = "annotate.dynu.net:3000"
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
prefsCss Preferences{opacity} = renderCSS $ do
  ".shape" ? do
    "fill-opacity" .= showText opacity


nonEmpty :: [a] -> Maybe [a]
nonEmpty = \case
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
errorMessage (ErrNotFound doc) = ("File not found", "File \"" <> doc <> "\" not found on server.")
errorMessage ErrNotRunning = ("Trainer error", "Trainer process not started.")
errorMessage (ErrTrainer msg) = ("Trainer error", msg)


printLog :: (MonadIO m, Show a) => a -> m ()
printLog = liftIO . putStrLn . truncate . show where
  truncate str = if length str > 160
    then take 160 str <> "..."
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


handleHistory :: GhcjsBuilder t m => Event t DocName -> m (Dynamic t (Maybe DocName))
handleHistory selected = mdo

  initial <- fromFrag <$> getLocationFragment
  file <- holdUniqDyn =<< holdDyn initial (Just <$> leftmost [selected, changes])

  let update     = filterMaybe $ updateHistory <$> current history <*> current file <@> selected
      changes    = uriDocument <$> updated history

  history <- manageHistory update
  return file

   where
     fromFrag "#"   = Nothing
     fromFrag ""    = Nothing
     fromFrag frag  = Just (T.drop 1 frag)

     uriDocument = T.pack . drop 1 . view #uriFragment . _historyItem_uri

     updateHistory item Nothing k = Just $ HistoryCommand_ReplaceState $ update item k
     updateHistory item (Just previous) k
        | previous /= k = Just $ HistoryCommand_PushState $ update item k
        | otherwise       = Nothing

     update (HistoryItem state uri) k = HistoryStateUpdate
        { _historyStateUpdate_state = state
        , _historyStateUpdate_title = ""
        , _historyStateUpdate_uri   = Just $ uri & #uriFragment .~ T.unpack ("#" <> k)
        }


sceneWidget :: forall t m. (GhcjsAppBuilder t m)
            => Event t [AppCommand]
            -> Event t Document
            -> m (Event t (DMap Shortcut Identity), Dynamic t Action, Dynamic t (Maybe EditorDocument))
sceneWidget cmds loaded = do

  dim <- holdDyn (800, 600) (view (#info . #imageSize) <$> loaded)
  viewport <- viewControls cmds dim

  rec
    input <- holdInputs (current viewport) sceneEvents =<< windowInputs element
    let (action, maybeDoc, sceneEvents) = r


    (element, r) <- Svg.svg' [class_ =: "expand enable-cursor", version_ =: "2.0"] $ do
        sceneDefines viewport =<< view #preferences

        inViewport viewport $ replaceHold
            (pure (def, pure Nothing, never))
            (documentEditor input cmds . fromDocument <$> loaded)

  return (matchShortcuts input, action, maybeDoc)


newDetections :: AnnotationId -> [Detection] -> Map AnnotationId Detection
newDetections nextId detections = M.fromList (zip [nextId..] detections)

replaceDetections :: EditorDocument -> Map AnnotationId Detection -> EditCmd
replaceDetections doc detections = DocEdit (replaceAllEdit doc (toAnnotation <$> detections)) where
  toAnnotation detection = Annotation
    { shape     = BoxShape (detection ^. #bounds)
    , label     = detection ^. #label
    , detection = Just detection
    }

documentEditor :: forall t m. (GhcjsAppBuilder t m)
            => SceneInputs t
            -> Event t [AppCommand]
            -> EditorDocument
            -> m (Dynamic t Action, Dynamic t (Maybe EditorDocument), Event t (DocPart, SceneEvent))
documentEditor input cmds loaded  = do

  rec
    document <- holdDyn loaded document'
    let (patch, document') = split (attachWithMaybe (flip applyCmd') (current document) docCmd)
        clearCmd = (clearAnnotations <$> current document) <@ (oneOf _ClearCmd cmds)

        detectionsCmd = replaceDetections <$> current document <@> detections'
        detections' = newDetections <$> current nextId <@> oneOf _DetectionsCmd cmds
        docCmd = leftmost [oneOf _EditCmd cmds, clearCmd, detectionsCmd]

    -- Keep track of next id to use for new annotations
    let initialId = maybe 0 (+1) (maxId loaded)
        addNew = (>>= maxEdit) . preview _DocEdit <?> docCmd
    nextId <- foldDyn (max . (+ 1)) initialId addNew

  -- command DetectionsAddedCmd detections'

  -- Set selection to the last added annotations (including undo/redo etc.)
  selection <- holdDyn mempty $
    leftmost [oneOf _SelectCmd cmds]


  env <- ask

  rec
    annotations  <- holdIncremental annotations0 (PatchMap <$> patch)
    let patched = patchIncremental annotations (pending <$> document <*> action)

    -- logEvent (updated (pending <$> document <*> action))

    (action, sceneEvents) <- sceneView $ Scene
      { image    = ("images/" <> loaded ^. #name, loaded ^. #info . #imageSize)
      -- , viewport = viewport
      , input    = input
      , document = document

      , selection = selection
      , annotations = patched
      -- , detections = detections

      , nextId = nextId
      , currentClass = view #currentClass env
      , config = view #config env
      , shortcut = view #shortcut env
      , preferences = view #preferences env
      }

  return (action, (Just <$> document), sceneEvents)
    where annotations0 = loaded ^. #annotations



pending :: EditorDocument -> Action -> PatchMap AnnotationId Annotation
pending doc Action{edit}
  | Just e <- edit     = maybe mempty (PatchMap . fst) (applyEdit e doc)
  | otherwise          = mempty


oneOf :: Reflex t => Getting (First a) s a -> Event t [s] -> Event t a
oneOf getter = fmapMaybe (preview (traverse . getter))

bodyWidget :: forall t m. GhcjsBuilder t m => Text -> m (AppEnv t)
bodyWidget host = mdo
  (opened, closed, serverMsg, serverErrors) <- network host clientMsg

  let disconnected = Workflow $ do
        Dialog.connecting
        return (("disconnected", never), connected <$ opened)

      connected = Workflow $ do
        return (("connected", never), ready <$> hello)

      ready _ = Workflow $ do

        postBuild <- getPostBuild
        let initialLoad = maybe (ClientNext Nothing) ClientOpen
            reqs        = mergeList
              [ ClientCollection   <$ postBuild
              , (initialLoad <$> current userSelected) `tag` postBuild
              ]

        return (("ready", toList <$> reqs), never)

  (state, clientMsgs) <- split <$> (workflow $
    commonTransition (disconnected <$ closed) disconnected)

  let clientMsg = ffilter (not . null) $ mconcat
        [ switchPrompt clientMsgs
        , fmap (pure . ClientOpen) <?> updated userSelected
        , fmapMaybe (preview _RemoteCmd) <$> cmds
        ]

  userSelected <- handleHistory (oneOf _LoadCmd cmds)

  setTitle $ ffor document $ \doc ->
    "Annotate - " <> fromMaybe ("no document") (view #name <$> doc)

  collection <- holdCollection serverMsg
  detections <- holdDyn mempty (oneOf _DetectionsAddedCmd cmds)


  let hello   = preview _ServerHello <?> serverMsg
      (loaded :: Event t Document)  = preview _ServerDocument <?> serverMsg

      env = AppEnv
        { basePath = "http://" <> host
        , document = document
        , detections = detections
        , commands = cmds
        , config = config
        , preferences = preferences
        , currentClass = currentClass
        , userSelected = userSelected
        , shortcut = fan shortcuts
        , collection = collection
        }

  config <- holdDyn def $ leftmost [view _2 <$> hello, preview _ServerConfig <?> serverMsg]
  preferences <- foldDyn updatePrefs def (fmapMaybe (preview _PrefCmd) <$> cmds)

  let classSelected = oneOf _ClassCmd cmds
  currentClass <- foldDyn ($) 0 $ mergeWith (.)
    [ validClass <$> updated config
    , const . snd <$> classSelected
    ]

  let makeDetections doc (k, detections) = do
        guard (Just k == (view #name <$> doc))
        return [DetectionsCmd detections]

      detectionsCmd = attachWithMaybe makeDetections (current document)  (preview _ServerDetection <?> serverMsg)
      cmds = leftmost [interfaceCmds, detectionsCmd]

  ((shortcuts, action, document), interfaceCmds) <- flip runReaderT env $ runEventWriterT $ do

    runWithClose $ leftmost
      [ runDialog <$> oneOf _DialogCmd cmds
      , errorDialog <$> serverErrors
      ]

    cursorLock action $ do
      div [class_ =: "scene expand"] $
        const <$> sceneWidget cmds loaded
              <*> overlay document preferences
  return env

runDialog :: AppBuilder t m => Dialog -> m (Event t ())
runDialog (ClassDialog selection) = selectClassDialog (M.keysSet selection)


updatePrefs :: [PrefCommand] -> Preferences -> Preferences
updatePrefs cmds = flip (foldr updatePref) cmds where
  updatePref :: PrefCommand -> Preferences -> Preferences
  updatePref (ZoomBrush delta) = #brushSize %~ clamp (2, 400) . (* wheelZoom delta)
  updatePref (SetOpacity opacity) = #opacity .~ opacity
  updatePref (SetInstanceColors b) = #instanceColours .~ b
  updatePref (SetControlSize size) = #controlSize .~ size
  updatePref (ShowClass (classId, shown)) = #hiddenClasses .
    at classId .~  if shown then Just () else Nothing

  updatePref (SetMinThreshold t)  = #detection . #threshold .~ t
  updatePref (SetNms nms)         = #detection . #nms .~ nms
  updatePref (SetDetections d)    = #detection . #detections .~ d

  updatePref (SetThreshold t)  = #threshold .~ t
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
holdCollection serverMsg = foldDyn ($) emptyCollection $ mergeWith (.)
    [ const  <$> collection
    , applyUpdate <$> update
    ]

  where
    collection = preview _ServerCollection <?> serverMsg
    update = preview _ServerUpdateInfo <?> serverMsg

    applyUpdate (k, info) = over #images (M.insert k info)





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

    where
      filterEq x y = if x == y then Nothing else Just y


checkboxLabel :: Builder t m => Text -> Text -> Dynamic t Bool -> m (Event t Bool)
checkboxLabel i t value = div [class_ =: "custom-control custom-checkbox"] $ do
    let attrs = M.fromList [("class", "custom-control-input"), ("id", i)]

    inp <- checkboxView (pure attrs) value
    Html.label [class_ =: "custom-control-label", Html.for_ =: i] $ text t

    return inp


settingsTab :: AppBuilder t m => m ()
settingsTab = column "h-100 v-spacing-2" $ do
  prefs <- view #preferences
  doc   <- view #document

  column "v-spacing-2 p-2 border" $ do
    h5 [] $ text "Interface settings"

    instanceCols <- checkboxLabel "instance-cols" "Instance colours" (view #instanceColours <$> prefs)
    prefCommand (SetInstanceColors <$> instanceCols)

    labelled "Mask opacity" $ do
        inp <- rangePreview printFloat (0.0, 1.0) 0.01 (view #opacity <$> prefs)
        prefCommand (SetOpacity <$> inp)

    labelled "Control size" $ do
      inp <- rangePreview (printFloat0) (5.0, 50.0) 1 (view #controlSize <$> prefs)
      prefCommand (SetControlSize <$> inp)

    return ()

  column "v-spacing-2 p-2 border" $ do
    h5 [] $ text "Detection settings"

    labelled "Non maxima suppression" $ do
      inp <- rangePreview printFloat (0.0, 1.0) 0.01 (view (#detection . #nms) <$> prefs)
      prefCommand (SetNms <$> inp)

    labelled "Minimum threshold" $ do
      inp <- rangePreview printFloat (0.0, 0.5) 0.01 (view (#detection . #threshold) <$> prefs)
      prefCommand (SetThreshold <$> inp)

    labelled "Maximum detections" $ do
      inp <- rangePreview (T.pack . show) (0, 1000) 1 (view (#detection . #detections) <$> prefs)
      prefCommand (SetDetections <$> inp)

  column "v-spacing-2 p-2 border" $ do
    h5 [] $ text "Active detections"

    let showCounts (V2 n total) = showText n <> "/" <> showText total
        counts = detectionSummary <$> doc <*> (view #threshold <$> prefs)

    labelled "Visible/detected" $ dynText (showCounts <$> counts)


    labelled "Visible threshold" $ do
      inp <- rangePreview printFloat (0.0, 1.0) 0.01 (view #threshold <$> prefs)
      prefCommand (SetThreshold <$> inp)


  spacer


detectionSummary :: Maybe EditorDocument -> Float -> V2 Int
detectionSummary Nothing    _         = V2 0 0
detectionSummary (Just doc) threshold = sum $ M.mapMaybe (fmap summary) detections where
  summary Detection{..} = V2 (if confidence > threshold then 1 else 0) 1
  detections = view #detection <$> doc ^. #annotations


sidebar :: AppBuilder t m => m ()
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
            icon (def & #name .~ Dyn (swapping ("chevron-right", "chevron-left") isOpen))
        isOpen <- toggle False (domEvent Click e)
        return isOpen


-- Main interface


selectedClass :: Reflex t => AppEnv t -> Dynamic t (Maybe ClassConfig)
selectedClass AppEnv{config, currentClass} = M.lookup <$> currentClass <*> fmap (view #classes) config

overlay :: AppBuilder t m => Dynamic t (Maybe EditorDocument) -> Dynamic t Preferences -> m ()
overlay document prefs = row "expand  disable-cursor" $ do
   sidebar
   column "expand" $
    sequence_ [header, spacer, footer]

  where
    header = buttonRow $ do
      asks selectedClass >>= classToolButton >>= command (const (DialogCmd (ClassDialog mempty)))

      spacer

      detect <- toolButton docOpen "Detect" "auto-fix" "Detect annotations using current trained model"

      let detectCmd doc prefs = ClientDetect <$> (view #name <$> doc) <*> pure (view #detection prefs)
      remoteCommand id (filterMaybe ((detectCmd <$> current document <*> current prefs) `tag` detect))

      buttonGroup $ do
        docCommand  (const DocUndo)  =<< toolButton canUndo "Undo" "undo" "Undo last edit"
        docCommand  (const DocRedo)  =<< toolButton canRedo "Redo" "redo" "Re-do last undo"
        command     (const ClearCmd) =<< toolButton' "Clear" "eraser" "Clear all annotations"



    canUndo = fromDocument False (not . null . view #undos)
    canRedo = fromDocument False (not . null . view #redos)

    footer = buttonRow $ do
      spacer
      buttonGroup $ do
        discard <- toolButton docOpen "Discard" "delete-empty" "Discard image from the collection"
        test  <- toolButton docOpen "Test" "teach" "Submit image for testing"
        train  <- toolButton docOpen "Train" "book-open-page-variant" "Submit image for training"

        remoteCommand id $ leftmost
          [ withDocument (ClientDiscard . view #name) discard
          , withDocument (ClientSubmit . category Train) train
          , withDocument (ClientSubmit . category Test) test
          ]

    buttonRow = row "p-2 spacing-4"
    nonEmpty label = notNullOf (_Just . label . traverse)

    fromDocument a f = fromMaybe a . fmap f <$> document
    withDocument f e = fmap (f . toDocument) <?> (current document `tag` e)

    category = set (#info . #category)

    docOpen = isJust <$> document
