module Client.Main where

import Annotate.Prelude hiding (div)
import Client.Common

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Printf 

import Data.Default

import Control.Monad.Reader
import Control.Lens (notNullOf)

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


main :: JSM ()
main = Main.mainWidgetWithHead' (const headWidget, bodyWidget)


orLocal :: Text -> Text
orLocal url = if url == "" then "localhost:3000" else url

headWidget :: GhcjsBuilder t m => m Text
headWidget = do

   -- host <- orLocal <$> getLocationHost
   -- base_ [href_ =: "http://" <> host]

   let host = "localhost:3000"
       css file = stylesheet ("http://" <> host <> "/css/" <> file)

   css "bootstrap.min.css"
   css "materialdesignicons.min.css"
   css "style.css"

   return host

  where
    stylesheet url = link_ [href_ =: url, rel_ =: ["stylesheet"], crossorigin_ =: "anonymous"]
    --css = decodeUtf8 $(embedFile "style.css")

nonEmpty :: [a] -> Maybe [a]
nonEmpty = \case
  [] -> Nothing
  xs -> Just xs


viewControls :: GhcjsBuilder t m => Event t AppCommand -> Dynamic t Dim -> m (Dynamic t Viewport)
viewControls cmds dim = do
  windowDim  <- windowDimensions

  rec 
    controls <- holdDyn  (1, V2 0 0) (updateView <$> viewCmds <#> current viewport)
    let viewport = makeViewport <$> controls <*> dim <*> windowDim

  return viewport

  where
    viewCmds = preview _ViewCmd <?> cmds

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
  performEvent_ (printLog <$> decoded)

  return
    ( socket ^. webSocket_open
    , close <$> socket ^. webSocket_close
    , decoded
    , errors)


  where
    close (True,  _, _)       = Nothing
    close (False, _, reason)  = Just reason


handleHistory :: GhcjsBuilder t m => Event t DocName -> m (Event t DocName)
handleHistory loaded = mdo

  currentFile <- hold Nothing (Just <$> leftmost [loaded, changes])

  let update     = id <?> (updateHistory <$> current history <*> currentFile <@> loaded)
      changes    = uriDocument <$> updated history

  history <- manageHistory update
  return $ new <?> (currentFile `attach` changes)

   where
     new (Nothing, k)       = Just k
     new (Just previous, k) = k <$ guard (previous /= k)

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
            => Event t AppCommand -> Event t Document -> m (Event t (DMap Shortcut Identity), Dynamic t Action, Dynamic t (Maybe Document))
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
            (documentEditor input cmds <$> loaded)

  return (matchShortcuts input, action, maybeDoc)
            

documentEditor :: forall t m. (GhcjsAppBuilder t m)
            => SceneInputs t 
            -> Event t AppCommand 
            -> Document 
            -> m (Dynamic t Action, Dynamic t (Maybe Document), Event t (Map AnnotationId SceneEvent))
documentEditor input cmds loaded  = do

  rec 
    document <- holdDyn loaded (snd <$> modified)
    let modified = attachWithMaybe (flip applyCmd') (current document) docCmd
        clearCmd = (clearAnnotations <$> current document) <@ (_ClearCmd ?> cmds)
        docCmd = leftmost [_DocCmd ?> cmds, clearCmd]
  
  -- Keep track of next id to use for new annotations
  let initialId = maybe 0 (+1) (maxId loaded)
      addNew = _DocEdit . _Add ?> docCmd
  nextId <- foldDyn (const (+1)) initialId addNew

  -- Set selection to the last added annotations (including undo/redo etc.)
  let latestEdit = fst . fst <$> modified
      latestAdd = fmap (S.fromList . fmap fst) . preview _Add <?> latestEdit
  selection <- holdDyn S.empty $
    leftmost [latestAdd, _SelectCmd ?> cmds]
    
  env <- ask
  
  rec
    annotations  <- holdIncremental annotations0 (PatchMap . editPatch <$> modified)
    let patched = patchIncremental annotations (pending <$> document <*> action)
    
    logEvent (updated (pending <$> document <*> action))
    
    (action, sceneEvents) <- sceneView $ Scene
      { image    = ("images/" <> loaded ^. #name, loaded ^. #info . #imageSize)
      -- , viewport = viewport
      , input    = input
      , document = document

      , selection = selection
      , annotations = patched

      , nextId = nextId
      , currentClass = view #currentClass env
      , config = view #config env
      , shortcut = view #shortcut env
      }

  return (action, (Just <$> document), sceneEvents)
    where annotations0 = loaded ^. #annotations


pending :: Document -> Action -> PatchMap AnnotationId Annotation
pending doc Action{edit} 
  | Just e <- edit     = maybe mempty PatchMap (editPatch <$> applyEdit e doc)
  | otherwise          = mempty

bodyWidget :: forall t m. GhcjsBuilder t m => Text -> m ()
bodyWidget host = mdo
  (opened, closed, serverMsg, serverErrors) <- network host clientMsg

  let disconnected = Workflow $ do
        Dialog.connecting
        return (("disconnected", never), connected <$ opened)

      connected = Workflow $ do
        return (("connected", never), ready <$> hello)

      ready _ = Workflow $ do
        
        postBuild <- getPostBuild
        let reqs = mergeList 
              [ ClientNext Nothing <$ needsLoad
              , ClientCollection   <$ postBuild ]
          
            needsLoad = preview _Nothing <?> (current document `tag` postBuild)

        return (("ready", toList <$> reqs), never)

  (state, clientMsgs) <- split <$> (workflow $
    commonTransition (disconnected <$ closed) disconnected)

  let clientMsg = mconcat
        [ switchPrompt clientMsgs
        , pure . ClientOpen <$> urlSelected
        , fmap pure . preview _RemoteCmd <?> cmds
        ]


  urlSelected <- handleHistory (view #name <$> loaded)
  setTitle $ ffor document $ \doc ->
    "Annotate - " <> fromMaybe ("no document") (view #name <$> doc)

  collection <- holdCollection serverMsg
    
  let hello   = preview _ServerHello <?> serverMsg
      (loaded :: Event t Document)  = preview _ServerDocument <?> serverMsg
            
      env = AppEnv 
        { basePath = "http://" <> host
        , document = document
        , commands = cmds
        , config = config
        , preferences = preferences
        , currentClass = currentClass
        , shortcut = fan shortcuts
        , collection = collection
        }
  
  config <- holdDyn defaultConfig $ leftmost [view _2 <$> hello, preview _ServerConfig <?> serverMsg]
  preferences <- holdDyn defaultPreferences never
  
  let classSelected = preview _ClassCmd <?> cmds 
  currentClass <- foldDyn ($) 0 $ mergeWith (.)   
    [ validClass <$> updated config
    , const . snd <$> classSelected
    ]

  ((shortcuts, action, document), cmds) <- flip runReaderT env $ runEventWriterT $ do
  
    runWithClose $ leftmost 
      [ fmap runDialog . preview _DialogCmd <?> cmds
      , errorDialog <$> serverErrors
      ]
  
    cursorLock action $ do
      div [class_ =: "scene expand"] $ 
        const <$> sceneWidget cmds loaded
              <*> overlay document
  return ()

runDialog :: AppBuilder t m => Dialog -> m (Event t ())
runDialog (ClassDialog selection) = selectClassDialog selection


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



detectionTab :: AppBuilder t m => m ()
detectionTab = text "detection"
  
sidebar :: AppBuilder t m => m ()
sidebar = mdo 
  
  isOpen <- div [classList ["enable-cursor sidebar bg-white p-2", swapping ("closed", "open") isOpen]] $ do
    isOpen <- toggler

    tabs 0 
      [ (classesTab,    tab "tag-multiple"   "Classes") 
      , (imagesTab,     tab "folder-multiple-image" "Images")
      , (detectionTab,  tab "auto-fix"  "Detection")
      ]
      
    return isOpen
  return ()

    where
      toggler = mdo 
        e <- a_ [href_ =: "#", class_ =: "toggler p-2"] $ 
            icon (def & #name .~ Dyn (swapping ("chevron-right", "chevron-left") isOpen))
        isOpen <- toggle False (domEvent Click e)
        return isOpen


-- Main interface


selectedClass :: Reflex t => AppEnv t -> Dynamic t (Maybe ClassConfig)
selectedClass AppEnv{config, currentClass} = M.lookup <$> currentClass <*> fmap (view #classes) config

overlay :: AppBuilder t m => Dynamic t (Maybe Document) -> m ()
overlay document = row "expand  disable-cursor" $ do
   sidebar
   column "expand" $ 
    sequence_ [header, spacer, footer]
  
  where
    header = buttonRow $ do 
      asks selectedClass >>= classToolButton >>= command (const (DialogCmd (ClassDialog mempty)))
            
      spacer
      buttonGroup $ do
        docCommand  (const DocUndo)  =<< toolButton canUndo "Undo" "undo" "Undo last edit"
        docCommand  (const DocRedo)  =<< toolButton canRedo "Redo" "redo" "Re-do last undo"
        command     (const ClearCmd) =<< toolButton' "Clear" "eraser" "Clear all annotations"

      detect <- toolButton docOpen "Detect" "auto-fix" "Detect annotations using current trained model"
      remoteCommand id (withDocument (ClientDetect . view #name) detect)
      
    canUndo = fromDocument False (not . null . view #undos)
    canRedo = fromDocument False (not . null . view #redos)
            
    footer = buttonRow $ do
      spacer
      buttonGroup $ do
        discard <- toolButton docOpen "Discard" "delete-empty" "Discard image from the collection"
        submit  <- toolButton docOpen "Submit" "content-save" "Submit image for training"

        remoteCommand id $ leftmost
          [ withDocument (ClientDiscard . view #name) discard
          , withDocument ClientSubmit submit
          ]
    
    buttonRow = row "p-2 spacing-4"
    nonEmpty label = notNullOf (_Just . label . traverse)
    
    fromDocument a f = fromMaybe a . fmap f <$> document
    withDocument f e = fmap f <?> (current document `tag` e)
    docOpen = isJust <$> document    
    