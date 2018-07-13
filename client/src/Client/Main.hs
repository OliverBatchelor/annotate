module Client.Main where

import Annotate.Common hiding (div)
import Client.Common

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Default
import Data.Monoid

import Control.Monad.Reader
import Control.Lens (notNullOf)

import Scene.Viewport
import Scene.View
import Scene.Types
import Scene.Events

import Input.Events (inputs)
import Scene.Events

import Reflex.Classes
import Builder.Html

import Input.Window

import Client.Widgets
import qualified Client.Dialog as Dialog

import Annotate.Types
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


   stylesheet "https://use.fontawesome.com/releases/v5.0.13/css/all.css"
   stylesheet "https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css"
   stylesheet ("http://" <> host <> "/css/style.css")

   return host

  where
    stylesheet url = link_ [href_ =: url, rel_ =: ["stylesheet"]]
    --css = decodeUtf8 $(embedFile "style.css")

nonEmpty :: [a] -> Maybe [a]
nonEmpty = \case
  [] -> Nothing
  xs -> Just xs


viewControls :: GhcjsBuilder t m => Event t AppCommand -> DocInfo -> m (Dynamic t Viewport)
viewControls cmds info = mdo
  windowDim  <- windowDimensions

  controls <- holdDyn  (1, V2 0 0) (updateView <$> viewCmds <#> current viewport)
  let viewport = makeViewport <$> controls <*> pure (info ^. #imageSize) <*> windowDim

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
errorDialog err = Dialog.ok title (Dialog.iconText ("text-danger", "fa-exclamation-circle") msg)
  where  (title, msg) = errorMessage err

errorMessage :: ErrCode -> (Text, Text)
errorMessage (ErrDecode msg) = ("Decode error", msg)
errorMessage (ErrNotFound doc) = ("File not found", "File \"" <> doc <> "\" not found on server.")
errorMessage ErrNotRunning = ("Trainer error", "Trainer process not started.")
errorMessage (ErrTrainer msg) = ("Trainer error", msg)



network :: GhcjsBuilder t m => Text -> Event t ClientMsg -> m (Event t (), Event t (Maybe Text), Event t ServerMsg, Event t ErrCode)
network host send = do
  socket <- webSocket ("ws://" <> host <> "/clients") $ def
    & webSocketConfig_send  .~ (pure . encode <$> send)

  let (errs, decoded) = splitEither (eitherDecodeStrict <$> socket ^. webSocket_recv) 
      errors = leftmost
        [ ErrDecode . fromString <$>  errs
        , preview _ServerError <?> decoded
        ]

  performEvent_ (liftIO . print <$> send)
  performEvent_ (liftIO . print <$> decoded)

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
            => Event t AppCommand -> Document -> m (Dynamic t Action, Dynamic t (Maybe Document))
sceneWidget cmds loaded  = mdo

  document <- holdDyn loaded (snd <$> modified)
  let modified = attachWithMaybe (flip applyCmd') (current document) docCmd
      clearCmd = (clearAnnotations <$> current document) <@ (_ClearCmd ?> cmds)
      docCmd = leftmost [_DocCmd ?> cmds, clearCmd]

  viewport <- viewControls cmds (loaded ^. #info)
  input <- holdInputs (current viewport) hover =<< inputs element

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

  (element, (action, hover)) <- sceneView $ Scene
    { image    = ("images/" <> loaded ^. #name, loaded ^. #info . #imageSize)
    , viewport = viewport
    , input    = input
    , document = document

    , selection = selection
    , annotations = Patched annotations0 (PatchMap . editPatch <$> modified)

    , nextId = nextId
    , currentClass = view #currentClass env
    , config = view #config env
    }

  return (action, (Just <$> document))
    where annotations0 = loaded ^. #annotations


bodyWidget :: forall t m. GhcjsBuilder t m => Text -> m ()
bodyWidget host = mdo
  (opened, closed, serverMsg, serverErrors) <- network host clientMsg

  let disconnected = Workflow $ do
        Dialog.connecting
        return (("disconnected", never), connected <$ opened)

      connected = Workflow $ do
        return (("connected", never), ready <$> hello)

      ready _ = Workflow $ do
        
        needsLoad <- filterMaybe <$>
          postCurrent (preview _Nothing <$> current document)

        return (("ready", ClientNext Nothing <$ needsLoad), never)

  (state, clientMsgs) <- split <$> (workflow $
    commonTransition (disconnected <$ closed) disconnected)

  runWithClose (errorDialog <$> serverErrors)

  let clientMsg = leftmost
        [ switchPrompt clientMsgs
        , ClientOpen <$> urlSelected
        , preview _RemoteCmd <?> cmds
        ]


  urlSelected <- handleHistory (view #name <$> loaded)
  setTitle $ ffor document $ \doc ->
    "Annotate - " <> fromMaybe ("no document") (view #name <$> doc)

  let hello   = preview _ServerHello <?> serverMsg
      (loaded :: Event t Document)  = preview _ServerDocument <?> serverMsg
      env = AppEnv 
        { envBasePath = "http://" <> host
        , envAction   = action
        , envDocument = document
        , envCommands = cmds
        , envConfig = config
        , envClass = currentClass
        }
  
  config <- holdDyn defaultConfig (view _2 <$> hello)
  currentClass <- holdDyn 0 never

  ((action, document), cmds) <- flip runReaderT env $ runEventWriterT $ 
    cursorLock action $ do
      div [class_ =: "scene expand"] $ do
        state <- replaceHold 
            (pure (def, pure Nothing)) 
            (sceneWidget cmds <$> loaded)
        overlay document
        return state
        
  return ()



cursorLock :: Builder t m => Dynamic t Action -> m a -> m a
cursorLock action child =
  div [class_ =: "expand", draggable_ =: False, style_ ~: cursorStyle <$> action] $
    div [classes_ ~: lockClass <$> action] child
  where
    cursorStyle Action{..} = [("cursor", cursor)]
    lockClass Action{..} = ["expand"] <> ["cursor-lock" | lock]




data SideTab = ClassesTab | ImagesTab | DetectionTab
  deriving (Show, Ord, Eq, Generic)

type Selectable t m = Dynamic t Bool -> m (Event t ()) 

tab :: Builder t m => Text -> Selectable t m
tab t = \active -> 
  li [class_ =: "nav-item"] $ do
    e <- a_ [classList ["nav-link h-100", "active" `gated` active], href_ =: "#"] $ text t
    return (domEvent Click e)

tabs :: (Ord k, Builder t m) => k -> [(k, Selectable t m)] -> m (Dynamic t k)
tabs initial items = ul [class_ =: "nav nav-tabs background-light enable-cursor "] $ selectable initial items

selectable :: (Ord k, Builder t m) => k -> [(k, Selectable t m)] -> m (Dynamic t k)
selectable initial items = mdo
  let isOpen = fanDyn open
  clicks <- for items $ \(k, tab) -> fmap (const k) <$> tab (isOpen k)
  
  open <- holdDyn initial (leftmost clicks)
  return open
  
sidebar :: AppBuilder t m => m ()
sidebar = mdo 
  isOpen <- div [classList ["enable-cursor sidebar bg-white p-2", swapping ("closed", "open") isOpen]] $ do
    isOpen <- toggler
    
    column "" $ do    
      openTab <- tabs ImagesTab 
        [ (ClassesTab, tab "Classes") 
        , (ImagesTab, tab "Images")
        , (DetectionTab, tab "Detection")
        ]
      spacer
      
    return isOpen
  return ()

    where
      toggler = mdo 
        e <- div_ [class_ =: "toggler p-2"] $ 
            i [classList ["fa", swapping ("fa-chevron-right", "fa-chevron-left") isOpen]] blank
        isOpen <- toggle False (domEvent Click e)
        return isOpen


-- Main interface



overlay :: AppBuilder t m => Dynamic t (Maybe Document) -> m ()
overlay document = row "expand  disable-cursor" $ do
   sidebar
   column "expand" $ 
    sequence_ [header, spacer, footer]
  
  where
    header = buttonRow $ do 
      toolButton docOpen "Category" "fa-tags" "Choose active category"

      
      spacer
      buttonGroup $ do
        docCommand  (const DocUndo)  =<< toolButton canUndo "Undo" "fa-undo" "Undo last edit"
        docCommand  (const DocRedo)  =<< toolButton canRedo "Redo" "fa-redo" "Re-do last undo"
        command     (const ClearCmd) =<< toolButton' "Clear" "fa-eraser" "Clear all annotations"

      detect <- toolButton docOpen "Detect" "fa-magic" "Detect annotations using current trained model"
      remoteCommand id (withDocument (ClientDetect . view #name) detect)
      
    canUndo = maybeDocument False (not . null . view #undos)
    canRedo = maybeDocument False (not . null . view #redos)
            
    footer = buttonRow $ do
      spacer
      buttonGroup $ do
        discard <- toolButton docOpen "Discard" "fa-trash" "Discard image from the collection"
        next    <- toolButton docOpen "Next" "fa-step-forward" "Skip to the next image"
        submit  <- toolButton docOpen "Submit" "fa-save" "Submit image for training"

        remoteCommand id $ leftmost
          [ withDocument (ClientDiscard . view #name) discard
          , withDocument (ClientNext . Just . view #name)  next
          , withDocument ClientSubmit submit
          ]
    
    buttonRow = row "p-2 spacing-4"
    nonEmpty label = notNullOf (_Just . label . traverse)
    
    maybeDocument a f = fromMaybe a . fmap f <$> document
    withDocument f e = fmap f <?> (current document `tag` e)
    docOpen = isJust <$> document    
    