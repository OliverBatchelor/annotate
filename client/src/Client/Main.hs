module Client.Main where

import Annotate.Common hiding (div)
import Client.Common

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Set as S

import Data.Default
import Data.Monoid

import Control.Monad.Reader

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
errorDialog err = okDialog title (iconText ("text-danger", "fa-exclamation-circle") msg)
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




sceneWidget :: forall t m. (GhcjsBuilder t m, EventWriter t AppCommand m, MonadReader AppEnv m)
            => Event t AppCommand -> (DocName, DocInfo, Document) -> m (Dynamic t Action, Dynamic t Document)
sceneWidget cmds (name, info, loaded)  = mdo

  document <- holdDyn loaded (snd <$> modified)
  let modified = attachWithMaybe (flip applyCmd') (current document) docCmd
      clearCmd = (clearObjects <$> current document) <@ (_ClearCmd ?> cmds)
      docCmd = leftmost [_DocCmd ?> cmds, clearCmd]

  viewport <- viewControls cmds info
  input <- holdInputs (current viewport) hover =<< inputs element

  -- Keep track of next id to use for new objects
  let initialId = maybe 0 (+1) (maxId loaded)
      addNew = _DocEdit . _Add ?> docCmd
  nextId <- foldDyn (const (+1)) initialId addNew

  -- Set selection to the last added objects (including undo/redo etc.)
  let latestEdit = fst . fst <$> modified
      latestAdd = fmap (S.fromList . fmap fst) . preview _Add <?> latestEdit
  selection <- holdDyn S.empty $
    leftmost [latestAdd, _SelectCmd ?> cmds]


  (element, (action, hover)) <- sceneView $ Scene
    { image    = ("images/" <> name, info ^. #imageSize)
    , viewport = viewport
    , input    = input
    , document = document

    , selection = selection
    , objects = Patched objects0 (PatchMap . editPatch <$> modified)

    , nextId = nextId
    , currentClass = pure 0
    }

  return (action, document)
    where objects0 = loaded ^. #instances


docMeta :: (DocName, DocInfo, Document) -> (DocName, DocInfo)
docMeta (name, info, _) = (name, info)

bodyWidget :: forall t m. GhcjsBuilder t m => Text -> m ()
bodyWidget host = mdo
  (opened, closed, serverMsg, serverErrors) <- network host clientMsg

  let disconnected = Workflow $ do
        connectingModal
        return (("disconnected", never), connected <$ opened)

      connected = Workflow $ do
        return (("connected", never), ready <$> hello)

      ready clientId = Workflow $ do
        needsLoad <- filterMaybe <$>
          postCurrent (preview _Nothing <$> current currentFile)

        return (("ready", ClientNext Nothing <$ needsLoad), never)

  (state, clientMsgs) <- split <$> (workflow $
    commonTransition (disconnected <$ closed) disconnected)

  runWithClose (errorDialog <$> serverErrors)

  let clientMsg = leftmost
        [ switchPrompt clientMsgs
        , ClientOpen <$> urlSelected
        , preview _RemoteCmd <?> cmds
        ]


  urlSelected <- handleHistory (view _1 <$> loaded)
  currentFile  <- holdDyn Nothing (Just . docMeta <$> loaded)

  setTitle $ ffor currentFile $ \k ->
    "Annotate - " <> fromMaybe ("no document") (fst <$> k)

  let hello   = preview _ServerHello <?> serverMsg
      loaded  = preview _ServerDocument <?> serverMsg
      env = AppEnv {basePath = "http://" <> host}

  (action, cmds) <- flip runReaderT env $ runEventWriterT $ cursorLock action $ do
    replaceHold (interface' blank blank >> return def) $
      ffor loaded $ \content@(k, info, doc0) -> do
        (action, document) <- sceneWidget cmds content
        interface (k, info, document)

        return action

  return ()


cursorLock :: Builder t m => Dynamic t Action -> m a -> m a
cursorLock action child =
  div [class_ =: "expand", draggable_ =: False, style_ ~: cursorStyle <$> action] $
    div [id_ =: "drawing", classes_ ~: lockClass <$> action] child
  where
    cursorStyle Action{..} = [("cursor", cursor)]
    lockClass Action{..} = ["expand"] <> ["cursor-lock" | lock]




interface' :: AppBuilder t m => m () -> m () -> m ()
interface' top bottom = column "expand disable-cursor" $ do
  row "p-2 spacing-4" $ do
    a [class_ =: "navbar-brand"] $ text "Annotate"
    top

  spacer
  row "p-2 spacing-4" $ bottom



-- Main interface
interface :: AppBuilder t m => (DocName, DocInfo, Dynamic t Document) -> m ()
interface (k, info, document) = interface' top bottom
  where
    canUndo = not . null . view #undos <$> document
    canRedo = not . null . view #redos <$> document

    top = do
      spacer
      buttonGroup $ do
        docCommand  (const DocUndo)  =<< toolButton canUndo "Undo" "fa-undo" "Undo last edit"
        docCommand  (const DocRedo)  =<< toolButton canRedo "Redo" "fa-redo" "Re-do last undo"
        command     (const ClearCmd) =<< toolButton' "Clear" "fa-eraser" "Clear all annotations"

      detect <- toolButton' "Detect" "fa-magic" "Detect objects using current trained model"
      remoteCommand id (ClientDetect k <$ detect)

    bottom = do
      spacer
      buttonGroup $ do
        discard <- toolButton' "Discard" "fa-trash" "Discard image from the collection"
        next    <- toolButton' "Next" "fa-step-forward" "Skip to the next image"
        submit  <- toolButton' "Submit" "fa-save" "Submit image for training"

        remoteCommand id $ leftmost
          [ ClientDiscard k     <$ discard
          , ClientNext (Just k) <$ next
          , ClientSubmit k Train <$> current document <@ submit
          ]
