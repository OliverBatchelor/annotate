{-# LANGUAGE OverloadedStrings #-}

import Annotate.Common hiding (div)

import qualified Data.Text as T
import qualified Data.Map as M

import Data.Default
import Data.Monoid

import Scene.Viewport
import Scene.View
import Scene.Types
import Scene.Events

import Input.Events
import Scene.Events

import Reflex.Classes
import Builder.Html

import Input.Window

import Widgets

import Annotate.Types
import Annotate.Document




main :: IO ()
main = mainWidgetWithHead' (const headWidget, bodyWidget)


orLocal :: Text -> Text
orLocal url =  if url == "" then "localhost:3000" else url

headWidget :: GhcjsBuilder t m => m Text
headWidget = do

   host <- orLocal <$> getLocationHost
   base_ [href_ =: "http://" <> host]

   stylesheet "https://use.fontawesome.com/releases/v5.0.13/css/all.css"
   stylesheet "https://stackpath.bootstrapcdn.com/bootstrap/4.1.1/css/bootstrap.min.css"
   stylesheet "css/style.css"

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
      ZoomCmd zoom pos -> zoomView zoom pos vp
      PanCmd localOrign page -> panView localOrign page vp

    getControls (Viewport _ _ pan zoom) = (zoom, pan)
    makeViewport (zoom, pan) image window =
        Viewport (fromDim image) (fromDim window) pan zoom


data Connection = Waiting | Error Text | Connected
  deriving (Show, Generic, Eq)

network :: GhcjsBuilder t m => Text -> Event t ClientMsg -> m (Event t (), Event t (Maybe Text), Event t ServerMsg)
network host send = do
  socket <- webSocket ("ws://" <> host <> "/ws") $ def
    & webSocketConfig_send  .~ (pure . encode <$> send)

  let recieved = decodeStrict <?> socket ^. webSocket_recv
  performEvent_ (liftIO . print <$> send)
  performEvent_ (liftIO . print <$> recieved)

  return
    ( socket ^. webSocket_open
    , close <$> socket ^. webSocket_close
    , recieved)


  where
    close (True,  _, _)       = Nothing
    close (False, _, reason)  = Just reason


handleHistory :: GhcjsBuilder t m => Event t DocName -> m (Event t DocName)
handleHistory loaded = mdo

  currentDoc <- hold Nothing (Just <$> leftmost [loaded, changes])

  let update     = id <?> (updateHistory <$> current history <*> currentDoc <@> loaded)
      changes    = uriDocument <$> updated history

  history <- manageHistory update
  return $ new <?> (currentDoc `attach` changes)

   where
     new (Nothing, doc)       = Just doc
     new (Just previous, doc) = doc <$ guard (previous /= doc)

     uriDocument = T.pack . drop 1 . view #uriFragment . _historyItem_uri

     updateHistory item Nothing doc = Just $ HistoryCommand_ReplaceState $ update item doc
     updateHistory item (Just previous) doc
        | previous /= doc = Just $ HistoryCommand_PushState $ update item doc
        | otherwise       = Nothing

     update (HistoryItem state uri) doc = HistoryStateUpdate
        { _historyStateUpdate_state = state
        , _historyStateUpdate_title = ""
        , _historyStateUpdate_uri   = Just $ uri & #uriFragment .~ T.unpack ("#" <> doc)
        }


sceneWidget :: (GhcjsBuilder t m, EventWriter t AppCommand m)
            => Event t AppCommand -> (DocName, DocInfo, Document) -> m (Dynamic t Action)
sceneWidget cmds (name, info, loaded)  = mdo
  document <- holdDyn loaded updatedDoc
  let (updatedDoc, updates) = split . filterMaybe $
        flip applyCmd <$> current document <@> (_DocCmd ?> cmds)

  objects <- holdIncremental initial (attachInfo <$> currentIncremental objects <@> updates)

  raw   <- inputs element
  viewport <- viewControls cmds info
  input <- holdInputs (current viewport) raw

  nextId <- holdDyn (maybe 0 (+1) (maxId loaded)) never

  (element, action) <- sceneView $ Scene
    { image    = ("images/" <> name, info ^. #imageSize)
    , viewport = viewport
    , input    = input
    , document = document

    , initial = initial
    , objects  = objects

    , nextId = nextId
    , currentClass = pure 0
    }

  return action
    where initial = (def,) <$> (loaded ^. #instances)

attachInfo :: Map ObjId (ObjectInfo, Object) -> Map ObjId (Maybe Object) -> PatchMap ObjId (ObjectInfo, Object)
attachInfo objects updates = PatchMap (M.mapWithKey f updates) where
    f k v = (fromMaybe def (fst <$> M.lookup k objects),) <$> v


docMeta :: (DocName, DocInfo, Document) -> (DocName, DocInfo)
docMeta (name, info, _) = (name, info)

bodyWidget :: forall t m. GhcjsBuilder t m => Text -> m ()
bodyWidget host = mdo

  (opened, closed, serverMsg) <- network host clientMsg

  let disconnected = Workflow $
        (("disconnected", never), connected <$ opened) <$ connectingModal

      connected = Workflow $ do
        return (("connected", never), ready <$> hello)

      ready (clientId, collection) = Workflow $ do
        nextCmd <- postCurrent $ ffor (current currentDoc) $ \case
          Nothing        -> ClientNext Nothing
          Just (name, _) -> ClientOpen name -- If we were disconnected, load the previous document again

        return (("ready", nextCmd), never)

  (state, clientMsgs) <- split <$> (workflow $
    commonTransition (disconnected <$ closed) disconnected)

  let clientMsg = leftmost
        [ switchPrompt clientMsgs
        , ClientOpen <$> urlSelected
        ]

  urlSelected <- handleHistory (view _1 <$> loaded)
  currentDoc  <- holdDyn Nothing (Just . docMeta <$> loaded)

  setTitle $ ffor currentDoc $ \doc ->
    "Annotate - " <> fromMaybe ("no document") (fst <$> doc)

  let hello   = preview _ServerHello <?> serverMsg
      loaded  = preview _ServerDocument <?> serverMsg

  (action, cmds) <- runEventWriterT $ cursorLock action $ do
    action <- replaceHold (return def) $ (sceneWidget cmds <$> loaded)
    interface
    return action

  return ()


cursorLock :: Builder t m => Dynamic t Action -> m a -> m a
cursorLock action child =
  div [class_ =: "expand", draggable_ =: False, style_ ~: cursorStyle <$> action] $
    div [id_ =: "drawing", classes_ ~: lockClass <$> action] child
  where
    cursorStyle Action{..} = [("cursor", cursor)]
    lockClass Action{..} = ["expand"] <> ["cursor-lock" | lock]


    -- div [class_ =: "card"] $
    --   column "p-2" $ do
    --     h5 [] $ text "Image.jpeg (800x600)"
    --     text "modified 2 days ago, training"




-- Main interface
interface :: AppBuilder t m => m ()
interface = column "expand disable-cursor" $ do

  toolCmds <- row "p-2" $ do
    a [class_ =: "navbar-brand"] $ text "Annotate"
    spacer
    command' DetectCmd $ toolButton "Detect" "fa-magic" "Detect objects using current trained model"

  spacer

  row "p-2" $ do
    spacer
    buttonGroup $ do
      command' DiscardCmd $ toolButton "Discard" "fa-trash" "Discard image from the collection"
      command' NextCmd    $ toolButton "Next" "fa-step-forward" "Skip to the next image"
      command' SubmitCmd  $ toolButton "Submit" "fa-save" "Submit image for training"
