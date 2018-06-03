{-# LANGUAGE OverloadedStrings #-}

import Common hiding (div)

import qualified Data.Text as T

import Data.Default
import Data.Monoid

import Scene.Viewport
import Scene.View
import Scene.Types
import Scene.Events

import Input.Events

import Reflex.Classes
import Builder.Html

import Input.Window

import Types




main :: IO ()
main = mainWidgetWithHead' (const headWidget, bodyWidget)


orLocal :: Text -> Text
orLocal url =  if url == "" then "localhost:3000" else url

headWidget :: GhcjsBuilder t m => m Text
headWidget = do

   host <- orLocal <$> getLocationHost
   base_ [href_ =: "http://" <> host]

   stylesheet "css/fontawesome-all.min.css"
   stylesheet "css/bootstrap.min.css"
   stylesheet "css/style.css"

   return host

  where
    stylesheet url = link_ [href_ =: url, rel_ =: ["stylesheet"]]
    --css = decodeUtf8 $(embedFile "style.css")

nonEmpty :: [a] -> Maybe [a]
nonEmpty = \case
  [] -> Nothing
  xs -> Just xs


cursorLock :: Builder t m => Dynamic t Action -> m a -> m a
cursorLock action child =
  div [draggable_ =: False, style_ ~: cursorStyle <$> action] $
    div [id_ =: "drawing", classes_ ~: lockClass <$> action] child
  where
    cursorStyle Action{..} = [("cursor", cursor)]
    lockClass Action{..} = ["expand"] <> ["cursor_lock" | lock]


viewControls :: GhcjsBuilder t m => Event t Command -> DocInfo -> m (Dynamic t Viewport)
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

  return
    ( socket ^. webSocket_open
    , close <$> socket ^. webSocket_close
    , decodeStrict <?> socket ^. webSocket_recv)


  where
    close (True,  _, _)       = Nothing
    close (False, _, reason)  = Just reason


-- network :: Text -> Event t Command ->
-- data ClientState = Connected
--   { clientId :: ClientId
--   , currentDoc  :: Maybe (DocName, DocInfo)
--
--   }

gated :: Reflex t => Monoid a => a -> Dynamic t Bool -> Dynamic t a
gated a d = ffor d $ \cond -> if cond then a else mempty

modal :: Builder t m => Dynamic t Bool -> m a -> m a
modal shown content = do
  r <- div [classes_ ~: (pure ["modal"] <> ["show"] `gated` shown), role_ =: "dialog", style_ ~: [("display", "block"), ("padding-right", "15px")] `gated` shown ] $
    div [class_ =: "modal-dialog modal-dialog-centered", role_ =: "document"] $
      div [class_ =: "modal-content"] $ content

  div [classes_ ~: (pure ["modal-backdrop"] <> ["show"] `gated` shown)] blank
  return r


connectingModal :: Builder t m => m ()
connectingModal = modal (pure True) $ do
  div [class_ =: "modal-header"] $ do
    h5 [class_ =:"modal-title"] $ text "Connecting..."


mapTransition ::  Builder t m => (Event t (Workflow t m a) -> Event t (Workflow t m a)) -> Workflow t m a -> Workflow t m a
mapTransition f (Workflow m) = Workflow (over _2 f' <$> m) where
  f' e = mapTransition f <$> f e

bodyWidget :: forall t m. GhcjsBuilder t m => Text -> m ()
bodyWidget host = mdo

  (opened, closed, serverMsg) <- network host (switch clientMsg)

  let disconnected = Workflow $ 
        (("disconnected", never), connected <$ opened) <$ connectingModal

      connected = Workflow $ do
        return (("connected", never), ready <$> hello)

      ready (clientId, collection) = Workflow $ do

        postBuild <- getPostBuild
        let nextCmd = getNext <$> current header <@ postBuild
            getNext  = \case
              Nothing -> ClientNext Nothing
              Just (name, _) -> ClientOpen name -- If we were disconnected, load the previous document again

        return (("ready", nextCmd), never)

  (state, clientMsg) <- split <$> (workflow $
    mapTransition (\e -> leftmost [disconnected <$ closed, e]) disconnected)

  dynText state

  let docLoaded = preview _ServerDocument <?> serverMsg
      hello = preview _ServerHello <?> serverMsg

  header <- holdDyn Nothing (docLoaded <&> \(name, info, _) -> Just (name, info))

  (action, cmds) <- cursorLock action $ do
    replaceHold (return (pure def, never)) $
      ffor docLoaded $ \(name, info, loaded) -> mdo

        document <- holdDyn loaded never
        input    <- sceneInputs scene
        viewport <- viewControls cmds info

        let initial :: Map ObjId (ObjectInfo, Object) = (def,) <$> (loaded ^. #instances)
            updates = never

        (scene, result) <- sceneView $ Scene
          { image    = (name, info ^. #imageSize)
          , viewport = viewport
          , input    = input
          , document = document
          , objects  = (initial, updates)
          }

        return result

  return ()
