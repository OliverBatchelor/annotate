module Input.Window where

import Annotate.Prelude
import Reflex.Dom
import Builder.Element

import Control.Applicative

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.EventM as DOM

import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.Document as Document


import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import Language.Javascript.JSaddle (MonadJSM)


windowEvent :: (TriggerEvent t m, MonadJSM m) => m (Event t ())
windowEvent = do
  window <- DOM.currentWindowUnchecked
  wrapDomEvent window (`DOM.on` DOM.blur) (return ())

getDim :: MonadJSM m => m (Int, Int)
getDim = do
  window <- DOM.currentWindowUnchecked
  liftA2 (,) (Window.getInnerWidth window) (Window.getInnerHeight window)


windowDimensions :: (MonadHold t m, TriggerEvent t m, MonadJSM m) => m (Dynamic t (Int, Int))
windowDimensions = do
  initial <- getDim
  window <- DOM.currentWindowUnchecked

  e       <- wrapDomEvent window (`DOM.on` DOM.resize) getDim
  holdDyn initial e

setTitle :: (PostBuild t m, MonadJSM m, Reflex t, PerformEvent t m, MonadJSM (Performable m)) => Dynamic t Text -> m ()
setTitle d = do
  doc <- DOM.currentDocumentUnchecked

  postBuild <- getPostBuild
  performEvent_ (DOM.liftJSM . Document.setTitle doc <$> leftmost [updated d, current d <@ postBuild])
  

