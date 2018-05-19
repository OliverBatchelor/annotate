module Window where

import Reflex.Dom

import qualified JSDOM as DOM
import qualified JSDOM.EventM as DOM

import qualified JSDOM.Window as Window
import qualified JSDOM.GlobalEventHandlers as DOM
import Language.Javascript.JSaddle (MonadJSM)


windowEvent :: (TriggerEvent t m, MonadJSM m) => m (Event t ())
windowEvent = do
  window <- DOM.currentWindowUnchecked
  wrapDomEvent window (`DOM.on` DOM.blur) (return ())
