module Input.Events
  ( module Input.Events
  , Key
  ) where

import Annotate.Common

import Reflex.Classes
import Builder.Element (ElemType)

import Control.Applicative
import Control.Lens (makePrisms)

import Control.Monad.Fix

import Web.KeyCode (Key, keyCodeLookup)
import qualified Data.Set as S
import Data.Default

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.EventM as DOM

import qualified GHCJS.DOM.WheelEvent as DOM

import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import Language.Javascript.JSaddle (MonadJSM)

import Annotate.Types

data Inputs t = Inputs
  { mouseDown :: Event t Button
  , mouseUp   :: Event t Button
  , click     :: Event t Button

  , wheel     :: Event t Float
  , focus     :: Event t Bool
  , keyDown   :: Event t Key
  , keyUp   :: Event t Key

  , mouseMove :: Event t Position
}


data Button
  = LeftButton
  | MiddleButton
  | RightButton
  | OtherButton Word
    deriving (Show, Eq, Ord, Generic)

toButton :: Word -> Button
toButton 0 = LeftButton
toButton 1 = MiddleButton
toButton 2 = RightButton
toButton n = OtherButton n

makePrisms ''Button


inputs :: (DomBuilderSpace m ~ GhcjsDomSpace, Reflex t, MonadFix m, MonadHold t m, TriggerEvent t m, MonadJSM m)
            => ElemType t m -> m (Inputs t)
inputs scene = do
  window <- DOM.currentWindowUnchecked

  mouseMove  <- wrapDomEvent window    (`DOM.on` DOM.mouseMove)
    (fromDim <$> DOM.uiPageXY)


  mouseDown <- wrapDomEvent se (`DOM.on` DOM.mouseDown)
    (toButton <$> DOM.mouseButton)

  mouseUp    <- wrapDomEvent window    (`DOM.on` DOM.mouseUp)
    (toButton <$> DOM.mouseButton)

  click  <- wrapDomEvent window    (`DOM.on` DOM.click)
    (toButton <$> DOM.mouseButton)

  wheel  <- wrapDomEvent window    (`DOM.on` DOM.wheel)
    (realToFrac <$> (DOM.event >>= DOM.getDeltaY))

  focusIn <- wrapDomEvent window    (`DOM.on` DOM.focus) (return True)
  focusOut <- wrapDomEvent window   (`DOM.on` DOM.blur) (return False)


  keyDown <- wrapDomEvent window    (`DOM.on` DOM.keyDown)
      (keyCodeLookup . fromIntegral <$> DOM.uiKeyCode)

  keyUp <- wrapDomEvent window    (`DOM.on` DOM.keyUp)
      (keyCodeLookup . fromIntegral <$> DOM.uiKeyCode)

  let focus = leftmost [focusIn, focusOut]


  return  Inputs{..}
    where

      se = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw scene
