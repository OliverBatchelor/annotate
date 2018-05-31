module Input.Events
  ( module Input.Events
  , Key
  ) where

import Common

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

import Types

data Inputs t = Inputs
  { mouseDown :: Button -> Event t Position
  , mouseUp   :: Button -> Event t Position
  , click     :: Button -> Event t Position
  , wheel     :: Event t Float
  , focus     :: Event t Bool
  , keyDown   :: Event t Key
  , keyUp     :: Event t Key

  , keys  :: Dynamic t (Set Key)
  , mouse :: Dynamic t Position
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

keyboard :: (MonadFix m, Reflex t, MonadHold t m) => Event t Bool -> (Event t Key, Event t Key) -> m (Dynamic t (Set Key))
keyboard focus (keyDown, keyUp) = foldDyn ($) S.empty $ mergeWith (.)
    [ const mempty <$ focus
    , S.insert     <$> keyDown
    , S.delete     <$> keyUp
    ]


sceneInputs :: (DomBuilderSpace m ~ GhcjsDomSpace, Reflex t, MonadFix m, MonadHold t m, TriggerEvent t m, MonadJSM m)
            => ElemType t m -> m (Inputs t)
sceneInputs scene = do
  window <- DOM.currentWindowUnchecked

  mouseMove  <- wrapDomEvent window    (`DOM.on` DOM.mouseMove)
    (fromDim <$> DOM.uiPageXY)

  mouse <- holdDyn (V2 0 0) mouseMove

  let selectButton e b = tag (current mouse) (guard . (== b) <?> e)

  mouseDown <- selectButton <$> wrapDomEvent se (`DOM.on` DOM.mouseDown)
    (toButton <$> DOM.mouseButton)

  mouseUp    <- selectButton <$> wrapDomEvent window    (`DOM.on` DOM.mouseUp)
    (toButton <$> DOM.mouseButton)

  click  <- selectButton <$> wrapDomEvent window    (`DOM.on` DOM.click)
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
  keys <- keyboard focus (keyDown, keyUp)

  return  Inputs{..}
    where

      se = DOM.uncheckedCastTo DOM.HTMLElement $ _element_raw scene
