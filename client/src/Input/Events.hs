module Input.Events
  ( module Input.Events
  , Key
  ) where

import Annotate.Prelude
import Client.Common

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
import qualified GHCJS.DOM.Element as DOM
import qualified GHCJS.DOM.DOMRectReadOnly as DOM


import qualified GHCJS.DOM.WheelEvent as DOM

import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.GlobalEventHandlers as DOM
import Language.Javascript.JSaddle (MonadJSM)

import Annotate.Common

data Inputs t = Inputs
  { mouseDown :: Event t Button
  , mouseUp   :: Event t Button
  , click     :: Event t Button

  , wheel     :: Event t Float
  , focus     :: Event t Bool


  , localKey  :: Event t Key
  , keyDown   :: Event t Key
  , keyUp   :: Event t Key
  , keyPress :: Event t Key

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


getCoords :: MonadJSM m => DOM.HTMLElement -> m Box
getCoords e = DOM.liftJSM $ do
   rect <- DOM.getBoundingClientRect e
   
   pos <-  V2 <$> DOM.getX rect     <*> DOM.getY rect
   size <- V2 <$> DOM.getWidth rect <*> DOM.getY rect
   
   return $ Box (realToFrac <$> pos) (realToFrac <$> size)

rawElement :: (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace) => ElemType t m -> m DOM.HTMLElement
rawElement e = return $ DOM.uncheckedCastTo DOM.HTMLElement (_element_raw e)

pollBoundingBox :: (GhcjsBuilder t m, MonadJSM m) => ElemType t m -> m (Dynamic t Box)
pollBoundingBox e = do
  t0 <- liftIO getCurrentTime
  timer <- tickLossy (1.0/30.0) t0
  
  raw <- rawElement e
  updates <- performEvent (getCoords raw <$ timer)
  let initial = Box (V2 0 0) (V2 0 0)
  
  holdUniqDyn =<< holdDyn initial updates


onCapturing :: (DOM.IsEventTarget t, DOM.IsEvent e) => t -> DOM.EventName t e -> DOM.EventM t e () -> DOM.JSM (DOM.JSM ())
onCapturing target eventName callback = do
    l <- DOM.newListener callback
    DOM.addListener target eventName l True
    return $ do
      DOM.removeListener target eventName l True
      DOM.releaseListener l

    

windowInputs :: (GhcjsBuilder t m) => ElemType t m -> m (Inputs t)
windowInputs scene = do
  
  window <- DOM.currentDocumentUnchecked
  raw <- rawElement scene

  -- Mouse down events on the element
  mouseDown <- wrapDomEvent raw (`DOM.on` DOM.mouseDown)
    (toButton <$> DOM.mouseButton)

  click  <- wrapDomEvent raw  (`DOM.on` DOM.click)
    (toButton <$> DOM.mouseButton)

  -- Other events on the window
  mouseMove  <- wrapDomEvent window    (`DOM.on` DOM.mouseMove)
    (fromDim <$> DOM.uiPageXY)

  mouseUp    <- wrapDomEvent window    (`DOM.on` DOM.mouseUp)
    (toButton <$> DOM.mouseButton)

  wheel  <- wrapDomEvent raw    (`DOM.on` DOM.wheel) wheelNormalized

  focusIn <- wrapDomEvent window    (`DOM.on` DOM.focus) (return True)
  focusOut <- wrapDomEvent window   (`DOM.on` DOM.blur) (return False)


  localKey <- wrapDomEvent raw    (`DOM.on` DOM.keyDown)
      (keyCodeLookup . fromIntegral <$> getKeyEvent)

  keyDown <- wrapDomEvent raw    (`onCapturing` DOM.keyDown)
    (keyCodeLookup . fromIntegral <$> getKeyEvent)

  keyUp <- wrapDomEvent window    (`onCapturing` DOM.keyUp)
      (keyCodeLookup . fromIntegral <$> getKeyEvent)
 
  keyPress <- wrapDomEvent window    (`onCapturing` DOM.keyPress)
          (keyCodeLookup . fromIntegral <$> getKeyEvent)


  let focus = leftmost [focusIn, focusOut]
  return  Inputs{..}


wheelNormalized = do 
  e <- DOM.event 
  
  dy <- realToFrac <$> DOM.getDeltaY e
  mode <- DOM.getDeltaMode e
  return (dy * deltaSize mode)
  
    where 
      deltaSize = \case
        0 -> 1  --DOM_DELTA_PIXEL
        1 -> 20  --DOM_DELTA_LINE
        2 -> 400 --DOM_DELTA_PAGE

