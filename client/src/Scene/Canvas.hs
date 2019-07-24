module Scene.Canvas where

import qualified GHCJS.DOM as DOM 
import qualified GHCJS.DOM.Types as DOM

import qualified GHCJS.DOM.HTMLCanvasElement as DOM
import GHCJS.DOM.Types (CanvasStyle(..), CanvasRenderingContext2D(..), toJSString, RenderingContext)

import Language.Javascript.JSaddle as JS

import Annotate.Geometry
import Annotate.Prelude

import GHCJS.DOM.CanvasPath               as C
import GHCJS.DOM.CanvasRenderingContext2D as C

import Control.Monad.Reader
import  Data.Coerce  (coerce)

import Builder.Element (ElemType)


rawCanvas :: (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace) => ElemType t m -> m DOM.HTMLCanvasElement
rawCanvas e = return $ coerce (_element_raw e)


getContext2d ::  (MonadJSM m) =>  DOM.HTMLCanvasElement -> m CanvasRenderingContext2D
getContext2d e =  coerce <$> DOM.getContextUnchecked e ("2d" :: Text) ([] :: [DOM.JSVal])


withContext :: MonadJSM m => DOM.HTMLCanvasElement -> (CanvasRenderingContext2D -> m a) -> m a
withContext canvas f = do
  context <- getContext2d canvas
  f context

getSize :: MonadJSM m => DOM.HTMLCanvasElement -> m (Word, Word)
getSize canvas = liftA2 (,) (DOM.getWidth canvas) (DOM.getHeight canvas)

setSize :: (DOM.MonadJSM m) => DOM.HTMLCanvasElement -> (Word, Word) -> m ()
setSize canvas (w, h) =  DOM.setWidth canvas w >> DOM.setHeight canvas h

str :: DOM.JSString -> DOM.JSString
str = id

newtype OffscreenCanvas = OffscreenCanvas { unOffscreenCanvas :: JSVal } 
  deriving (PToJSVal, ToJSVal, PFromJSVal, MakeObject)


offscreenCanvas :: MonadJSM m => (Word, Word) -> m OffscreenCanvas
offscreenCanvas dim = OffscreenCanvas <$> (liftJSM $ 
  JS.new (JS.jsg (str "OffscreenCanvas")) dim)

offscreenContext :: MonadJSM m => OffscreenCanvas -> m CanvasRenderingContext2D
offscreenContext offscreen = CanvasRenderingContext2D <$> (liftJSM $ 
  offscreen ^. js1 (str "getContext") (str "2d"))


transferBitmap :: MonadJSM m => OffscreenCanvas -> DOM.HTMLCanvasElement -> m ()
transferBitmap offscreen (DOM.HTMLCanvasElement canvas) = void $ liftJSM $ do 
  bitmap     <- offscreen ^. js0  (str "transferToImageBitmap")
  context    <- canvas ^. js1 (str "getContext") (str "bitmaprenderer")
  context ^. js1 (str "transferFromImageBitmap") bitmap


buffered :: MonadJSM m => DOM.HTMLCanvasElement -> (CanvasRenderingContext2D -> m a) -> m a
buffered canvas paint = do
  offscreen <- offscreenCanvas =<< getSize canvas 
  a <- (offscreenContext offscreen >>= paint) 
  transferBitmap offscreen canvas

  return a


newtype Render a = Render { unRender :: ReaderT CanvasRenderingContext2D JSM a } 
  deriving (Functor, Applicative, Monad)

deriving instance MonadReader CanvasRenderingContext2D Render

render :: (CanvasRenderingContext2D -> JSM a) -> Render a
render f = Render $ ask >>= lift . f


runRender :: MonadJSM m => CanvasRenderingContext2D -> Render a -> m a
runRender context = liftJSM . flip runReaderT context . unRender


renderBuffered :: MonadJSM m => DOM.HTMLCanvasElement -> Render a -> m a
renderBuffered canvas = buffered canvas . flip runRender


-- arc :: Circle -> (Float, Float) -> Bool -> Render ()
-- arc (Circle (V2 x y) r) (begin, end) clockwise = reader $ \context -> return ()
  --C.arc context (realToFrac x) (realToFrac y) (realToFrac r) begin end clockwise


push :: Render a -> Render a 
push r = render C.save >> r <* render C.restore


strokePath :: Render a -> Render a 
strokePath r = render C.beginPath >> r <* render C.stroke


fillPath :: Render a -> Render a 
fillPath r = render C.beginPath >> r <* render (flip C.fill Nothing)



