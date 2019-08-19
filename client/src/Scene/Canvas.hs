module Scene.Canvas where

import qualified GHCJS.DOM as DOM 
import qualified GHCJS.DOM.Types as DOM

import qualified GHCJS.DOM.HTMLCanvasElement as DOM
import qualified GHCJS.DOM.HTMLImageElement as DOM (getComplete, setSrc, getSrc)
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.GlobalEventHandlers as DOM

import qualified GHCJS.DOM.Enums as DOM


import GHCJS.DOM.Types (CanvasStyle(..), CanvasRenderingContext2D(..), toJSString, RenderingContext)

import qualified  GHCJS.DOM.CanvasPath               as C
import qualified  GHCJS.DOM.CanvasRenderingContext2D as C

import Language.Javascript.JSaddle as JS

import Annotate.Geometry
import Annotate.Prelude

import Control.Monad.Reader
import  Data.Coerce  (coerce)

import Builder.Element (ElemType)
import Reflex.Classes


renderCanvas :: (MonadJSM m, DomRenderHook t m, MonadHold t m, PostBuild t m) => DOM.HTMLCanvasElement -> Dynamic t (Dim, Render ()) -> m ()
renderCanvas canvas state = do
  -- sample state >>= r canvas
  postBuild <- getPostBuild
  requestDomAction_ $ r canvas <$> leftmost 
    [updated state, current state `tag` postBuild]
    
    where 
      r canvas (dim, render) = do 
        size <- getSize canvas
        when (size /= dim) $ 
          setSize canvas dim     
        renderBuffered canvas render


-- loadImage :: (TriggerEvent t m, MonadJSM m) => (Text, Dim) -> m (DOM.HTMLImageElement, Event t ())
-- loadImage (url, dim) = do
--   image <- DOM.HTMLImageElement <$> 
--     liftJSM (JS.new (JS.jsg (str "Image")) dim)

--   DOM.setSrc image url
--   e <- wrapDomEvent image (`DOM.on` DOM.load) (pure ())
--   return (image, e)




loadImage :: (TriggerEvent t m, MonadJSM m) => (Text, Dim) -> m (DOM.HTMLImageElement)
loadImage (url, dim) = do
  image <- DOM.HTMLImageElement <$> 
    liftJSM (JS.new (JS.jsg (str "Image")) dim)

  DOM.setSrc image url
  return image


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

instance Default (Render ()) where
  def = pure ()

instance Monoid (Render ()) where
  mempty = pure ()

instance Semigroup (Render ()) where
  (<>) r r' = r >> r'
  


render :: (CanvasRenderingContext2D -> JSM a) -> Render a
render f = Render $ ask >>= lift . f


runRender :: MonadJSM m => CanvasRenderingContext2D -> Render a -> m a
runRender context = liftJSM . flip runReaderT context . unRender


renderBuffered :: MonadJSM m => DOM.HTMLCanvasElement -> Render a -> m a
renderBuffered canvas = buffered canvas . flip runRender

moveTo :: Vec -> Render ()
moveTo (V2 x y) = render $ \context -> C.moveTo context (realToFrac x) (realToFrac y)


arc :: Circle -> (Float, Float) -> Bool -> Render ()
arc (Circle (V2 x y) r) (begin, end) clockwise = render $ \context -> 
  C.arc context (realToFrac x) (realToFrac y) (realToFrac r) (realToFrac begin) (realToFrac end) clockwise

circle :: Circle -> Render ()
circle c = arc c (0, pi * 2) False


pushState :: Render a -> Render a 
pushState r = render C.save >> r <* render C.restore


strokePath :: Render a -> Render a 
strokePath r = beginPath >> r <* render C.stroke


beginPath :: Render ()
beginPath = render C.beginPath

nonScalingStrokePath :: Render a -> Render a 
nonScalingStrokePath r = beginPath >> r <* nonScalingStroke


fillPath :: Render a -> Render a 
fillPath r = beginPath >> r <* render (flip C.fill Nothing)



rect :: Box -> Render ()
rect (Box l u) = render $ \context ->
  C.rect context (realToFrac x) (realToFrac y) (realToFrac w) (realToFrac h) 
    where 
      (V2 x y) = l
      (V2 w h) = u - l

strokeRect :: Box -> Render ()
strokeRect (Box l u) = render $ \context ->
  C.strokeRect context x y w h 
    where 
      (V2 x y) = l
      (V2 w h) = u - l

fillRect :: Box -> Render ()
fillRect (Box l u) = render $ \context ->
  C.fillRect context x y w h 
    where 
      (V2 x y) = l
      (V2 w h) = u - l
         


translate :: Vec -> Render ()
translate (V2 tx ty) = render $ \context -> C.translate context tx ty

scale :: Vec -> Render ()
scale (V2 sx sy) = render $ \context -> C.scale context sx sy


scaleUniform :: Float -> Render ()
scaleUniform s = scale (V2 s s)



drawImageAt :: DOM.HTMLImageElement -> Vec -> Render ()
drawImageAt image (V2 x y) = render $ \context -> do
  complete <- DOM.getComplete image
  when complete $
    C.drawImage context (DOM.toCanvasImageSource image) x y


setLineWidth :: Float -> Render ()
setLineWidth = render . flip C.setLineWidth

setLineDash ::  [Float] -> Render ()
setLineDash = render . flip C.setLineDash

setFillStyle ::  Text -> Render ()
setFillStyle = render . flip C.setFillStyle . toJSString

setFillColorRGB :: (Float, Float, Float, Float) -> Render ()
setFillColorRGB (r, g, b, a) = render $ \context -> 
    C.setFillColorRGB context (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)


resetTransform :: Render ()
resetTransform = render C.resetTransform

stroke :: Render ()
stroke = render C.stroke

fill' :: DOM.CanvasWindingRule -> Render ()
fill' rule = render (flip C.fill (Just rule))

fill :: Render ()
fill = fill' DOM.CanvasWindingRuleNonzero

nonScalingStroke :: Render ()
nonScalingStroke = pushState $ resetTransform >> stroke
