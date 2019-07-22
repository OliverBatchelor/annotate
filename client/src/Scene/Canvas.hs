module Scene.Canvas where

import Reflex.Classes
import Builder.Element (ElemType)

import qualified GHCJS.DOM as DOM 
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.EventM as DOM
import qualified GHCJS.DOM.Element as DOM

import qualified GHCJS.DOM.HTMLElement as DOM
import qualified GHCJS.DOM.HTMLImageElement as DOM (getComplete)
import qualified GHCJS.DOM.HTMLCanvasElement as DOM
import GHCJS.DOM.Types (CanvasStyle(..), CanvasRenderingContext2D(..), toJSString, RenderingContext)

import Language.Javascript.JSaddle as JS
       

import Scene.Viewport
import Scene.Controller
import Scene.Types
import Scene.Events

import Client.Common
import Client.Widgets

import Annotate.Common
import Annotate.Geometry
import Builder.Html

import Annotate.Prelude

import GHCJS.DOM.CanvasPath               as C
import GHCJS.DOM.CanvasRenderingContext2D as C

import  Data.Coerce  (coerce)


rawCanvas :: (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace) => ElemType t m -> m DOM.HTMLCanvasElement
rawCanvas e = return $ coerce (_element_raw e)


getContext2d ::  (MonadJSM m) =>  DOM.HTMLCanvasElement -> m CanvasRenderingContext2D
getContext2d e =  coerce <$> DOM.getContextUnchecked e ("2d" :: Text) ([] :: [DOM.JSVal])


type DrawState = (Viewport, Action, Maybe Editor)


transformView :: MonadJSM m => CanvasRenderingContext2D -> Viewport ->  m ()
transformView context vp = do 
  C.translate context tx ty
  C.scale context zoom zoom 
    where
      (V2 tx ty) = localOffset vp
      zoom = vp ^. #zoom


drawCircle :: MonadJSM m => CanvasRenderingContext2D -> Circle -> m ()
drawCircle context (Circle (V2 x y) r) = do
    C.beginPath context
    C.arc context (realToFrac x) (realToFrac y) (realToFrac r) 0 (2 * pi) False
    C.stroke context


drawAnnotation :: MonadJSM m => CanvasRenderingContext2D -> Annotation -> m ()
drawAnnotation context Annotation{shape} = case shape of
  (ShapeCircle c) -> drawCircle context c
  (ShapeBox b) -> error "not implemented"
  (ShapeLine l) -> error "not implemented"
  (ShapePolygon p) -> error "not implemented"


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


buffered :: MonadJSM m => DOM.HTMLCanvasElement -> (CanvasRenderingContext2D -> m ()) -> m ()
buffered canvas paint = void $ do
  size <- getSize canvas
  
  offscreen <- offscreenCanvas size
  offscreenContext offscreen >>= paint 

  transferBitmap offscreen canvas

    




-- withContextOffscreen :: MonadJSM m => DOM.HTMLCanvasElement -> (CanvasRenderingContext2D -> m ()) -> m ()
-- withContextOffscreen canvas paint = void $ do
--   -- doc <- currentDocumentUnchecked

--   (w, h) <- getSize canvas
--   offScreen <- liftJSM $ JS.new (JS.jsg ("OffscreenCanvas" :: DOM.JSString)) (w, h)

--   getContext2d (coerce offScreen) >>= paint



drawScene :: MonadJSM m => DOM.HTMLCanvasElement -> DOM.HTMLImageElement -> DrawState -> m ()
drawScene canvas image (vp, action, doc)   = do
  
  size <- getSize canvas
  when (size /= vp ^. #window) $
    setSize canvas (vp ^. #window)


  buffered canvas $ \context -> do
    -- C.resetTransform context
    -- C.clearRect context 0 0 (fromIntegral w) (fromIntegral h)

    transformView context vp   

    complete <- DOM.getComplete image
    when complete $
      C.drawImage context (DOM.toCanvasImageSource image) 0 0
      
    C.setLineWidth context (2 / vp ^. #zoom)

    forM_ doc $ \Editor{annotations} ->    
      traverse (drawAnnotation context) annotations


  

loadImage :: (AppBuilder t m) => Dynamic t Text -> m DOM.HTMLImageElement 
loadImage file =  do
  e <- preload file
  return $ coerce (_element_raw e)



sceneCanvas :: forall t m. (GhcjsAppBuilder t m)
  => Dynamic t Viewport
  -> Dynamic t Action
  -> Dynamic t (Maybe Editor)
  -> m ()
sceneCanvas viewport action mDoc = do 
  
  e <- canvas_ [class_ =: "expand"]

  image <- loadImage (pure "CamB_1755.jpg")--(fromMaybe "" . fmap (view #name) <$> mDoc)
  canvas <- rawCanvas e

  vp <- sample viewport
  DOM.liftJSM $ setSize canvas (vp ^. #window)
  
  
  requestDomAction_ (drawScene canvas image <$> updated state)
    where state = liftA3 (,,) viewport action mDoc



