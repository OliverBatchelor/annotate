
module Scene.Canvas where

  import Reflex.Classes
  

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
  



type DrawState = (Viewport, Action, Maybe Editor)


translate :: Vec -> Render ()
translate (V2 x y) = C.translate context tx ty

inViewport :: MonadJSM m => Viewport -> Render a -> Render a
inViewport context vp = do 

  transform = do 
    C.translate context tx ty
    C.scale context zoom zoom 
      where
        (V2 tx ty) = localOffset vp
        zoom = vp ^. #zoom



drawAnnotation :: MonadJSM m => CanvasRenderingContext2D -> Annotation -> m ()
drawAnnotation context Annotation{shape} = case shape of
  (ShapeCircle c) -> circleShape context c
  (ShapeBox b) -> error "not implemented"
  (ShapeLine l) -> error "not implemented"
  (ShapePolygon p) -> error "not implemented"


drawImageAt :: DOM.HTMLImageElement -> Render ()
drawImageAt image (V2 x y) = render $ \context -> do
  complete <- DOM.getComplete image
  when complete $
    C.drawImage context (DOM.toCanvasImageSource image) x y


setLineWidth :: Float -> Render ()
setLineWidth = render . flip C.setLineWidth


drawAnnotations :: Viewport -> DOM.HTMLImageElement -> Editor -> Render ()
drawAnnotations vp image Editor{annotations} = inViewport vp $ do 
    drawImageAt (V2 0 0) image

    setLineWidth (2 / vp ^. #zoom)
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
