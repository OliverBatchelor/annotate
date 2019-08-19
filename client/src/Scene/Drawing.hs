
module Scene.Drawing 
  ( module Scene.Drawing
  , Render(..)
    
  ) where

import Reflex.Classes

import Scene.Viewport
import Scene.Types

import Client.Common

import Annotate.Common
import Annotate.Geometry
import Builder.Html

import Annotate.Prelude
import Scene.Canvas
  
import Data.Coerce
import qualified GHCJS.DOM.Types as DOM

import qualified Data.Map as Map

selectRect :: Box -> Render ()
selectRect box = pushState $ do
  setLineDash([5, 5])
  setLineWidth 1
  setFillStyle "rgba(0, 0, 0, 0.1)"

  rect box
  fill >> nonScalingStroke



renderViewport :: Viewport -> Render a -> Render a
renderViewport vp render = pushState $ 
  translate (localOffset vp) >> scaleUniform (vp ^. #zoom) >> render


rawCanvas :: (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace) => ElemType t m -> m DOM.HTMLCanvasElement
rawCanvas e = return $ coerce (_element_raw e)



drawAnnotation :: Annotation -> Maybe AnnParts -> Maybe AnnParts -> Render ()
drawAnnotation Annotation{shape} selected highlight = case shape of
  (ShapeCircle c) -> do 
    circle c 
    pushState $ do 
      resetTransform 

      traverse_ (const $ setLineWidth 2) highlight
      stroke

      traverse_ (const $ fill) selected
    

  (ShapeBox b) -> error "not implemented"
  (ShapeLine l) -> error "not implemented"
  (ShapePolygon p) -> error "not implemented"




drawAnnotations :: Viewport -> DOM.HTMLImageElement -> Editor -> Action -> DocParts -> Render ()
drawAnnotations vp image Editor{annotations} Action{overlay, highlight}  selection = renderViewport vp $ do 
    drawImageAt image (V2 0 0)

    setLineWidth 1
    itraverse_ drawAnnotation' annotations

    overlay

  where
    drawAnnotation' k v = drawAnnotation v (Map.lookup k selection) (Map.lookup k highlight)

sceneCanvas :: forall t m. (GhcjsAppBuilder t m)
  => Dynamic t Dim
  -> Dynamic t (Render ())
  -> m (ElemType t m)
sceneCanvas size render  = do 
  e <- canvas_ [class_ =: "expand"]
  canvas <- rawCanvas e

  renderCanvas canvas state
  return e

    where 
      state  = liftA2 (,) size render

