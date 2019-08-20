module Scene.Viewport where

import Annotate.Prelude
import Annotate.Common
import Control.Lens hiding (zoom)

import Scene.Types

import Debug.Trace


panView :: Point -> Point -> Viewport -> Viewport
panView localOrigin page view = view & #pan %~ (+ d)
  where d  = toLocal view page - localOrigin

wheelZoom :: Bool -> Float -> Float
wheelZoom fine delta = (1 - delta' / 500) where
  delta' = delta * (if fine then 0.3 else 1.0)


zoomDelta :: Float -> Viewport -> Viewport
zoomDelta delta = #zoom %~ clamp (0.25, 4) . (* delta)

zoomView :: Float -> Point -> Viewport -> Viewport
zoomView amount localOrigin view = panView localOrigin page (zoomDelta amount view)
  where page = toPage view localOrigin


toPage :: Viewport -> Point -> Point
toPage view local = localOffset view + zoom view *^ local

toLocal :: Viewport -> Point -> Point
toLocal view page = (page - localOffset view) ^/ zoom view


localOffset :: Viewport -> Point
localOffset Viewport{..} = pan ^* zoom + 0.5 *^ (fromDim window - zoom *^ fromDim image)

