module Scene.Viewport where

import Annotate.Common
import Annotate.Types
import Control.Lens hiding (zoom)

import Scene.Types

import Debug.Trace


panView :: Position -> Position -> Viewport -> Viewport
panView localOrigin page view = view & #pan %~ (+ d)
  where d  = toLocal view page - localOrigin

wheelZoom :: Float -> Float
wheelZoom delta = 1 - delta / 500

zoomDelta :: Float -> Viewport -> Viewport
zoomDelta delta = #zoom %~ clamp (0.25, 4) . (* wheelZoom delta)

zoomView :: Float -> Position -> Viewport -> Viewport
zoomView amount localOrigin view = panView localOrigin page (zoomDelta amount view)
  where page = toPage view localOrigin


toPage :: Viewport -> Position -> Position
toPage view local = localOffset view + zoom view *^ local

toLocal :: Viewport -> Position -> Position
toLocal view page = (page - localOffset view) ^/ zoom view


localOffset :: Viewport -> Position
localOffset Viewport{..} = pan ^* zoom + 0.5 *^ (window - zoom *^ image)
