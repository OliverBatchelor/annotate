{-# LANGUAGE OverloadedStrings #-}

import Common hiding (div)

import qualified Data.Text as T

import Data.FileEmbed
import Data.Text.Encoding (decodeUtf8)
import Data.Default
import Data.Monoid

import Scene.Viewport
import Scene.View
import Scene.Types
import Scene.Events

import Input.Events

import Reflex.Classes
import Builder.Html

import Input.Window

import Types

main :: IO ()
main = mainWidgetWithHead headWidget bodyWidget

headWidget = do
   style [] $ text bootstrap
   style [] $ text stylesheet

  where
    bootstrap = decodeUtf8 $(embedFile "css/bootstrap.min.css")
    stylesheet = decodeUtf8 $(embedFile "style.css")

nonEmpty :: [a] -> Maybe [a]
nonEmpty = \case
  [] -> Nothing
  xs -> Just xs


makeViewport :: Controls -> Image -> Dim -> Viewport
makeViewport (zoom, pan) (_, image) window =
    Viewport (fromDim image) (fromDim window) pan zoom

viewControls :: Viewport -> Controls
viewControls (Viewport _ _ pan zoom) = (zoom, pan)

bodyWidget = mdo
  dim   <- windowDimensions

  (cmds, action) <- div [draggable_ =: False, style_ ~: cursorStyle <$> action] $
    div [id_ =: "drawing", classes_ ~: lockClass <$> action] $ mdo

      image <- holdDyn ("/home/oliver/trees.jpg", (1600, 1064)) never

      let viewCmds = preview _ViewCmd <?> cmds
      controls <- holdDyn  (0.5, V2 0 0) (updateView <$> viewCmds <#> current viewport)

      let viewport = makeViewport <$> controls <*> image <*> dim
      (scene, (cmds, action)) <- sceneView $ Scene image viewport input

      input <- sceneInputs scene
      return (cmds, action)

  return ()

    where
      cursorStyle Action{..} = [("cursor", cursor)]
      lockClass Action{..} = ["expand"] <> ["cursor_lock" | lock]

      updateView cmd vp = viewControls $ case cmd of
        ZoomCmd zoom pos -> zoomView zoom pos vp
        PanCmd localOrign page -> panView localOrign page vp
