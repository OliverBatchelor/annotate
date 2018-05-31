module Scene.View where

import Common

import Reflex.Classes
import Builder.Svg hiding (switch, cursor)

import qualified Builder.Svg as Svg

import Input.Events
import Scene.Events
import Scene.Types
import Scene.Viewport
import Geometry

type Builder t m = (Adjustable t m, MonadHold t m, DomBuilder t m, MonadFix m, PostBuild t m)

transforms :: Viewport -> [Svg.Transform]
transforms vp = [Translate tx ty, Scale zoom zoom] where
    (V2 tx ty) = localOffset vp
    zoom = vp ^. #zoom

inViewport :: (Builder t m) => Dynamic t Viewport -> m a -> m (ElemType t m, a)
inViewport vp child = svg' [class_ =: "expand", version_ =: "2.0"] $
  g [transform_ ~: transforms <$> vp] child

boxView :: (Builder t m) => [Property t] -> Box -> m (ElemType t m)
boxView props (Box l u) = rect_ (props <> [x_ =: x, y_ =: y, width_ =: w, height_ =: h]) where
  (V2 w h) = u - l
  (V2 x y) = l


holdChanges :: (Reflex t, MonadHold t m) => a -> Event t a -> m (Event t (a, a))
holdChanges initial e = flip attach e <$> hold initial e


actions :: (Builder t m)
        => Scene t -> m (Dynamic t (Action, Event t Command))
actions scene = workflow base where

  base = Workflow $ do

    let beginPan = pan <$> (local <@> mouseDown LeftButton)
    return ((def, zoomCmd), leftmost [beginPan])

  pan (localOrigin :: V2 Float) = Workflow $ do
    let action = (def :: Action) & #cursor .~ "move" & #lock .~ True
        panCmd = ViewCmd . PanCmd localOrigin <$> updated mouse

    return ((action, leftmost [panCmd, zoomCmd]), base <$ mouseUp LeftButton)

  zoomCmd = traceEvent "foo" $ (ViewCmd <$> attachWith (flip ZoomCmd) (local <*> current mouse) wheel)

  local      = toLocal' scene
  Inputs{..} = scene ^. #input


sceneView :: Builder t m
          => Scene t -> m (ElemType t m, (Event t Command, Dynamic t Action))
sceneView scene = inViewport (scene ^. #viewport) $ do
    dyn (imageView <$> (scene ^. #image))

    (action, cmds) <- split <$> actions scene
    --   boxView [class_ =: "object"] (Box (V2 100 100) (V2 200 400))

    return (switch cmds, action)

    where

      imageView (file, (w, h)) = void $ Svg.image_
        [width_ =: fromIntegral w, height_ =: fromIntegral h, href_ =: file]
