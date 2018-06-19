module Scene.View where

import Annotate.Common

import Reflex.Classes
import Builder.Svg hiding (switch, cursor)

import qualified Builder.Svg as Svg

import Input.Events
import Scene.Events

import Web.KeyCode as Key

import Scene.Types
import Scene.Viewport

import Annotate.Geometry
import Annotate.Types
import Annotate.Document

transforms :: Viewport -> [Svg.Transform]
transforms vp = [Translate tx ty, Scale zoom zoom] where
    (V2 tx ty) = localOffset vp
    zoom = vp ^. #zoom

inViewport :: (Builder t m) => Text -> Dynamic t Viewport -> m a -> m (ElemType t m, a)
inViewport svgClass vp child = svg' [class_ =: svgClass, version_ =: "2.0"] $
  g [transform_ ~: transforms <$> vp] child

boxView :: (Builder t m) =>  Box -> m (ElemType t m)
boxView (Box l u) = rect_ [class_ =: "object", x_ =: x, y_ =: y, width_ =: w, height_ =: h] where
  (V2 w h) = u - l
  (V2 x y) = l


imageView :: (Builder t m) =>  [Property t] -> Image -> m (ElemType t m)
imageView props (file, (w, h)) = Svg.image_ $
  (props <> [width_ =: fromIntegral w, height_ =: fromIntegral h, href_ =: file])


holdChanges :: (Reflex t, MonadHold t m) => a -> Event t a -> m (Event t (a, a))
holdChanges initial e = flip attach e <$> hold initial e


type SceneAction t m = Workflow t m (Dynamic t Action)
type Cursor = Text


action :: AppBuilder t m => m (Dynamic t Cursor, Event t (SceneAction t m)) -> SceneAction t m
action m = Workflow $ over _1 (fmap f) <$> m
   where f cursor = Action cursor True

--  switchHold :: MonadHold t m => a -> Event t a -> m a

holdWorkflow :: forall t m a. (Reflex t, Adjustable t m, MonadFix m, MonadHold t m, SwitchHold t a) => Workflow t m a -> m a
holdWorkflow w0 = do
 rec (r, transition) <- replaceHold (unWorkflow w0) $ (unWorkflow <$> transition)
 return r

workflow' :: (Reflex t, MonadHold t m) => m (Event t (Workflow t m ())) -> Workflow t m ()
workflow' m = Workflow $ ((),) <$> m






addObject :: AppBuilder t m => Scene t -> Event t Object -> m ()
addObject Scene{nextId} add = tellEvent (addCommand <$> current nextId `attach` add)

addCommand :: (ObjId, Object) -> AppCommand
addCommand = DocCmd . DocEdit . Add . pure


drawBoxes :: AppBuilder t m => Scene t -> SceneInputs t -> m ()
drawBoxes scene SceneInputs{..} = do
  e <- filterMaybe <$> workflowView (idle Nothing)
  addObject scene (boxObject e)
  where
    idle r = Workflow $ do
      return (r, drawing <$> (current mouse <@ mouseDown LeftButton))

    drawing p1 = Workflow $ do
      let box = makeBox p1 <$> mouse
          done = current box <@ mouseUp LeftButton

      dyn (boxView <$> box)
      return (Nothing, idle . Just <$> done)

    makeBox p1 p2 = Box (liftI2 min p1 p2) (liftI2 max p1 p2)
    boxObject e = makeObject <$> current (scene ^. #currentClass) <@> e
    makeObject classId box = Object (BoxShape box) classId []



actions :: AppBuilder t m => Scene t -> m (Dynamic t Action)
actions scene = holdWorkflow base where

  base = commonTransition (base <$ cancel) $ Workflow $ do
    let beginPan = pan <$> (current mouse <@ mouseDown LeftButton)
        beginDraw = drawMode <$ keyDown Key.Space

    tellEvent zoomCmd
    return (def, leftmost [beginDraw, beginPan])

  drawMode = action $
    ("crosshair", base <$ keyUp Key.Space) <$ drawBoxes scene (scene ^. #input)

  pan localOrigin = action $ do
    tellEvent $ leftmost
      [ ViewCmd . PanCmd localOrigin <$> updated pageMouse
      , zoomCmd
      ]
    return ("move", base <$ mouseUp LeftButton)

  zoomCmd = ViewCmd <$> attachWith (flip ZoomCmd) (current mouse) wheel
  cancel = leftmost [void focus, void $ keyDown Key.Escape]

  SceneInputs{..} = scene ^. #input


sceneView :: AppBuilder t m => Scene t -> m (ElemType t m, Dynamic t Action)
sceneView scene = inViewport "expand enable-cursor" (scene ^. #viewport) $ do
    imageView [] (scene ^. #image)
    actions scene
