module Scene.View where

import Annotate.Common
import Client.Common

import Reflex.Classes
import Builder.Svg hiding (switch, cursor, view)

import qualified Builder.Svg as Svg

import Input.Events
import Scene.Events

import qualified Data.Set as S
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
  g [transform_ ~: (transforms <$> vp)] child

splitBox :: Functor f => f Box -> (f (V2 Float), f (V2 Float))
splitBox ab = (view #lower <$> ab, view #upper <$> ab)


xy_ :: Attribute (V2 Float)
xy_ =  contramap (view _x) x_ <> contramap (view _y) y_

cxcy_ :: Attribute (V2 Float)
cxcy_ =  contramap (view _x) x_ <> contramap (view _y) y_


wh_ :: Attribute (V2 Float)
wh_ =  contramap (view _x) width_ <> contramap (view _y) height_


boxView :: (Builder t m) => Active t [Text] -> Active t Box -> m (ElemType t m)
boxView classes b = rect_ [classes_ ~: classes, xy_ ~: lower, wh_ ~: (upper - lower)] where
  (lower, upper) = splitBox b

circleView :: (Builder t m) =>  Active t [Text] -> Active t Circle -> m (ElemType t m)
circleView classes c  = circle_ [classes_ ~: classes, cxcy_ ~: centre, r_ ~: view #radius <$> c] where
  centre = view #centre <$> c


shapeView :: forall t m. (Builder t m) => Active t [Text] -> Updated t Object -> m (ElemType t m)
shapeView classes  obj  = case view #shape <$> obj of
  Updated (BoxShape b) e    -> boxView classes . Dyn =<< holdDyn b (_BoxShape ?> e)
  Updated (CircleShape c) e -> circleView classes . Dyn =<< holdDyn c (_CircleShape ?> e)

outlineView :: forall t m. (Builder t m) => Updated t Object -> m ()
outlineView = void . shapeView (pure ["outline"])

objectView :: forall t m. (Builder t m) => Dynamic t Bool -> ObjId -> Updated t Object -> m (Event t Bool)
objectView selected k obj = do
  e <- shapeView classes obj
  return $ leftmost
    [ True <$ domEvent Mouseenter e
    , False <$ domEvent Mouseleave e
    ]
  where
    selectClass b = if b then "selected" else ""
    classes = activeList [pure "object", Dyn $ selectClass <$> selected]


imageView :: (AppBuilder t m) =>  Text -> Image -> m (ElemType t m)
imageView classes (file, dim) = do
    path <- localPath file
    Svg.image_ [class_ =: classes, wh_ =: fromDim dim, href_ =: path]


holdChanges :: (Reflex t, MonadHold t m) => a -> Event t a -> m (Event t (a, a))
holdChanges initial e = flip attach e <$> hold initial e


type SceneAction t m = Workflow t m (Dynamic t Action)
type Cursor = Text


action :: AppBuilder t m => m (Dynamic t Cursor, Event t (SceneAction t m)) -> SceneAction t m
action m = Workflow $ over _1 (fmap f) <$> m
   where f cursor = Action cursor True


addObject :: AppBuilder t m => Scene t -> Event t Object -> m ()
addObject Scene{nextId} add = editCommand (Add . pure <$> current nextId `attach` add)



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

      boxView  (pure []) (Dyn box)
      return (Nothing, idle . Just <$> done)

    makeBox p1 p2 = Box (liftI2 min p1 p2) (liftI2 max p1 p2)
    boxObject e = makeObject <$> current (scene ^. #currentClass) <@> e
    makeObject classId box = Object (BoxShape box) classId []

selectChange :: Set Key -> Set ObjId -> Set ObjId -> Set ObjId
selectChange keys existing target
  | S.member Shift keys = existing <> target
  | otherwise           = target


actions :: AppBuilder t m => Scene t -> m (Dynamic t Action)
actions scene@Scene{input, selection, document } = holdWorkflow $
  commonTransition (base <$ cancel) base where

  base = Workflow $ do
    let beginPan = pan <$> (current mouse <@ mouseDown LeftButton)
        beginDraw = drawMode <$ keyDown Key.Space
        beginDrag = filterMaybe $ drag <$> current mouse <*> current document <@> selection'
        selection' = (selectChange <$> current keyboard <*> current selection) <@> downOn LeftButton

    viewCommand zoomCmd
    command SelectCmd selection'
    return (def, leftmost [beginDrag, beginDraw, beginPan])

  drag origin doc target
        | S.null target = Nothing
        | otherwise     = Just $ action $ do
    let offset = mouse - pure origin
        objects = lookupObjects  (S.toList target) doc
        endDrag = mouseUp LeftButton

    for_ objects $ \obj -> outlineView $
      Updated obj (flip (transformObj 1.0) obj <$> updated offset)

    editCommand ((Transform (S.toList target) 1.0 <$> current offset) <@ endDrag)

    return ("pointer", base <$ endDrag)

  drawMode = action $
    ("crosshair", base <$ keyUp Key.Space) <$ drawBoxes scene input

  pan origin = action $ do
    viewCommand $ leftmost
      [ PanView origin <$> updated pageMouse
      , zoomCmd
      ]
    return ("move", base <$ mouseUp LeftButton)

  zoomCmd = attachWith (flip ZoomView) (current mouse) wheel
  cancel = leftmost [void focus, void $ keyDown Key.Escape]

  SceneInputs{..} = input
  downOn b =  current hover <@ mouseDown b



sceneView :: AppBuilder t m => Scene t -> m (ElemType t m, (Dynamic t Action, Event t (Map ObjId Bool)))
sceneView scene@Scene{image, viewport, objects, selection} = inViewport "expand enable-cursor" viewport $ do
    imageView "" image

    hovers <- holdMergePatched =<< patchMapWithUpdates objects objectView'
    action <- actions scene

    return (action, hovers)
      where
          isSelected    = fanDynSet selection
          objectView' k = objectView (isSelected k) k
