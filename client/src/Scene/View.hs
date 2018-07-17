module Scene.View where

import Annotate.Prelude
import Client.Common

import Builder.Svg hiding (switch, cursor, view)

import qualified Builder.Svg as Svg

import Input.Events
import Scene.Events

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Web.KeyCode as Key

import Scene.Types
import Scene.Viewport

import Annotate.Geometry
import Annotate.Common
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


boxView :: (Builder t m) => Active t [Text] -> Active t Box -> m (ElemType t m)
boxView classes b = rect_ [classes_ ~: classes, xy_ ~: lower, wh_ ~: (upper - lower)] where
  (lower, upper) = splitBox b

circleView :: (Builder t m) =>  Active t [Text] -> Active t Circle -> m (ElemType t m)
circleView classes c  = circle_ [classes_ ~: classes, cxcy_ ~: centre, r_ ~: view #radius <$> c] where
  centre = view #centre <$> c


polygonView :: (Builder t m) =>  Active t [Text] -> Active t Polygon -> m (ElemType t m)
polygonView classes poly  = polygon_ [classes_ ~: classes, points_ ~: toList . view #points <$> poly] 
  
lineView :: (Builder t m) =>  Active t [Text] -> Active t WideLine -> m (ElemType t m)
lineView classes line = g_ [classes_ ~: classes] blank

shapeView :: forall t m. (Builder t m) => Active t [Text] -> Updated t Annotation -> m (ElemType t m)
shapeView classes  obj  = case view #shape <$> obj of
  Updated (BoxShape s)     e  -> boxView classes     . Dyn =<< holdDyn s (_BoxShape ?> e)
  Updated (PolygonShape s) e  -> polygonView classes . Dyn =<< holdDyn s (_PolygonShape ?> e)
  Updated (LineShape s)    e  -> lineView classes    . Dyn =<< holdDyn s (_LineShape ?> e)
  
  -- Updated (CircleShape c) e -> circleView classes . Dyn =<< holdDyn c (_CircleShape ?> e)

outlineView :: forall t m. (Builder t m) => Updated t Annotation -> m ()
outlineView = void . shapeView (pure ["outline"])

annotationView :: forall t m. (Builder t m) => Dynamic t Bool -> AnnotationId -> Updated t Annotation -> m (Event t Bool)
annotationView selected k obj = do
  e <- shapeView classes obj
  return $ leftmost
    [ True <$ domEvent Mouseenter e
    , False <$ domEvent Mouseleave e
    ]
  where
    selectClass b = if b then "selected" else ""
    classes = activeList [pure "annotation", Dyn $ selectClass <$> selected]


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


addAnnotation :: AppBuilder t m => Scene t -> Event t Annotation -> m ()
addAnnotation Scene{nextId} add = editCommand (Add . pure <$> current nextId `attach` add)



drawBoxes :: AppBuilder t m => Scene t -> SceneInputs t -> m ()
drawBoxes scene SceneInputs{..} = do
  e <- filterMaybe <$> workflowView (idle Nothing)
  addAnnotation scene (boxAnnotation e)
  where
    idle r = Workflow $ do
      return (r, drawing <$> (current mouse <@ mouseDown LeftButton))

    drawing p1 = Workflow $ do
      let box = makeBox p1 <$> mouse
          done = current box <@ mouseUp LeftButton

      boxView  (pure ["outline"]) (Dyn box)
      return (Nothing, idle . Just <$> done)

    makeBox p1 p2 = Box (liftI2 min p1 p2) (liftI2 max p1 p2)
    boxAnnotation e = makeAnnotation <$> current (scene ^. #currentClass) <@> e
    makeAnnotation classId box = Annotation (BoxShape box) classId []

selectChange :: Set Key -> Set AnnotationId -> Set AnnotationId -> Set AnnotationId
selectChange keys existing target
  | S.member Key.Shift keys = existing <> target
  | otherwise           = target


matchShortcuts :: Reflex t => SceneInputs t -> Event t (Map Shortcut ())
matchShortcuts SceneInputs{..} = mergeMap $ M.fromList
  [ (ShortUndo, keyCombo Key.KeyZ [Key.Control])
  , (ShortRedo, keyCombo Key.KeyZ [Key.Control, Key.Shift])
  , (ShortDelete, keyDown Key.Delete)
  , (ShortCancel, keyDown Key.Escape)
  ]

actions :: AppBuilder t m => Scene t -> m (Dynamic t Action)
actions scene@Scene{input, selection, document, shortcut} = holdWorkflow $
  commonTransition (base <$ cancel) base where

  base = Workflow $ do
    let beginPan    = pan <$> (current mouse <@ mouseDown LeftButton)
        beginDraw   = drawMode <$ keyDown Key.Space
        beginDrag   = filterMaybe $ drag <$> current mouse <*> current document <@> selection'
        selection'  = (selectChange <$> current keyboard <*> current selection) <@> downOn LeftButton

    viewCommand zoomCmd
    command SelectCmd selection'

    editCommand $ Delete . S.toList <$> ffilter (not . null)
      (current selection <@ shortcut ShortDelete)

    docCommand (const DocUndo) (shortcut ShortUndo)
    docCommand (const DocRedo) (shortcut ShortRedo)

    return (def, leftmost [beginDrag, beginDraw, beginPan])

  -- Translate dragged annotations
  drag origin doc target
        | S.null target = Nothing
        | otherwise     = Just $ action $ do
    let offset  = mouse - pure origin
        annotations = lookupAnnotations  (S.toList target) doc
        endDrag = mouseUp LeftButton

    scale <- foldDyn (\z -> max 0.1 . (z *)) 1.0 (wheelZoom <$> wheel)

    for_ annotations $ \obj -> outlineView $
      Updated obj (updated $ transformObj <$> scale <*> offset <*> pure obj)

    editCommand ((Transform (S.toList target) <$> current scale <*> current offset) <@ endDrag)
    return ("pointer", base <$ endDrag)

  -- Draw boxes
  drawMode = action $
    ("crosshair", base <$ keyUp Key.Space) <$ drawBoxes scene input

  -- Pan the view when a blank part of the scene is dragged
  pan origin = action $ do
    viewCommand $ leftmost
      [ PanView origin <$> updated pageMouse
      , zoomCmd
      ]
    return ("move", base <$ mouseUp LeftButton)

  zoomCmd = attachWith (flip ZoomView) (current mouse) wheel
  cancel = leftmost [void focus, void $ keyDown Key.Escape]

  SceneInputs{..} = input



sceneView :: AppBuilder t m => Scene t -> m (ElemType t m, (Dynamic t Action, Event t (Map AnnotationId Bool)))
sceneView scene@Scene{image, viewport, annotations, selection} = inViewport "expand enable-cursor" viewport $ do
    imageView "" image

    hovers <- holdMergePatched =<< patchMapWithUpdates annotations annotationView'
    action <- actions scene

    return (action, hovers)
      where
          isSelected    = fanDynSet selection
          annotationView' k = annotationView (isSelected k) k
