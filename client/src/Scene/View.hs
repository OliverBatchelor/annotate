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

inViewport :: (Builder t m) => Dynamic t Viewport -> m a -> m a
inViewport vp child = g [transform_ ~: (transforms <$> vp)] child

splitBox :: Functor f => f Box -> (f (V2 Float), f (V2 Float))
splitBox ab = (view #lower <$> ab, view #upper <$> ab)


boxElem :: (Builder t m) => [Property t] -> Active t Box -> m (ElemType t m)
boxElem props b = rect_ $ props <> [xy_ ~: lower, wh_ ~: (upper - lower)] where
  (lower, upper) = splitBox b


sceneDefines :: Builder t m => Dynamic t Viewport -> Dynamic t Preferences -> m ()
sceneDefines vp prefs = void $ defs [] $ do
    boxElem [id_ =: controlId, class_ =: "control"] (Dyn $ makeBox <$> prefs <*> vp)

  where

    makeBox prefs vp = getBounds $ Extents (V2 0 0) (V2 s s)
      where s = (prefs ^. #controlSize) / (vp ^. #zoom)

boxVertices :: Box -> (Position, Position, Position, Position)
boxVertices (Box (V2 lx ly) (V2 ux uy)) =
  ( V2 lx ly
  , V2 ux ly
  , V2 ux uy
  , V2 lx uy
  )


controlId :: Text
controlId = "control"

control :: Builder t m => Dynamic t Position -> m ()
control p = void $ use_ [href_ =: "#" <> controlId, transform_ ~: toCentre <$> p ]
  where toCentre (V2 x y) = [Translate x y]

boxView :: Builder t m => ShapeProperties t  -> Dynamic t Box -> m (Event t SceneEvent)
boxView props box = do
  e <- boxElem (shapeProperties props) (Dyn box)

  g_ [shown_ ~: props ^. #selected] $ sequence_
    [control v1, control v2, control v3, control v4]

  return $ leftmost
    [ SceneEnter <$ domEvent Mouseenter e
    , SceneLeave <$ domEvent Mouseleave e
    , SceneDown <$ domEvent Mousedown e
    ]

  where
    (v1, v2, v3, v4) = split4 (boxVertices <$> box)


polygonView :: Builder t m => ShapeProperties t  -> Dynamic t Polygon -> m (Event t SceneEvent)
polygonView props poly = do
  e <- polygonElem (shapeProperties props) (Dyn poly)
  return never

lineView :: Builder t m => ShapeProperties t  -> Dynamic t WideLine -> m (Event t SceneEvent)
lineView props line = do
  e <- lineElem (shapeProperties props) (Dyn line)

  return $ leftmost
    [ SceneEnter <$ domEvent Mouseenter e
    , SceneLeave <$ domEvent Mouseleave e
    ]


circleElem :: (Builder t m) =>  [Property t] -> Active t Circle -> m (ElemType t m)
circleElem props c  = circle_ $ props <> [cxcy_ ~: centre, r_ ~: view #radius <$> c] where
  centre = view #centre <$> c

polygonElem :: (Builder t m) =>  [Property t] -> Active t Polygon -> m (ElemType t m)
polygonElem props poly  = polygon_ $ props <> [points_ ~: toList . view #points <$> poly]

lineElem  :: (Builder t m) =>  [Property t] -> Active t WideLine -> m (ElemType t m)
lineElem props line = g_ [] blank

data ShapeProperties t = ShapeProperties
  { selected :: !(Dynamic t Bool)
  , fill     :: !(Dynamic t HexColour)
  } deriving Generic

shapeView :: forall t m. (Builder t m) => ShapeProperties t -> Updated t Annotation -> m (Event t SceneEvent)
shapeView props obj  = case view #shape <$> obj of
  Updated (BoxShape s)     e  -> boxView props      =<< holdDyn s (_BoxShape ?> e)
  Updated (PolygonShape s) e  -> polygonView props  =<< holdDyn s (_PolygonShape ?> e)
  Updated (LineShape s)    e  -> lineView props     =<< holdDyn s (_LineShape ?> e)


shapeElem :: forall t m. (Builder t m) => [Property t] -> Updated t Annotation -> m (ElemType t m)
shapeElem props obj  = case view #shape <$> obj of
  Updated (BoxShape s)     e  -> boxElem props     . Dyn =<< holdDyn s (_BoxShape ?> e)
  Updated (PolygonShape s) e  -> polygonElem props . Dyn =<< holdDyn s (_PolygonShape ?> e)
  Updated (LineShape s)    e  -> lineElem props    . Dyn =<< holdDyn s (_LineShape ?> e)



shapeProperties :: Reflex t => ShapeProperties t -> [Property t]
shapeProperties ShapeProperties{selected, fill} =
    [ classes_ ~: activeList [pure "annotation", Dyn $ selectClass <$> selected]
    , style_ ~: style <$> fill ]
  where

    selectClass b = if b then "selected" else ""
    style colour = [("fill", showColour colour)]

  -- Updated (CircleShape c) e -> circleView classes . Dyn =<< holdDyn c (_CircleShape ?> e)

outlineView :: forall t m. (Builder t m) => Updated t Annotation -> m ()
outlineView = void . shapeElem [class_ =: "outline"]


annotationView :: forall t m. (Builder t m) => Dynamic t Config -> Dynamic t Bool -> AnnotationId -> Updated t Annotation -> m (Event t SceneEvent)
annotationView config selected k obj = do
  classId <- holdUpdated (view #label <$> obj)
  let colour = classColour <$> config <*> classId

  shapeView (ShapeProperties selected colour) obj
    where
      classColour Config{classes} k = case M.lookup k classes of
        Nothing   -> 0x000000
        Just ClassConfig{colour} -> colour


imageView :: (AppBuilder t m) =>  Image -> m (ElemType t m)
imageView  (file, dim) = do
    path <- localPath file
    Svg.image_ [draggable_ =: False, class_ =: "disable-cursor", wh_ =: fromDim dim, href_ =: path]


holdChanges :: (Reflex t, MonadHold t m) => a -> Event t a -> m (Event t (a, a))
holdChanges initial e = flip attach e <$> hold initial e


type SceneAction t m = Workflow t m (Dynamic t Action)
type Cursor = Text


action :: AppBuilder t m => m (Dynamic t Cursor, Event t (SceneAction t m)) -> SceneAction t m
action m = Workflow $ over _1 (fmap f) <$> m
   where f cursor = Action cursor True Nothing

editAction :: AppBuilder t m => m (Dynamic t Cursor, Dynamic t Edit, Event t (SceneAction t m)) -> SceneAction t m
editAction m = Workflow $ do
    (cursor, edit, transitions) <- m
    return (f <$> cursor <*> edit, transitions)
      where f cursor edit = Action cursor True (Just edit)


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

      boxElem  [class_ =: "outline"] (Dyn box)
      return (Nothing, idle . Just <$> done)

    makeBox p1 p2 = Box (liftI2 min p1 p2) (liftI2 max p1 p2)
    boxAnnotation e = makeAnnotation <$> current (scene ^. #currentClass) <@> e
    makeAnnotation classId box = Annotation (BoxShape box) classId []

selectChange :: Set Key -> Set AnnotationId -> Set AnnotationId -> Set AnnotationId
selectChange keys existing target
  | S.member Key.Shift keys = existing <> target
  | otherwise           = target


actions :: AppBuilder t m => Scene t -> m (Dynamic t Action)
actions scene@Scene{..} = holdWorkflow $
  commonTransition (base <$ cancel) base where

  base = Workflow $ do
    let beginPan    = pan <$> (current mouse <@ mouseDown LeftButton)
        beginDraw   = (drawMode <$> current currentClass <*> current config) `tag` keyDown Key.Space
        beginDrag   = filterMaybe $ drag <$> current mouse <*> current document <@> selection'
        selection'  = (selectChange <$> current keyboard <*> current selection) <@> downOn LeftButton

    viewCommand zoomCmd
    command SelectCmd selection'

    editCommand $ Delete . S.toList <$> ffilter (not . null)
      (current selection <@ select shortcut ShortDelete)

    docCommand (const DocUndo) (select shortcut ShortUndo)
    docCommand (const DocRedo) (select shortcut ShortRedo)

    return (def, leftmost [beginDrag, beginDraw, beginPan])

  -- Translate dragged annotations
  drag origin doc target
        | S.null target = Nothing
        | otherwise     = Just $ editAction $ do
    let offset  = mouse - pure origin
        annotations = lookupAnnotations  (S.toList target) doc
        endDrag = mouseUp LeftButton

    scale <- foldDyn (\z -> max 0.1 . (z *)) 1.0 (wheelZoom <$> wheel)
    let edit = Transform (S.toList target) <$> scale <*> offset

    -- for_ annotations $ \obj -> outlineView $
    --   Updated obj (updated $ transformObj <$> scale <*> offset <*> pure obj)
    editCommand (current edit <@ endDrag)
    return ("pointer", edit, base <$ endDrag)

  -- Draw boxes
  drawMode k config = action $ do
    drawBoxes scene input
    return ("crosshair", base <$ keyUp Key.Space)

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



sceneView :: AppBuilder t m => Scene t -> m (Dynamic t Action, Event t (Map AnnotationId SceneEvent))
sceneView scene@Scene{..} = do
    imageView image

    sceneEvents <- holdMergePatched =<< incrementalMapWithUpdates annotations $ \k ->
       annotationView config (isSelected k) k

    action <- actions scene

    return (action, sceneEvents)
      where
          isSelected = fanDynSet selection
