module Scene.Controller where

import Annotate.Prelude
import Client.Common

import Builder.Html hiding (select, for_)

import Input.Events
import Scene.Events

import qualified Data.Set as S
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as Monoidal
import qualified Data.List.NonEmpty as NE

import qualified Web.KeyCode as Key

import Scene.Types
import Scene.Viewport
import Scene.Canvas (loadImage)
import Scene.Drawing

import Annotate.Geometry
import Annotate.Common
import Annotate.Editor

import Data.Bool (bool)




splitBox :: Functor f => f Box -> (f (V2 Float), f (V2 Float))
splitBox ab = (view #lower <$> ab, view #upper <$> ab)


between :: Circle -> Circle -> Polygon
between (Circle p1 r1) (Circle p2 r2) = Polygon $ NE.fromList [p1 - v1 , p1 + v1, p2 + v2, p2 - v2]
  where
    v = normalize $ perp (p2 - p1)
    (v1, v2) = (v ^* r1, v ^* r2)



annConfidence :: Annotation -> Maybe Float
annConfidence Annotation{detection} = case detection of 
  Just (_, d)   -> Just $ d ^. #confidence
  _             -> Nothing

isShown :: Annotation -> Bool -> (Float, Float) -> (Bool, Bool)
isShown Annotation{detection} review (higher, lower) = fromMaybe (False, False) $ do 
  (tag, d) <- detection
  let confidence = d ^. #confidence
  return $ case tag of 
    Review      -> (False, False)
    Missed      -> (not (review && confidence >= lower), True)
    Detected    -> (not (review && confidence >= lower || confidence >= higher), confidence < higher)
    Confirmed _ -> (False, False)


holdChanges :: (Reflex t, MonadHold t m) => a -> Event t a -> m (Event t (a, a))
holdChanges initial e = flip attach e <$> hold initial e

type ControllerAction t m = Workflow t (ControllerT t m) ()

withAction :: (Reflex t, MonadHold t m, MonadFix m) => (Action -> Action) -> ControllerT t m a -> ControllerT t m a
withAction f (ControllerT m) = ControllerT $ withDynamicWriterT f m

action :: (Reflex t, MonadHold t m, MonadFix m) => ControllerT t m (Event t (ControllerAction t m)) -> ControllerAction t m
action = makeWorkflow'

withCursor :: AppBuilder t m => (Cursor, Bool) -> ControllerT t m a -> ControllerT t m a
withCursor cursor m =  withAction (#cursor .~ pure cursor)  m

captures :: AppBuilder t m => Cursor -> ControllerT t m a -> ControllerT t m a
captures cursor = withCursor (cursor, True)


displaying :: (Reflex t, MonadHold t m) =>  Dynamic t (Action -> Action) -> ControllerT t m ()
displaying = ControllerT . tellDyn . fmap ($ def)

setCursor :: (Reflex t, MonadHold t m) => Dynamic t (Cursor, Bool) -> ControllerT t m ()
setCursor = displaying . fmap (set #cursor . pure)

maybeEditing :: (Reflex t, MonadHold t m) => Dynamic t (Maybe Edit) -> ControllerT t m ()
maybeEditing = displaying . fmap (set #edit . maybeToList)

editing :: (Reflex t, MonadHold t m) => Dynamic t Edit -> ControllerT t m ()
editing = displaying . fmap (set #edit . pure)

overlaying :: (Reflex t, MonadHold t m) => Dynamic t (Render ()) -> ControllerT t m ()
overlaying = displaying . fmap (set #overlay)

highlighting :: (Reflex t, MonadHold t m) => Dynamic t DocParts -> ControllerT t m ()
highlighting = displaying . fmap (set #highlight)



  
makeBox :: Point -> Point -> Box
makeBox p1 p2 = Box (liftI2 min p1 p2) (liftI2 max p1 p2)

drawBoxes :: AppBuilder t m => Scene t -> SceneInputs t -> Event t () -> ControllerT t m ()
drawBoxes scene SceneInputs{..} _ =
  addShapes scene . filterMaybe . updated =<< workflow (idle Nothing)

  where
    idle r = Workflow $ do
      return (r, drawing <$> (current mouse <@ click LeftButton))

    drawing p1 = Workflow $ do
      let box = makeBox p1 <$> mouse
          done = current box <@ click LeftButton

      -- boxElem  [class_ =: "outline"] box
      return (Nothing, idle . Just . ShapeBox <$> done)


drawPolygons :: AppBuilder t m => Scene t -> SceneInputs t -> Event t () -> ControllerT t m ()
drawPolygons scene SceneInputs{..} finish = void $ workflow idle

  where
    idle = action $
      return (drawing . pure <$> (current mouse `tag` click LeftButton))

    drawing points = action $ do
      let points'  = (`NE.cons` points) <$> mouse
          shape     = ShapePolygon (Polygon points)
          next     = current points' `tag` click LeftButton

      -- polygonElem  [class_ =: "outline"] (Polygon <$> points')
      addShapes scene (shape <$ finish)

      return (drawing <$> next)


drawCircles :: AppBuilder t m => Scene t -> SceneInputs t -> Event t () -> ControllerT t m ()
drawCircles scene SceneInputs{..} finish = do

  brushSize <- holdUniqDyn $ view (#display . #brushSize) <$> (scene ^. #preferences)
  let 
    cursor = Circle <$> mouse <*> brushSize
  
  prefCommand (ZoomBrush <$> wheel)
  -- circleElem [class_ =: "outline"] cursor

  addShapes scene (ShapeCircle <$> current cursor `tag` click LeftButton)


drawLines :: forall t m. AppBuilder t m => Scene t -> SceneInputs t -> Event t () -> ControllerT t m ()
drawLines scene SceneInputs{..} finish = void $ do
  prefCommand (ZoomBrush <$> wheel)
  workflow idle

  where
    brushSize = view (#display . #brushSize) <$> (scene ^. #preferences)
    cursor = Circle <$> mouse <*> brushSize

    idle  = action $ do
      -- circleElem [class_ =: "outline"] cursor
      return (drawing . pure <$> (current cursor `tag` click LeftButton))

    drawing points = action $ do
      let points'  = (`NE.cons` points) <$> cursor
          shape    = ShapeLine (WideLine points)
          next     = current points' `tag` click LeftButton

      -- lineElem "draw" [class_ =: "outline"] (WideLine <$> points')
      addShapes scene (shape <$ finish)

      return (drawing <$> next)


addAnnotation :: AppBuilder t m => Event t BasicAnnotation -> ControllerT t m ()
addAnnotation add = editCommand (EditAdd . pure <$> add)

addShapes :: AppBuilder t m => Scene t -> Event t Shape -> ControllerT t m ()
addShapes scene e = addAnnotation (makeAnnotation e)
  where  makeAnnotation e = BasicAnnotation <$> e <#> current (scene ^. #currentClass)




selectChange :: Set Key -> Editor -> DocParts -> Maybe DocPart -> DocParts
selectChange keys doc existing target
  | S.member Key.Shift keys = fromMaybe existing (flip (addPart doc) existing <$> target)
  | otherwise               = fromMaybe mempty (toParts doc <$> target)

selectParts :: Reflex t => Scene t -> Event t DocParts
selectParts Scene{editor, selection, input, shortcut} =
  selectChange <$> current keyboard <*> current editor <*> current selection <@> partsClicked where

    partsClicked = leftmost
      [ Just    <$>  mouseDownOn LeftButton
      , Nothing <$ mouseDown LeftButton
      ]
    SceneInputs{keyboard, mouseDown, mouseDownOn} = input


confirmAnnotation :: Reflex t => Scene t -> Event t (Maybe (AnnotationId, Bool))
confirmAnnotation Scene{editor, input, preferences} 
    = confirm <$> current editor <*> current preferences <@> down where

  confirm Editor{annotations} prefs (i, _) = do 
    ann <- Map.lookup i annotations
    (t, d) <- ann ^. #detection
    guard (canConfirm t)
    return (i, belowThreshold t (d ^. #confidence) (prefs ^. #thresholds))
    
  down = mouseDownOn input LeftButton
  canConfirm t = t == Detected || t == Missed

  
belowThreshold :: DetectionTag -> Float -> (Float, Float) -> Bool
belowThreshold Detected conf (t, _) = conf < t
belowThreshold Missed   _    (t, _) = True

highlightParts :: Editor -> [DocPart] -> DocParts
highlightParts editor parts = fromMaybe mempty (toParts editor <$> preview _head parts)


actions :: forall t m. AppBuilder t m => Scene t -> ControllerT t m ()
actions scene@Scene{..} = holdWorkflow $
  commonTransition (base <$ cancel) base where

  base :: ControllerAction t m
  base = action $ do
    let beginPan    = pan <$> mouseDownAt
        beginSelectRect = rectSelect <$> gate (current holdingShift) mouseDownAt

        beginDraw   = (drawMode <$> current currentClass <*> current config) `tag` localKeyDown drawKey
        beginDragSelection   = filterMaybe $ drag <$> current mouse <*> current editor <@> selectionClick

    viewCommand zoomCmd
    command SelectCmd $ leftmost [selectAll, selectionClick]

    let deleteSelection = ffilter (not . null) (current selection <@ select shortcut ShortDelete)
    editCommand $ EditDeleteParts <$> deleteSelection 
    editCommand $ fmap (EditConfirmDetection . uncurry Map.singleton) <?> confirmAnnotation scene

    docCommand (const DocUndo) (select shortcut ShortUndo)
    docCommand (const DocRedo) (select shortcut ShortRedo)

    setCursor (cursor <$> holdingShift <*> hover)
    highlighting (liftA2 highlightParts editor hover)

    return (leftmost [beginSelectRect, beginDragSelection, beginDraw, beginPan])
      where 
        holdingShift   = S.member Key.Shift <$> keyboard
        selectAll        = documentParts <$> current editor `tag` select shortcut ShortSelectAll
        selectionClick   = selectParts scene
        mouseDownAt = current mouse <@ mouseDown LeftButton      

        cursor additive hovering = (name, False)
          where name = if not (null hovering) then "grab"
                  else (if additive then "crosshair" else "default")

  -- Translate dragged annotations
  drag :: Vec -> Editor -> DocParts -> Maybe (ControllerAction t m)
  drag origin doc target
        | null   target = Nothing
        | otherwise     = Just $ action $ do
    let offset  = mouse - pure origin
        endDrag = mouseUp LeftButton

    scale <- foldDyn (\z -> max 0.1 . (z *)) 1.0 zoom

    let maybeEdit s t = do
          guard $ abs (s - 1.0) > 0.01 || norm t > 0.01
          return $ EditTransformParts (s, t) target 
        edit = maybeEdit <$> scale <*> offset

        pointer e = if isJust e then ("pointer", True) else ("default", False)

    maybeEditing edit 

    editCommand $ filterMaybe (current edit <@ endDrag)
    setCursor (pointer <$> edit)
    return (base <$ endDrag)

  -- Draw boxes
  drawMode :: ClassId -> Config -> ControllerAction t m
  drawMode k config = action $ do
    for_ (Map.lookup k (config ^. #classes)) $ \classConfig ->
      case classConfig ^. #shape of
        ConfigBox     -> drawBoxes scene input finish
        ConfigCircle  -> drawCircles scene input finish
        ConfigPolygon -> drawPolygons scene input finish
        ConfigLine    -> drawLines scene input finish

    return (base <$ finish)
        where finish = keyUp drawKey

  rectSelect  :: Vec -> ControllerAction t m
  rectSelect p1 = action $ withCursor ("crosshair", True) $ do
    let box = makeBox p1 <$> mouse
        done = current box <@ leftmost [click LeftButton, mouseUp LeftButton]

        parts  = queryBox <$> query <*> current box
        parts' = mappend <$> current selection <*> parts

    command SelectCmd (parts' `tag` done)

    overlaying (selectRect <$> box)
    return (base <$ done)


  -- Pan the view when a blank part of the scene is dragged
  pan  :: Vec -> ControllerAction t m
  pan origin = action $ captures "move" $ do
    viewCommand $ leftmost
      [ PanView origin <$> updated pageMouse
      , zoomCmd
      ]
    return (base <$ mouseUp LeftButton)


  zoom = wheelZoom <$> (S.member Key.Control <$> current keyboard) <@> wheel
  zoomCmd = attachWith (flip ZoomView) (current mouse) zoom
  cancel = leftmost [void focus, select shortcut ShortCancel]

  SceneInputs{..} = input
  drawKey = Key.Space


  

scene :: (MonadReader (Scene t) m, Reflex t) => (Scene t -> m a) -> m a
scene f = ask >>= f




    
controller :: AppBuilder t m => Scene t -> m (Dynamic t Action)
controller scene = runController $ actions scene


      


