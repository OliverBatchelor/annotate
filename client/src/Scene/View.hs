module Scene.View where

import Annotate.Prelude
import Client.Common

import Builder.Svg hiding (switch, cursor, view)
import Builder.Html (classList)


import qualified Builder.Svg as Svg

import Input.Events
import Scene.Events

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE

import qualified Web.KeyCode as Key

import Scene.Types
import Scene.Viewport

import Annotate.Geometry
import Annotate.Common
import Annotate.Document

import Debug.Trace

transforms :: Viewport -> [Svg.Transform]
transforms vp = [Translate tx ty, Scale zoom zoom] where
    (V2 tx ty) = localOffset vp
    zoom = vp ^. #zoom

inViewport :: (Builder t m) => Dynamic t Viewport -> m a -> m a
inViewport vp child = g [transform_ ~: (transforms <$> vp)] child




sceneDefines :: Builder t m => Dynamic t Viewport -> Dynamic t Preferences -> m ()
sceneDefines vp prefs = void $ defs [] $ do
    boxElem [ id_ =: controlId ] (makeBox <$> prefs <*> vp)

    brightness <- holdUniqDyn (view #brightness <$> prefs)
    contrast   <- holdUniqDyn (view #contrast <$> prefs)
    gamma      <- holdUniqDyn ((1.0 /) . view #gamma <$> prefs)

    Svg.filter [id_ =: adjustmentId ] $
      feComponentTransfer [] $ void $ do
        feFuncR_ [type_ =: "gamma", amplitude_ ~: contrast, exponent_ ~: gamma, offset_ ~: brightness]
        feFuncG_ [type_ =: "gamma", amplitude_ ~: contrast, exponent_ ~: gamma, offset_ ~: brightness]
        feFuncB_ [type_ =: "gamma", amplitude_ ~: contrast, exponent_ ~: gamma, offset_ ~: brightness]

    -- Svg.filter [id_ =: "select-filter"] $
    --   feMorphology [operator_ =: "dilate" radius_ =: 2]

  where

    makeBox prefs vp = getBounds $ Extents (V2 0 0) (V2 s s)
      where s = (prefs ^. #controlSize) / (vp ^. #zoom)


controlId, adjustmentId :: Text
controlId = "control"
adjustmentId = "adjustment"


control :: Builder t m => Dynamic t Bool -> Dynamic t Position -> m (Event t SceneEvent)
control selected point = do
  e <- use_ [ href_ =: "#" <> controlId
            , classList ["control", "selected" `gated` selected]
            , transform_ ~: toCentre <$> point ]

  return $ (SceneDown  <$ domEvent Mousedown e)
    where toCentre (V2 x y) = [Translate x y]

controlCircle :: Builder t m => Dynamic t Bool -> Dynamic t Circle -> m (Event t SceneEvent)
controlCircle selected point = do
  e <- circleElem [classList ["control", "selected" `gated` selected]] point

  return (SceneDown  <$ domEvent Mousedown e)
    where toCentre (V2 x y) = [Translate x y]




controls :: Builder t m => Dynamic t (Maybe (Set Int)) -> [Dynamic t Position] -> m [Event t (Maybe Int, SceneEvent)]
controls selection = itraverse control' where
  control' k point = fmap (Just k,) <$>
    control (isSelected k <$> selection) point

  isSelected k = maybe False (S.member k)

dynControls :: (Foldable f, Builder t m)
            => (Dynamic t Bool -> Dynamic t a -> m (Event t SceneEvent))
            -> Dynamic t (Maybe (Set Int)) -> Dynamic t (f a) -> m (Event t (Maybe Int, SceneEvent))
dynControls makeControl selection ps = do
  update <- dynList control' ps
  e <- switchHold never (mergeMap <$> update)

  return (minElem <?> e)

  where
    control' k point = fmap (Just k,) <$>
      makeControl (isSelected k <$> selection) point

    isSelected k = maybe False (S.member k)


sceneEvents k e = (k,) <$> leftmost
    [ SceneEnter <$ domEvent Mouseenter e
    , SceneLeave <$ domEvent Mouseleave e
    , SceneDown <$ domEvent Mousedown e
    -- , SceneClick <$ domEvent Click e
    -- , SceneDoubleClick <$ domEvent Dblclick e
    ]


annotationProperties :: Reflex t => ShapeProperties t -> [Property t]
annotationProperties ShapeProperties{selected, hidden} =
    [ classList ["annotation", "selected" `gated` (isJust <$> selected)]
    , hidden_ ~: hidden
    , pointer_events_ =: "visiblePainted"
    ]


circleView :: Builder t m => ShapeProperties t  -> Dynamic t Circle -> m (Event t (Maybe Int, SceneEvent))
circleView props circle = fmap (sceneEvents Nothing) $
  g_ (annotationProperties props) $ void $
    circleElem (shapeAttributes "circle" props) circle




boxView :: Builder t m => ShapeProperties t  -> Dynamic t Box -> m (Event t (Maybe Int, SceneEvent))
boxView props box = do
  (e, events) <- g' (annotationProperties props) $ do
    boxElem (shapeAttributes "box" props) box
    controls (props ^. #selected) [v1, v2, v3, v4]

  return $ leftmost (events <> [sceneEvents Nothing e])

  where
    (v1, v2, v3, v4) = split4 (boxVertices <$> box)


polygonView :: Builder t m => ShapeProperties t  -> Dynamic t Polygon -> m (Event t (Maybe Int, SceneEvent))
polygonView props poly = do
  (e, events) <- g' (annotationProperties props) $ do
    polygonElem (shapeAttributes "polygon" props) poly
    dynControls control (props ^. #selected) (view #points <$> poly)

  return $ leftmost [events, sceneEvents Nothing e]



lineView :: Builder t m => ShapeProperties t  -> Dynamic t WideLine -> m (Event t (Maybe Int, SceneEvent))
lineView props line = do
  (e, events) <- g' (annotationProperties props) $ do

    lineElem (props ^. #elemId) (shapeAttributes "line" props) line
    dynControls controlCircle (props ^. #selected) (view #points <$> line)

  return $ leftmost [events, sceneEvents Nothing e]



splitBox :: Functor f => f Box -> (f (V2 Float), f (V2 Float))
splitBox ab = (view #lower <$> ab, view #upper <$> ab)


bounds_ :: Attribute Box
bounds_ =  contramap (view #lower) xy_ <> contramap boxSize wh_


boxElem :: (Builder t m) =>  [Property t] -> Dynamic t Box -> m (ElemType t m)
boxElem props b = rect_ $ props <> [xy_ ~: lower, wh_ ~: (upper - lower)] where
  (lower, upper) = splitBox b


circleElem :: (Builder t m) =>  [Property t] -> Dynamic t Circle -> m (ElemType t m)
circleElem props c  = circle_ $ props <> [ cxcy_ ~: centre, r_ ~: view #radius <$> c] where
  centre = view #centre <$> c

polygonElem :: (Builder t m) =>  [Property t] -> Dynamic t Polygon -> m (ElemType t m)
polygonElem props poly  = polygon_ $ props <> [points_ ~: toList . view #points <$> poly]

urlId :: Text -> Text
urlId i = "url(#" <> i <> ")"

url :: Text -> Text
url t = "url(" <> t <> ")"

cursorImage :: Text -> Text
cursorImage t = "url(" <> t <> ") 12 12, auto"



href_id_ :: Attribute Text
href_id_ = contramap urlId href_

mask_id_ :: Attribute Text
mask_id_ = contramap urlId mask_

clip_id_ :: Attribute Text
clip_id_ = contramap urlId clip_path_

lineElem  :: (Builder t m) => Text -> [Property t] -> Dynamic t WideLine -> m (ElemType t m)
lineElem elemId props line = do
    dynList' (lineShape maskId . toList) linePoints
    boxElem (props <> [clip_id_ =: maskId]) bounds

    where
      linePoints = view #points <$> line
      bounds = getBounds <$> linePoints

      maskId = "mask_" <> elemId

lineShape :: Builder t m => Text -> [Dynamic t Circle] -> m ()
lineShape maskId points = do
  defs [] $ void $ do
    clipPath [id_ =: maskId] $ shapes []

  where
    shapes properties = do
      traverse_ (circleElem properties) points
      traverse_ (polygonElem properties)
        (zipWith (liftA2 between) points (drop 1 points))


between :: Circle -> Circle -> Polygon
between (Circle p1 r1) (Circle p2 r2) = Polygon $ NE.fromList [p1 - v1 , p1 + v1, p2 + v2, p2 - v2]
  where
    v = normalize $ perp (p2 - p1)
    (v1, v2) = (v ^* r1, v ^* r2)

type ClassMap = Map ClassId ClassAttrs

data ClassAttrs = ClassAttrs
  { hidden :: Bool
  , colour   :: HexColour
  , name   :: Text
  } deriving (Generic, Eq)

classProperties :: Config -> Preferences -> ClassMap
classProperties Config{classes} Preferences{hiddenClasses} =
    flip M.mapWithKey classes $ \k conf ->
      ClassAttrs
        { hidden   = S.member k hiddenClasses
          , colour = conf ^. #colour
          , name   = conf ^. #name
        }


data ShapeProperties t = ShapeProperties
  { selected :: !(Dynamic t (Maybe (Set Int)))
  , colour   :: !(Dynamic t HexColour)
  , hidden   :: !(Dynamic t Bool)
  , elemId   :: !Text
  } deriving Generic


shapeAttributes :: Reflex t => Text -> ShapeProperties t -> [Property t]
shapeAttributes shapeType ShapeProperties{selected, hidden, colour} =
    [ classList [pure shapeType, "shape", defaults "selected" . isJust <$> selected]
    , style_ ~: style <$> colour
    , pointer_events_ =: "visiblePainted"]
  where

    style colour = [("fill", showColour colour)]



shapeView :: forall t m. (Builder t m) => ShapeProperties t -> Updated t Annotation -> m (Event t (Maybe Int, SceneEvent))
shapeView props obj  = case view #shape <$> obj of
  Updated (CircleShape s)  e  -> circleView props   =<< holdDyn s (_CircleShape ?> e)
  Updated (BoxShape s)     e  -> boxView props      =<< holdDyn s (_BoxShape ?> e)
  Updated (PolygonShape s) e  -> polygonView props  =<< holdDyn s (_PolygonShape ?> e)
  Updated (LineShape s)    e  -> lineView props     =<< holdDyn s (_LineShape ?> e)


holdHover :: (MonadHold t m, Reflex t) => Event t SceneEvent -> m (Dynamic t Bool)
holdHover e = holdDyn False $ fmapMaybe f e where
  f SceneEnter = Just True
  f SceneLeave = Just False
  f _          = Nothing


annotationView :: forall t m. (Builder t m)
               => Dynamic t ClassMap
               -> Dynamic t Float
               -> Dynamic t Bool
               -> Dynamic t (Maybe (Set Int))
               -> AnnotationId -> Updated t Annotation -> m (Event t (DocPart, SceneEvent))
annotationView classMap threshold instanceCols selected k obj = do
  classId <- holdUpdated (view #label <$> obj)

  let (colour :: Dynamic t HexColour)    = lookupCol <$> instanceCols <*> classInfo
      (classInfo :: Dynamic t (Maybe ClassAttrs)) = M.lookup <$> classId <*> classMap
      hidden    = liftA2 (||)  (hiddenClass <$> classInfo) ((> confidence) <$> threshold)

  rec
    e     <- shapeView (ShapeProperties selected colour hidden shapeId) obj
    -- hover <- holdHover (snd <$> e)

  return (arrange k <$> e)
    where
      shapeId = "ann" <> fromString (show k)

      hiddenClass = fromMaybe False . fmap (view #hidden)
      confidence  = getConfidence (obj ^. #initial)

      lookupCol :: Bool -> Maybe ClassAttrs -> HexColour
      lookupCol instanceCols classInfo = fromMaybe 0x000000 $ if instanceCols
        then M.lookup (k `mod` length defaultColours) defaultColourMap
        else (view #colour <$> classInfo)

      arrange k (part, e) = ((k, part), e)

imageView :: (AppBuilder t m) => Image -> m (ElemType t m)
imageView (file, dim) = do
    path <- localPath file

    Svg.image_
      [draggable_ =: False, class_ =: "disable-cursor"
      , wh_ =: fromDim dim, href_ =: path
      , filter_ =: urlId adjustmentId
      ]



holdChanges :: (Reflex t, MonadHold t m) => a -> Event t a -> m (Event t (a, a))
holdChanges initial e = flip attach e <$> hold initial e


type SceneAction t m = Workflow t m (Dynamic t Action)


action :: AppBuilder t m => m (Dynamic t Cursor, Event t (SceneAction t m)) -> SceneAction t m
action m = Workflow $ over _1 (fmap f) <$> m
   where f cursor = Action cursor True Nothing

editAction :: AppBuilder t m => m (Dynamic t Cursor, Dynamic t (Maybe Edit), Event t (SceneAction t m)) -> SceneAction t m
editAction m = Workflow $ do
    (cursor, edit, transitions) <- m
    return (f <$> cursor <*> edit, transitions)
      where f cursor edit = Action cursor True edit


addAnnotation :: AppBuilder t m => Scene t -> Event t Annotation -> m ()
addAnnotation scene add = editCommand (addEdit <$> current nextId <@> add)
  where nextId = view #nextId <$> scene ^. #document



makeBox :: Position -> Position -> Box
makeBox p1 p2 = Box (liftI2 min p1 p2) (liftI2 max p1 p2)

drawBoxes :: AppBuilder t m => Scene t -> SceneInputs t -> Event t () -> m ()
drawBoxes scene SceneInputs{..} _ =
  addShapes scene . filterMaybe =<< workflowView (idle Nothing)

  where
    idle r = Workflow $ do
      return (r, drawing <$> (current mouse <@ click LeftButton))

    drawing p1 = Workflow $ do
      let box = makeBox p1 <$> mouse
          done = current box <@ click LeftButton

      boxElem  [class_ =: "outline"] box
      return (Nothing, idle . Just . BoxShape <$> done)





drawPolygons :: AppBuilder t m => Scene t -> SceneInputs t -> Event t () -> m ()
drawPolygons scene SceneInputs{..} finish =
  addShapes scene . filterMaybe =<< workflowView (idle Nothing)

  where
    idle r = Workflow $ do
      return (r, drawing . pure <$> (current mouse `tag` click LeftButton))

    drawing points = Workflow $ do
      let points'  = (`NE.cons` points) <$> mouse
          shape     = PolygonShape (Polygon points)
          next     = current points' `tag` click LeftButton

      polygonElem  [class_ =: "outline"] (Polygon <$> points')
      return (Nothing, leftmost
        [ idle (Just shape) <$ finish
        , drawing <$> next
        ])


drawCircles :: AppBuilder t m => Scene t -> SceneInputs t -> Event t () -> m ()
drawCircles scene SceneInputs{..} finish = do
  prefCommand (ZoomBrush <$> wheel)
  circleElem [class_ =: "outline"] cursor

  addShapes scene (CircleShape <$> current cursor `tag` click LeftButton)

  where
    brushSize = view #brushSize <$> (scene ^. #preferences)
    cursor = Circle <$> mouse <*> brushSize


drawLines :: AppBuilder t m => Scene t -> SceneInputs t -> Event t () -> m ()
drawLines scene SceneInputs{..} finish = do
  prefCommand (ZoomBrush <$> wheel)
  circleElem [class_ =: "outline"] cursor


  addShapes scene . filterMaybe =<< workflowView (idle Nothing)

  where
    brushSize = view #brushSize <$> (scene ^. #preferences)
    cursor = Circle <$> mouse <*> brushSize

    idle r = Workflow $ do
      return (r, drawing . pure <$> (current cursor `tag` click LeftButton))

    drawing points = Workflow $ do
      let points'  = (`NE.cons` points) <$> cursor
          shape    = LineShape (WideLine points)
          next     = current points' `tag` click LeftButton

      lineElem "draw" [class_ =: "outline"] (WideLine <$> points')
      return (Nothing, leftmost
        [ idle (Just shape) <$ finish
        , drawing <$> next
        ])


addShapes :: AppBuilder t m => Scene t -> Event t Shape -> m ()
addShapes scene e = addAnnotation scene (makeAnnotation e)
  where
    makeAnnotation e = create <$> current (scene ^. #currentClass) <@> e
    create label shape = Annotation
      { shape
      , label
      , detection = Nothing
      , confirm   = True
      }


alterPart ::  AnnotationId -> (Set Int -> Set Int) -> DocParts -> DocParts
alterPart k f = M.alter f' k where
  f' p = if null result then Nothing else Just result
    where result = f (fromMaybe mempty p)


togglePart :: EditorDocument -> DocPart -> DocParts -> DocParts
togglePart doc (k, sub) = alterPart k $ \existing ->
  case sub of
    Nothing -> if existing == allParts then S.empty else allParts
    Just i  -> toggleSet i existing

  where
    allParts = subParts doc k
    toggleSet i s = if S.member i s then S.delete i s else S.insert i s

addPart :: EditorDocument -> DocPart -> DocParts -> DocParts
addPart doc part = mergeParts (toParts doc part)

toParts :: EditorDocument -> DocPart -> DocParts
toParts doc (k, p) = case p of
  Nothing -> M.singleton k (subParts doc k)
  Just i  -> M.singleton k (S.singleton i)

selectChange :: Set Key -> EditorDocument -> DocParts -> Maybe DocPart -> DocParts
selectChange keys doc existing target
  | S.member Key.Shift keys = fromMaybe existing (flip (addPart doc) existing <$> target)
  | otherwise               = fromMaybe mempty (toParts doc <$> target)


selectParts :: Reflex t => Scene t -> Event t DocParts
selectParts Scene{document, selection, input, shortcut} =
  selectChange <$> current keyboard <*> current document <*> current selection <@> partsClicked where

    partsClicked = leftmost
      [ Just <$> mouseDownOn
      , Nothing <$ mouseDown LeftButton
      ]
    SceneInputs{keyboard, mouseDown, mouseDownOn} = input


boxQuery :: EditorDocument -> Box -> DocParts
boxQuery EditorDocument{annotations} box = M.mapMaybe (queryShape . view #shape) annotations where
  queryShape shape | getBounds shape `intersectBoxBox` box = queryParts shape
                   | otherwise = Nothing

  queryParts (CircleShape c) =  defaults (Just (S.singleton 0)) (intersectBoxCircle box c)
  queryParts (BoxShape b) =  maybeParts (intersectBoxPoint box <$> boxVertices' b)
  queryParts (PolygonShape p) = Nothing
  queryParts (LineShape p)    = Nothing

  maybeParts bs = case catMaybes (imap f bs) of
    []    -> Nothing
    parts -> Just (S.fromList parts)

  f i b = if b then Just i else Nothing




maskOut :: AppBuilder t m => Dim -> Dynamic t (Maybe Box) -> m ()
maskOut dim box = void $ do
  path_ [ d_ ~: shadowPath, class_ =: "shadow" ]
  -- path_ [ d_ ~: boxPath,    class_ =: "dotted" ]

    where
      imageBox = Box (V2 0 0) (fromDim dim)
      overlap    = (>>= boxIntersection imageBox) <$> box

      shadowPath = fromMaybe [] . fmap (maskPath imageBox) <$> overlap
      boxPath    = fromMaybe [] . fmap (polygon . boxVertices') <$> overlap



m :: V2 Float -> PathCommand
m (V2 x y) = M x y

l :: V2 Float -> PathCommand
l (V2 x y) = L x y

polygon :: [V2 Float] -> [PathCommand]
polygon (v:vs) = (m v : fmap l vs) <> [Z]

maskPath :: Box -> Box -> [PathCommand]
maskPath outer inner = polygon (boxVertices' outer) <> polygon (reverse (boxVertices' inner))



actions :: AppBuilder t m => Scene t -> m (Dynamic t Action)
actions scene@Scene{..} = holdWorkflow $
  commonTransition (base <$ cancel) base where

  base = action $ do
    let beginPan    = pan <$> mouseDownAt
        beginSelectRect = rectSelect <$> gate (current holdingShift) mouseDownAt

        beginDraw   = (drawMode <$> current currentClass <*> current config) `tag` keyDown Key.Space
        beginDragSelection   = filterMaybe $ drag <$> current mouse <*> current document <@> selectionClick

    viewCommand zoomCmd
    command SelectCmd $ leftmost [selectAll, selectionClick]

    editCommand $ filterMaybe $ deletePartsEdit <$> current selection <*> current document
       <@ select shortcut ShortDelete

    docCommand (const DocUndo) (select shortcut ShortUndo)
    docCommand (const DocRedo) (select shortcut ShortRedo)

    return (defaultCursor, leftmost [beginSelectRect, beginDragSelection, beginDraw, beginPan, selectArea <$ select shortcut ShortArea])

  -- Translate dragged annotations
  drag origin doc target
        | null   target = Nothing
        | otherwise     = Just $ editAction $ do
    let offset  = mouse - pure origin
        endDrag = mouseUp LeftButton

    scale <- foldDyn (\z -> max 0.1 . (z *)) 1.0 (wheelZoom <$> wheel)

    let maybeEdit s t = do
          guard $ abs (s - 1.0) > eps || norm t > eps
          return $ transformPartsEdit (s, t) target doc
        edit = maybeEdit <$> scale <*> offset

        pointer e = if isJust e then "pointer" else "default"

    editCommand $ filterMaybe (current edit <@ endDrag)
    return (pointer <$> edit, edit, base <$ endDrag)

  -- Draw boxes
  drawMode k config = action $ do
    let finish = keyUp Key.Space

    for_ (M.lookup k (config ^. #classes)) $ \classConfig ->
      case classConfig ^. #shape of
        BoxConfig     -> drawBoxes scene input finish
        CircleConfig     -> drawCircles scene input finish
        PolygonConfig -> drawPolygons scene input finish
        LineConfig    -> drawLines scene input finish


    return ("crosshair", base <$ finish)


  selectArea = editAction $ do
    let  imageBox = Box (V2 0 0) (fromDim (snd image))
         maybeIntersection b = do
            guard (boxArea b > eps)
            boxIntersection imageBox b

    selectedArea <- switchHold def $ ffor mouseDownAt $ \p1 ->
      (maybeIntersection . makeBox p1 <$> mouse)

    let doneEdit      = current selectedEdit `tag` mouseUp LeftButton
        selectedEdit = (Just . setAreaEdit) <$> selectedArea

    editCommand (filterMaybe doneEdit)
    return ("crosshair", selectedEdit, base <$ doneEdit)


  rectSelect p1 = action $ do
    let box = makeBox p1 <$> mouse
        done = current box <@ click LeftButton

        parts  = boxQuery <$> current document <*> current box
        parts' = mergeParts <$> current selection <*> parts

    command SelectCmd (parts' `tag` done)

    boxElem  [class_ =: "outline"] box
    return (defaultCursor, base <$ done)


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

  selectAll        = documentParts <$> current document `tag` select shortcut ShortSelectAll
  selectionClick   = selectParts scene


  mouseDownAt = current mouse <@ mouseDown LeftButton

  holdingShift   = S.member Key.Shift <$> keyboard
  defaultCursor = (\b -> if b then "copy" else "default") <$> holdingShift
  eps = 1e-2


sceneView :: AppBuilder t m => Scene t -> m (Dynamic t Action, Event t (DocPart, SceneEvent))
sceneView scene@Scene{..} = do
    imageView image

    classMap     <- holdUniqDyn (classProperties <$> config <*> preferences)
    instanceCols <- holdUniqDyn (view #instanceColours <$> preferences)
    threshold    <- holdUniqDyn (view #threshold <$> preferences)

    events <- holdMergePatched =<< incrementalMapWithUpdates annotations (\k ->
       annotationView classMap threshold instanceCols (isSelected k) k)

    maskOut (snd image) (view #validArea <$> currentEdit)

    action <- actions scene


    return (action, minElem <?> events)
      where
          isSelected = fanDynMap selection
