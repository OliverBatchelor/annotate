module Client.Class where

import Annotate.Prelude hiding (div)
import Annotate.Common hiding (label)

import Client.Common
import Client.Widgets
import Client.Select
import Client.Dialog

import Annotate.Editor (isNew)

import Builder.Html
import qualified Builder.Html as Html

import qualified Data.Map as M

showClass :: Builder t m => ClassConfig -> m ()
showClass ClassConfig{shape, colour, name} =
  void $ row "align-items-center spacing-2 p-1" $ do
    span [] $ text name
    spacer
    div [style_ =: bgColour (Just colour)] $
      icon $ shapeIcon shape



shapeIcon :: Reflex t => ShapeConfig -> IconConfig t
shapeIcon ConfigCircle     = "circle-outline"
shapeIcon ConfigBox     = "vector-rectangle"
shapeIcon ConfigPolygon = "vector-polygon"
shapeIcon ConfigLine    = "vector-line"

shapeDesc :: ShapeConfig -> Text
shapeDesc ConfigBox = "Box"
shapeDesc ConfigCircle = "Circle"
shapeDesc ConfigPolygon = "Polygon"
shapeDesc ConfigLine = "Line"


shapeTypes :: M.Map Text ShapeConfig
shapeTypes = M.fromList
  [ ("Box", ConfigBox)
  , ("Circle", ConfigCircle)
  , ("Polygon", ConfigPolygon)
  , ("Line", ConfigLine)
  ]

dialogButton :: Builder t m => Text -> m (Event t ())
dialogButton t = domEvent Click <$>  button_ [type_ =: "button", class_ =: "btn btn-primary"] (text t)


saveDialog :: forall t m. AppBuilder t m => (DocName, ImageCat) -> DocName -> m (Event t ())
saveDialog (currentDoc, category) nextDoc = modal (pure True) $ sections header widgets buttons 
    where
      header = if isNew category 
        then titleClose "Submit annotations?"
        else titleClose "Confirm modifications?"

      widgets = do 
        iconText ("text-warning", "alert") currentDoc
        return never

      buttons = centreRow $ do

        yes <- if isNew category 
          then (do 
            submit <- dialogButton "Submit"
            command (OpenCmd nextDoc . Just) (SubmitNew <$ submit)
            return submit)
          else (do
            confirm <- dialogButton "Confirm"
            command (OpenCmd nextDoc . Just) (SubmitConfirm (Just category) <$ confirm)
            return confirm)
            

        no <- dialogButton "Discard changes"
        discard <- dialogButton "Discard image"

        command (OpenCmd nextDoc . Just) (SubmitDiscard <$ discard)
        command (OpenCmd nextDoc) (Nothing <$ no)

        cancel <- dialogButton "Cancel"
        
        cancels <- askShortcut ShortCancel
        return $ leftmost [yes, no, cancel, cancels, discard]




selectClassDialog :: AppBuilder t m => Set AnnotationId -> m (Event t ())
selectClassDialog selection = modal (pure True) $ sections
  (titleClose "Select class") widget (dialogButton "Cancel")
    where
      widget = do
        classes <- askClasses
        selected <- selectTable (-1) (Dyn (M.toList . fmap showClass <$> classes))
        command (ClassCmd selection) selected

        cancels <- askShortcut ShortCancel
        return $ leftmost [cancels, void selected ]


editClass :: Builder t m => Maybe ClassConfig -> m (Event t ClassConfig)
editClass conf = do
  column "v-spacing-2 p-2 border" $ do
    name <- labelled "Name" $ inputElem  [class_ =: "form-control", disable] $ def &
      inputElementConfig_initialValue .~ fromMaybe "" (view #name <$> conf)

    shape <- labelled "Type" $ selectElem_ [class_ =: "form-control", disable] selectConf $
        forM_ (M.keys shapeTypes) $ \k -> option [value_ =: k] $ text k

    labelled "Colour" $ div_ [class_ =: "border expand", style_ =: bgColour (view #colour <$> conf), disable] blank

    rec
      weighting <- labelled "Weighting" $ do 
        e <- rangePreview printFloat (0.01, 1.0) 0.01 weighting
        holdDyn (fromMaybe 0.25 (view #weighting <$> conf)) e

      countWeight <- labelled "Count Weight" $ do 
        e <- rangePreview showText (0, 10) 1 countWeight
        holdDyn (fromMaybe 1 (view #countWeight <$> conf)) e
  

    update <- row "" $ do
      spacer
      iconButton (pure $ isJust conf) "Update" "content-save" "Update class changes"

    let value = ClassConfig
          <$> current (_inputElement_value name)
          <*> (fromDesc <$> current (_selectElement_value shape))
          <*> pure (fromMaybe def $ view #colour <$> conf)
          <*> current weighting
          <*> current countWeight

    return $ value `tag` update

      where selectConf = def & selectElementConfig_initialValue .~
              fromMaybe "" (shapeDesc . view #shape <$> conf)

            disable = disabled_ =: isNothing conf
            fromDesc desc = fromMaybe ConfigBox $ M.lookup desc shapeTypes




addClassShortcut :: forall t m. AppBuilder t m 
                 => Dynamic t (Map ClassId ClassConfig) 
                 -> Dynamic t (Set AnnotationId)
                 -> Event t Int
                 -> m ()
addClassShortcut classes selection keyEvent = command id $ filterMaybe 
     (selectCmd <$> current classes <*> current selection <@> keyEvent)
  where 
    selectCmd classes selection i = fmap (ClassCmd selection) $  
      (fst <$> selectIndex classes (i - 1))

    selectIndex m i = if i < M.size m 
        then Just $ M.elemAt i m
        else Nothing

    

classesTab :: forall t m. AppBuilder t m => m ()
classesTab = column "h-100 p-0 v-spacing-2" $ do
  classes  <- askClasses
  selected <- view #currentClass
  selection <- view #selection

  (Shortcuts shortcut) <- askShortcuts
  addClassShortcut classes (M.keysSet <$> selection) (shortcut ShortSetClass)

  let selectedClass = M.lookup <$>  selected <*> classes


  userSelect <- div [class_ =: "scroll border"] $ do
    selectTable selected (Dyn (M.toList . fmap showClass <$> classes))

  command (ClassCmd mempty) $ leftmost [userSelect]

  (added, removed) <- row "" $ buttonGroup $ do
    add <- toolButton' "Add" "plus-box" "Add new class"
    remove  <- toolButton (isJust <$> selectedClass) "Remove" "minus-box" "Remove selected class"
    return (nextClass <$> current classes `tag` add, current selected `tag` remove)   

  spacer
  (updated :: Event t ClassConfig) <- switchHold never =<< dyn (editClass <$> selectedClass)

  command id $ leftmost
    [ newClassCmd    <$> added
    , removeClassCmd <$> removed
    , attachWith updateClassCmd (current selected) updated
    ]
    

  return ()
    where
      nextClass classes = fromMaybe 0 ((+1)  <$> maxKey classes)
      newClassCmd k           = ConfigCmd (ConfigClass k (Just $ newClass k))
      removeClassCmd k        = ConfigCmd (ConfigClass k Nothing)
      updateClassCmd k update = ConfigCmd (ConfigClass k (Just update))


showClass' :: Builder t m => Maybe ClassConfig -> m ()
showClass' mConfig = do
  row "align-items-center spacing-2 p-1" $ do
    case mConfig of
      Just ClassConfig{..} -> do
         div [style_ =: bgColour (Just colour)] $
            icon $ shapeIcon shape
         span [] $ text name
      Nothing -> do
        icon "help-circle"
        span [] $ text "Select class"


classToolButton :: forall t m. Builder t m => Dynamic t (Maybe ClassConfig) -> m (Event t ())
classToolButton selected = do
  e <- button_ [class_ =: "btn btn-secondary enable-cursor", title_ =: "Select class to annotate"] $
    dyn_ (showClass' <$> selected)

  return (domEvent Click e)



