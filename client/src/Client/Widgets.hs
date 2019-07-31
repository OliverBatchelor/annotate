module Client.Widgets where

import Annotate.Prelude hiding (div)
import Annotate.Common (HexColour, showColour, clamp)
import Client.Common

import Reflex.Classes
import Builder.Html hiding (title)
import qualified Builder.Html as Html

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Map as Map

import Data.Default
import Data.Tuple (swap)
import Data.Coerce (coerce)

import qualified GHCJS.DOM.Types as DOM



column :: Builder t m => Text -> m a -> m a
column classes children = div [classes_ =: ["d-flex flex-column", classes]] children


row :: Builder t m => Text -> m a -> m a
row classes children = div [classes_ =: ["d-flex flex-row", classes]] children

row_ :: Builder t m => Text -> m a -> m (ElemType t m)
row_ classes children = div_ [classes_ =: ["d-flex flex-row", classes]] (void children)



spacer :: Builder t m => m ()
spacer = void $ div_ [class_ =: "m-auto"] blank


data IconSize = IconTiny | IconSmall | IconMed | IconLarge
  deriving (Show, Eq, Ord, Generic)

data IconConfig t = IconConfig
  { name :: Active t Text
  , size :: IconSize
  } deriving Generic

instance Reflex t => Default (IconConfig t) where
  def = IconConfig
    { name = "help-circle"
    , size = IconSmall
    }

instance Reflex t => IsString (IconConfig t) where
  fromString name = IconConfig (pure $ T.pack name) IconSmall

bgColour :: Maybe HexColour -> [Style]
bgColour (Just colour) = [("background-color", showColour colour)]
bgColour Nothing = []

iconConf :: Reflex t => IconConfig t
iconConf = def

icon :: Builder t m => IconConfig t -> m ()
icon IconConfig{..} = i [ classes_ ~: activeList ["mdi", (mappend "mdi-") <$> name, pure sizeClass]] blank
  where
    sizeClass = case size of
      IconTiny  -> "mdi-18px"
      IconSmall -> "mdi-24px"
      IconMed   -> "mdi-36px"
      IconLarge -> "mdi-48px"

tinyIcon ::  Builder t m => Active t Text -> m ()
tinyIcon name = icon $ IconConfig name IconTiny
      

smallIcon ::  Builder t m => Active t Text -> m ()
smallIcon name = icon $ IconConfig name IconSmall

medIcon ::  Builder t m => Active t Text -> m ()
medIcon name = icon $ IconConfig name IconMed

centreRow :: Builder t m => m a -> m a
centreRow = row "align-items-center spacing-2"



iconTextH :: Builder t m => Text -> IconConfig t -> m ()
iconTextH t conf = centreRow $ do
  icon conf
  span [] $ text t


iconTextV :: Builder t m => Text -> IconConfig t -> m ()
iconTextV t conf = column "neg-v-spacing-3" $ do
  icon conf
  span [class_ =: "small"] $ text t


closeButton :: Builder t m => m (Event t ())
closeButton =  do
  e <- button_ [type_ =: "button", class_ =: "close"] $ span [] $ text "×"
  return (domEvent Click e)


iconButton :: Builder t m => Dynamic t Bool -> Text -> IconConfig t -> Text -> m (Event t ())
iconButton enabled name conf tooltip = fmap (domEvent Click) $
    button_ [class_ =: "btn btn-secondary enable-cursor", title_ =: tooltip, disabled_ ~: not <$> enabled] $
      iconTextH name conf

iconButton' :: Builder t m => Text -> IconConfig t -> Text -> m (Event t ())
iconButton' = iconButton (pure True)

toolButtonClasses :: [Text]
toolButtonClasses = ["btn", "btn-secondary", "enable-cursor", "pt-0", "pb-0"]

toolButton :: Builder t m => Dynamic t Bool -> Text -> IconConfig t -> Text -> m (Event t ())
toolButton enabled name conf tooltip = fmap (domEvent Click) $
    button_ [classes_ =: toolButtonClasses, title_ =: tooltip, disabled_ ~: not <$> enabled] $
      iconTextV name conf

    --   <div class="dropdown-menu" aria-labelledby="dropdownMenuButton">
    --   <a class="dropdown-item" href="#">Action</a>
    --   <a class="dropdown-item" href="#">Another action</a>
    --   <a class="dropdown-item" href="#">Something else here</a>
    -- </div>

toolSplit :: AppBuilder t m => Dynamic t Bool -> [(Text, a)] -> m (Event t a)
toolSplit enabled options =  do
  cancels <- view #cancel
  button_ [classes_ =: classes, disabled_ ~: not <$> enabled] $ return ()

  div_ [class_ =: "dropdown-menu show", style_ =: dropStyle] $ forM_ options $ \(t, v) -> 
    a_ [class_ =: "dropdown-item"] $ text t
    
  return never
  where
    classes = toolButtonClasses <> ["dropdown-toggle", "dropdown-toggle-split"]

    dropStyle = [ ("position", "absolute")
            , ("will-change", "transform") 
            , ("height", "7em")
            ]


toolButtonToggle :: Builder t m => Dynamic t Bool -> Dynamic t Bool -> Text -> IconConfig t -> Text -> m (Event t Bool)
toolButtonToggle enabled active name conf tooltip = do
    e <- button_ [classes_ ~: classes <$> active, title_ =: tooltip, disabled_ ~: not <$> enabled ] $
          iconTextV name conf

    return $ attachWith (const . not) (current active) (domEvent Click e)
      where classes isActive = if isActive then ("active" : toolButtonClasses) else toolButtonClasses


toolButton' :: Builder t m => Text -> IconConfig t -> Text -> m (Event t ())
toolButton' = toolButton (pure True)


preload :: (AppBuilder t m) => Dynamic t Text -> m (ElemType t m)
preload file =  do
    base <- view #basePath
    let toAbsolute path = base <> "/images/" <> path
    link_ [rel_ =: ["preload"], href_ ~: toAbsolute <$> file]


loadImage :: (AppBuilder t m) => Dynamic t Image -> m DOM.HTMLImageElement
loadImage image = coerce . _element_raw <$> do
  base <- view #basePath
  let toAbsolute path = base <> "/images/" <> path
  img_ [src_ ~: toAbsolute <$> file, width_ ~: fromIntegral <$> width, height_ ~: fromIntegral <$> height, hidden_ =: True] 
    where 
      (file, dim) = split image
      (width, height) = split dim


groupPane :: AppBuilder t m => Text -> m a -> m a
groupPane title children = column "v-spacing-2 p-2 border" $ do
    h6 [class_ =: "font-weight-bold m-0"] $ text title
    children

pane :: AppBuilder t m => m a -> m a
pane = column "v-spacing-2 p-2 border"
    

sidePane :: AppBuilder t m => m a -> m a
sidePane = column "h-100 v-spacing-2" 


buttonGroup :: Builder t m => m a -> m a
buttonGroup inner = div [class_ =: "btn-group enable-cursor"]  inner


splitUpGroup :: Builder t m => m a -> m a
splitUpGroup inner = div [class_ =: "btn-group enable-cursor dropup"]  inner



timeout :: GhcjsBuilder t m => (Event t a, Event t a) -> NominalDiffTime -> m (Event t a)
timeout (down, up) time = do
  delayed <- delay time down
  let gateDelay = do
       isDown <- hold True (False <$ leftmost [up, delayed])
       return $ gate isDown delayed

  switchHold never $  pushAlways (const gateDelay) down


selectOption' :: (Eq a, Builder t m, Show a) => [Property t] -> [(Text, a)] -> a -> Event t a -> m (Dynamic t a)
selectOption' props options initial setter = fmap fromText . _selectElement_value <$>
    selectElem_  props config (traverse_ makeOption options)

    where
      fromText t = fromMaybe initial (lookup t options)
      toText a   = fromMaybe "" $ lookup a (swap <$> options)

      makeOption (t, _) = option [value_ =: t] $ text t
      config  = def & selectElementConfig_initialValue .~ toText initial
                    & selectElementConfig_setValue .~ (toText <$> setter)


selectOption :: (Eq a, Builder t m, Show a) =>  [(Text, a)] -> a -> Event t a -> m (Dynamic t a)
selectOption = selectOption' [class_ =: "custom-select"]


-- selectOption :: Builder t m => [Property t] -> [(Text, a)] -> a -> Event t a -> m (Dynamic t a)
selectView' :: (Builder t m, Eq a, Show a) => [Property t]  -> [(Text, a)] -> Dynamic t a -> m (Event t a)
selectView' props options = toView (selectOption' props options option)
  where option = snd (L.head options)

selectView :: (Builder t m, Eq a, Show a) => [(Text, a)] -> Dynamic t a -> m (Event t a)
selectView = selectView' [class_ =: "custom-select"]



labelled :: Builder t m => Text -> m a -> m a
labelled t inner = row "align-items-stretch " $ do
  label [class_ =: "grow-1 align-self-center m-0"] $ text t
  div [class_ =: "grow-2"] inner

toView :: (Builder t m, Eq a, Show a) => (Event t a -> m (Dynamic t a)) -> Dynamic t a -> m (Event t a)
toView makeWidget value = do
    postBuild <- getPostBuild

    rec
      value' <- makeWidget $ leftmost
        [ (attachPromptlyDynWithMaybe filterEq value' (updated value))
        , current value `tag` postBuild
        ]

    return (updated value')


filterEq :: (Eq a) => a -> a -> Maybe a
filterEq x y = if x == y then Nothing else Just y


rangeSlider :: (Builder t m, Read a, Show a, Num a, Ord a) => (a, a) -> a -> a -> Event t a -> m (Dynamic t a)
rangeSlider (l, u) step initial setter = do

  rec
    slider <- inputElem
        [ type_ =: "range", showA "min" =: l, showA "max" =: u
        , showA "step" =: step, class_ =: "custom-range"] $ def

          & inputElementConfig_setValue      .~ (textValue <$> setter)
          & inputElementConfig_initialValue  .~ (textValue initial)

  holdDyn initial (read . T.unpack <$> _inputElement_input slider)

    where
      textValue = T.pack . show

rangeView :: (Builder t m, Read a, Show a, Num a, Eq a, Ord a) => (a, a) -> a -> Dynamic t a -> m (Event t a)
rangeView range step = toView (rangeSlider range step (fst range))




grow :: forall t m a. Builder t m => m a -> m a 
grow = div [class_ =: "grow-1"]

grow2 :: forall t m a. Builder t m => m a -> m a 
grow2 = div [class_ =: "grow-2"]

grow3 :: forall t m a. Builder t m => m a -> m a 
grow3 = div [class_ =: "grow-3"]  

grow5 :: forall t m a. Builder t m => m a -> m a 
grow5 = div [class_ =: "grow-5"]  

rangePreview :: (Builder t m, Read a, Show a, Num a, Eq a, Ord a) => (a -> Text) -> (a, a) -> a -> Dynamic t a -> m (Event t a)
rangePreview showValue range step value = row "spacing-3 align-items-center" $ do
  inp <- grow5 $ rangeView range step value
  span [ class_ =: "grow-1 text-right" ] $ dynText $ (showValue <$> value)
  return inp


  
checkboxLabel :: Builder t m => Text -> Text -> Dynamic t Bool -> m (Event t Bool)
checkboxLabel i t value = div [class_ =: "custom-control custom-checkbox"] $ do
    let attrs = Map.fromList [("class", "custom-control-input"), ("id", i)]

    inp <- checkboxView (pure attrs) value
    Html.label [class_ =: "custom-control-label", Html.for_ =: i] $ text t

    return inp
 

-- pushButtonView :: forall t m. Builder t m =>  Dynamic t Bool -> Dynamic t Bool -> Text -> IconConfig t -> m (Event t Bool)
-- pushButtonView enabled pressed tooltip name = fmap (domEvent Click) $   
--     button_ [class_ =: toolButtonClasses, title_ =: tooltip, disabled_ ~: not <$> enabled] $
--       iconTextV name conf


toggleButtonView :: forall t m. Builder t m => (Text, Text) -> Dynamic t Bool -> m (Event t Bool)
toggleButtonView icons d = do 
  e <- button_ [class_ =: "btn btn-light"] $
    icon ( (def :: IconConfig t) & #name .~ Dyn (swapping icons d))
  return $ not <$> current d `tag` domEvent Click e

toggleButton :: forall t m. Builder t m => (Text, Text) -> m (Dynamic t Bool)
toggleButton icons = do
  rec
    e <- toggleButtonView icons isOpen
    isOpen <- toggle False e

  return isOpen
    