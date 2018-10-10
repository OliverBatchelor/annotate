module Client.Widgets where

import Annotate.Prelude hiding (div)
import Annotate.Common (HexColour, showColour)
import Client.Common

import Reflex.Classes
import Builder.Html hiding (title)

import qualified Data.Text as T
import Data.Default
import Data.Tuple (swap)

import Text.Printf

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


icon :: Builder t m => IconConfig t -> m ()
icon IconConfig{..} = i [ classes_ ~: activeList ["mdi", (mappend "mdi-") <$> name, pure sizeClass]] blank
  where
    sizeClass = case size of
      IconTiny  -> "mdi-18px"
      IconSmall -> "mdi-24px"
      IconMed   -> "mdi-36px"
      IconLarge -> "mdi-48px"

iconTextH :: Builder t m => Text -> IconConfig t -> m ()
iconTextH t conf = row "align-items-center spacing-2" $ do
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


toolButton :: Builder t m => Dynamic t Bool -> Text -> IconConfig t -> Text -> m (Event t ())
toolButton enabled name conf tooltip = fmap (domEvent Click) $
    button_ [class_ =: "btn btn-secondary enable-cursor pt-0 pb-0", title_ =: tooltip, disabled_ ~: not <$> enabled] $
      iconTextV name conf

toolButton' :: Builder t m => Text -> IconConfig t -> Text -> m (Event t ())
toolButton' = toolButton (pure True)



buttonGroup :: Builder t m => m a -> m a
buttonGroup inner = div [class_ =: "btn-group enable-cursor"]  inner


timeout :: GhcjsBuilder t m => (Event t a, Event t a) -> NominalDiffTime -> m (Event t a)
timeout (down, up) time = do
  delayed <- delay time down
  let gateDelay = do
       isDown <- hold True (False <$ leftmost [up, delayed])
       return $ gate isDown delayed

  switchHold never $  pushAlways (const gateDelay) down


selectOption :: (Eq a, Builder t m) => [Property t] -> [(Text, a)] -> a -> Event t a -> m (Dynamic t a)
selectOption props options initial setter = fmap fromText . _selectElement_value <$>
    selectElem_  props config (traverse_ makeOption options)

    where
      fromText t = fromMaybe initial (lookup t options)
      toText a   = fromMaybe (error "selectOption: missing value") $
        lookup a (swap <$> options)

      makeOption (t, _) = option [value_ =: t] $ text t
      config  = def & selectElementConfig_initialValue .~ toText initial
                    & selectElementConfig_setValue .~ (toText <$> setter)


labelled :: Builder t m => Text -> m a -> m a
labelled t inner = row "align-items-stretch " $ do
  label [class_ =: "grow-1 align-self-center"] $ text t
  div [class_ =: "grow-2"] inner
