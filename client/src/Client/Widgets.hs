module Client.Widgets where

import Annotate.Common hiding (div)
import Client.Common

import Reflex.Classes
import Builder.Html hiding (title)

import qualified Data.Text as T
import Data.Default

column :: Builder t m => Text -> m a -> m a
column classes children = div [classes_ =: ["d-flex flex-column", classes]] children


row :: Builder t m => Text -> m a -> m a
row classes children = div [classes_ =: ["d-flex flex-row", classes]] children

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

icon :: Builder t m => IconConfig t -> m ()
icon IconConfig{..} = i [classes_ ~: activeList ["mdi", (mappend "mdi-") <$> name, pure sizeClass]] blank
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
