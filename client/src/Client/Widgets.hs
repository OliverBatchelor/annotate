module Client.Widgets where

import Annotate.Common hiding (div)
import Client.Common

import Reflex.Classes
import Builder.Html hiding (title)


column :: Builder t m => Text -> m a -> m a
column classes children = div [classes_ =: ["d-flex flex-column", classes]] children


row :: Builder t m => Text -> m a -> m a
row classes children = div [classes_ =: ["d-flex flex-row", classes]] children

spacer :: Builder t m => m ()
spacer = void $ div_ [class_ =: "m-auto"] blank

toolButton' :: Builder t m => Text -> Text -> Text -> m (Event t ())
toolButton' = toolButton (pure True)

iconText :: Builder t m => Text -> Text -> m ()
iconText name icon = column "" $ do
  i [classes_ =: ["fa", "fa-2x", icon]] blank
  span [class_ =: "small"] $ text name

toolButton :: Builder t m => Dynamic t Bool -> Text -> Text -> Text -> m (Event t ())
toolButton enabled name icon tooltip = fmap (domEvent Click) $
    button_ [class_ =: "btn btn-secondary enable-cursor", title_ =: tooltip, disabled_ ~: not <$> enabled] $
      iconText name icon


buttonGroup :: Builder t m => m a -> m a
buttonGroup inner = div [class_ =: "btn-group enable-cursor"]  inner


timeout :: GhcjsBuilder t m => (Event t a, Event t a) -> NominalDiffTime -> m (Event t a)
timeout (down, up) time = do
  delayed <- delay time down
  let gateDelay = do
       isDown <- hold True (False <$ leftmost [up, delayed])
       return $ gate isDown delayed

  switchHold never $  pushAlways (const gateDelay) down
