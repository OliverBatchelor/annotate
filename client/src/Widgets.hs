module Widgets where

import Annotate.Common hiding (div)

import Reflex.Classes
import Builder.Html




column :: Builder t m => Text -> m a -> m a
column classes children = div [classes_ =: ["d-flex flex-column", classes]] children

row :: Builder t m => Text -> m a -> m a
row classes children = div [classes_ =: ["d-flex flex-row", classes]] children

spacer :: Builder t m => m ()
spacer = void $ div_ [class_ =: "m-auto"] blank

toolButton :: Builder t m => Text -> Text -> Text -> m (Event t ())
toolButton name icon tooltip = fmap (domEvent Click) $ button_ [class_ =: "btn btn-secondary enable-cursor", title_ =: tooltip] $
  column "" $ do
    i [classes_ =: ["fa", "fa-2x", icon]] blank
    span [class_ =: "small"] $ text name


buttonGroup :: Builder t m => m a -> m a
buttonGroup inner = div [class_ =: "btn-group enable-cursor"]  inner


-- Modal dialogs

modal :: Builder t m => Dynamic t Bool -> m a -> m a
modal shown content = do
  r <- div [classes_ ~: (pure ["modal"] <> ["show"] `gated` shown), role_ =: "dialog", style_ ~: [("display", "block"), ("padding-right", "15px")] `gated` shown ] $
    div [class_ =: "modal-dialog modal-dialog-centered", role_ =: "document"] $
      div [class_ =: "modal-content"]  content

  div [classes_ ~: (pure ["modal-backdrop"] <> ["show"] `gated` shown)] blank
  return r


connectingModal :: Builder t m => m ()
connectingModal = modal (pure True) $
  div [class_ =: "modal-header"] $
    h5 [class_ =:"modal-title"] $ text "Connecting..."
