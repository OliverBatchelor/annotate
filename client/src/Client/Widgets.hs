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

toolButton :: Builder t m => Dynamic t Bool -> Text -> Text -> Text -> m (Event t ())
toolButton enabled name icon tooltip = fmap (domEvent Click) $
    button_ [class_ =: "btn btn-secondary enable-cursor", title_ =: tooltip, disabled_ ~: not <$> enabled] $
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

title :: Builder t m => Text -> m (Event t a)
title t = never <$ h5 [class_ =: "modal-title text-center"] (text t)

titleClose :: Builder t m => Text -> m (Event t ())
titleClose t = do
  title t
  e <- button_ [type_ =: "button", class_ =: "close"] $
    span [] $ text "×"
  return (domEvent Click e)

iconText :: Builder t m => (Text, Text) -> Text -> m (Event t a)
iconText (iconClass, icon) content = row "align-items-center spacing-16" $ do
  i [classes_ =: ["fa", "fa-4x", iconClass, icon]] blank
  span [] $ text content
  return never

okDialog :: Builder t m => Text -> m (Event t ()) -> m (Event t ())
okDialog title content = modal (pure True) $ sections
  (titleClose title) (never <$ content)
  (domEvent Click <$> button_ [type_ =: "button", class_ =: "btn btn-primary"] (text "Ok"))


sections :: (Builder t m, Monoid a) => m (Event t a) -> m (Event t a) -> m (Event t a) -> m (Event t a)
sections title content footer = leftmost <$> sequence
  [ div [class_ =: "modal-header"]  title
  , div [class_ =: "modal-body"]    content
  , div [class_ =: "modal-footer"]  footer
  ]



connectingModal :: Builder t m => m ()
connectingModal = modal (pure True) $
  div [class_ =: "modal-header"] $
    h5 [class_ =:"modal-title"] $ text "Connecting..."
