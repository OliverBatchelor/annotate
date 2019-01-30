module Client.Select where

import Annotate.Prelude
import Client.Common
import Client.Widgets

import Builder.Html



type Selectable t m = Dynamic t Bool -> m (Event t ())

tab :: forall t m. Builder t m => Text -> Text -> Selectable t m
tab iconName t = \active ->
  li [class_ =: "nav-item"] $ do
    e <- a_ [classList ["nav-link", "active" `gated` active]] $
      iconTextH t ( (def :: IconConfig t)  & #name .~ pure iconName)
    return (domEvent Click e)



tabs :: (Builder t m) => Int -> [(m (), Selectable t m)] -> m ()
tabs initial items = column "expand" $ mdo
    click <- ul [class_ =: "nav nav-tabs background-light enable-cursor "] $
      selectable openTab (pure (enumerate header))

    openTab <- holdDyn initial click
    tabContent openTab (enumerate content)
  where
    (content, header) = split items



selectable :: (Ord k, Builder t m) => Dynamic t k -> Active t [(k, Selectable t m)] -> m (Event t k)
selectable selected items = active (fmap leftmost . traverse f <$> items)
    where
      f (k, item) = fmap (const k) <$> item (isOpen k)
      isOpen = fanDyn selected

tabContent :: (Ord k, Builder t m) =>  Dynamic t k -> [(k, m ())] -> m ()
tabContent selected items = void $ div_ [class_ =: "tab-content p-0 pt-2 grow"] $ traverse_ item items
  where
    item (k, m) = div_ [classList ["tab-pane h-100", "active" `gated` isOpen k]] m
    isOpen = fanDyn selected



selectPaged ::  (Ord k, Builder t m)
            => Dynamic t Int
            -> Dynamic t Int
            -> Dynamic t [(k, a)]
            -> (Dynamic t (Maybe (k, a)) -> Dynamic t Bool -> m (Event t b))
            -> Dynamic t (Maybe k)
            -> m (Event t b)
selectPaged size offset items buildChild selected = do
  m <- dynList buildChild' items'
  switchHold never (leftmostMap <$> m)
    where

    items' = range <$> offset <*> size <*> items
    range off n = take n . (<> repeat Nothing) . fmap Just . drop off

    buildChild' _ item = buildChild item isSelected where
      isSelected = liftA2 (==) (fmap fst <$> item) selected

    lookupItem i xs = preview (ix i) xs

selectTable' :: (Ord k, Builder t m) =>  Dynamic t (Maybe k) -> Active t [(k, m ())] -> m (Event t k)
selectTable' selected items = do
  e <- selectTable selected (fmap (over _1 Just) <$> items)
  return (filterMaybe e)

selectTable :: (Ord k, Builder t m) =>  Dynamic t k -> Active t [(k, m ())] -> m (Event t k)
selectTable selected items = table [class_ =: "table table-hover"] $ do
  tbody [] $ selectable selected (fmap (over _2 row) <$> items)

  where
    row item = \active -> domEvent Click <$>
      tr_ [class_ ~: gated "table-active" active] item
