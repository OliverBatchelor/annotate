module Client.Images where

import Annotate.Prelude hiding (div)
import Annotate.Common hiding (label)

import Client.Common
import Client.Widgets
import Client.Select

import Data.Ord (comparing)
import Data.List (sortBy)

import Builder.Html
import qualified Builder.Html as Html

import qualified Data.Map as M

import Data.Time.Format.Human

showImage :: Builder t m => Dynamic t (Maybe (DocName, DocInfo)) -> Dynamic t Bool -> m (Event t DocName)
showImage d selected = tr [class_ ~: gated "table-active" selected] $ do
  e <- row_ "align-items-center justify-content-between" $ do
    span [] $ dynText (fromMaybe "" <$> name)
    span [] $ dynText (numAnnotations <$> info)
    span [] $ dynText (category <$> info)


    -- div [hidden_ =: (numAnnotations == 0)] $
    --   icon (def & #name .~ "pencil" & #size .~ IconTiny)

  return $ never -- filterMaybe (current name `tag` domEvent Click e)

    where
      numAnnotations = fromMaybe "" . fmap (showText . view #numAnnotations)
      category = fromMaybe "" . fmap (showText . view #category)

      info = fmap snd <$> d
      name = fmap fst <$> d

      -- (Ord k, Builder t m)
      --             => Dynamic t Int
      --             -> Dynamic t Int
      --             -> Dynamic t [(k, a)]
      --             -> (Dynamic t (Maybe (k, a)) -> Dynamic t Bool -> m (Event t b))
      --             -> Dynamic t (Maybe k)
      --             -> m (Event t b)


imagesTab :: forall t m. AppBuilder t m => m ()
imagesTab = column "h-100 p-1 v-spacing-2" $ mdo

  images :: Dynamic t [(DocName, DocInfo)]  <- fmap (M.toList . view #images) <$> view #collection
  selected <- view #userSelected

--  userSelect <- selectPaged size offset (order <*> images) showImage selected
  -- command LoadCmd userSelect


  return ()
    where
      order :: Dynamic t ([(DocName, DocInfo)] -> [(DocName, DocInfo)])
      order = pure (sortBy (comparing (view (_2 . #category))))

      size = pure 20
      offset = pure 0
