module Client.Images where

import Annotate.Prelude hiding (div)
import Annotate.Common hiding (label) 

import Client.Common
import Client.Widgets
import Client.Select
-- import Client.Dialog

import Builder.Html
import qualified Builder.Html as Html

import qualified Data.Map as M

import Data.Time.Format.Human

showImage :: Builder t m => DocName -> DocInfo -> m ()
showImage name DocInfo{..} = 
  void $ row "align-items-center spacing-2 p-1" $ do
    span [] $ text name
    spacer  
    div [hidden_ =: (numAnnotations == 0)] $ 
      icon (def & #name .~ "pencil" & #size .~ IconTiny)


imagesTab :: forall t m. AppBuilder t m => m ()
imagesTab = column "h-100 p-1 v-spacing-2" $ mdo 

  images <- fmap (view #images) <$> view #collection
  selected <- fmap selectedKey <$> view #document 
  
  userSelect <- div [class_ =: "border scroll-grow"] $ do
    selectTable selected (Dyn (M.toList . M.mapWithKey showImage <$> images))
    
  remoteCommand ClientOpen userSelect


  return ()  
    where 
      selectedKey = fromMaybe "" . fmap (view #name)