{-# LANGUAGE OverloadedStrings #-}

import Common hiding (div)

import TextShow
import qualified Data.Text as T

import Data.FileEmbed
import Data.Text.Encoding (decodeUtf8)
import Data.Default
import Data.Monoid

import Html

import qualified Builder.Svg as Svg
import Builder.Svg



import Types

main :: IO ()
main = mainWidgetWithHead headWidget bodyWidget

headWidget = do
  style [] $ text bootstrap
--   style [] $ text stylesheet

  where
    bootstrap = decodeUtf8 $(embedFile "css/bootstrap.min.css")
    stylesheet = decodeUtf8 $(embedFile "style.css")

data Action = Action
  { cursor      :: Text
  , lock        :: Bool
  , pending     :: [Edit]
  } deriving (Generic)

instance Default Action where
  def = Action "default" False []


bodyWidget = mdo
  action <- holdDyn (def :: Action) never
  div [draggable_ =: False, style_ ~: cursorStyle <$> action] $
    div [id_ =: "drawing", classes_ ~: attrs <$> action] interface

  return ()

  where
    cursorStyle action = [] -- [("cursor", action ^. #cursor)]
    attrs action = ["expand"] <> if (action ^. #lock) then ["cursor_lock"] else []


interface = void $ svg' [class_ =: "expand"] $ do
  Svg.a_ [Svg.href_ =: "www.google.com"] $
    void $ circle_ [cx_ =: 100, cy_ =: 100, r_ =: 100]


-- scene =
