module Builder.Svg where

import Prelude
import Reflex.Dom hiding (El, El, El_, link, button, select)


import Builder.Element
import Builder.Attribute

import Data.Text (Text)

svgNs :: Text
svgNs = "http://www.w3.org/2000/svg"

xlinkNs :: Text
xlinkNs = "https://www.w3.org/1999/xlink"

xlink :: Attribute a -> Attribute a
xlink = setNs xlinkNs

svgEl' :: Text -> El'
svgEl' = makeElem (Just svgNs)

svgEl :: Text -> El
svgEl elemName props child = snd <$> makeElem (Just svgNs) elemName props child

svgEl_ :: Text -> El_
svgEl_ elemName props child = fst <$> makeElem (Just svgNs) elemName props child

svgChild_ :: Text -> ElChild_
svgChild_ elemName props = fst <$> makeElem (Just svgNs) elemName props blank

svg' = svgEl' "svg" :: El'


a_ = svgEl_ "a" :: El_


circle_  = svgChild_ "circle" :: ElChild_


cx_         = floatA "cx"                    :: Attribute Float
cy_         = floatA "cy"                    :: Attribute Float

r_         = floatA "r"                    :: Attribute Float

href_         = setNs svgNs (strA "href")  :: Attribute Text
