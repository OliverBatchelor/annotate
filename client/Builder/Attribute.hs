module Builder.Attribute where

import Common
import Reflex hiding (Value)
import Reflex.Dom (AttributeName(..))

import Data.Functor.Contravariant
import Data.Text as T
import TextShow


data Attribute a = Attribute { attrConvert :: a -> Maybe Text, attrName :: !AttributeName }

instance Contravariant Attribute where
  contramap f' (Attribute f name) = Attribute (f . f') name

data Binding t a where
  StaticBinding :: a -> Binding t a
  DynBinding    :: Dynamic t a -> Binding t a
  EventBinding  :: (a, Event t a) -> Binding t a


data Property t where
  AttrProp :: Attribute a -> Binding t a -> Property t


infixr 0 =:, -:, ~:

(=:) :: Attribute a -> a -> Property t
(=:) attr a = AttrProp attr (StaticBinding a)

(-:) :: Attribute a -> (a, Event t a) -> Property t
(-:) attr b = AttrProp attr (EventBinding b)

(~:) :: Attribute a -> Dynamic t a -> Property t
(~:) attr d = AttrProp attr (DynBinding d)

setNs :: Text -> Attribute a -> Attribute a
setNs ns (Attribute f (AttributeName _ name)) = Attribute f (AttributeName (Just ns) name)

optional :: Attribute a -> Attribute (Maybe a)
optional (Attribute f name) = Attribute  (>>= f) name

cond :: a -> Attribute a -> Attribute Bool
cond a (Attribute f name) = Attribute (\b -> if b then f a else Nothing) name


sepBy :: Text -> Attribute a -> Attribute [a]
sepBy sep (Attribute f name) = Attribute  f' name where
  f' xs = case catMaybes (f <$> xs) of
      []  -> Nothing
      strs -> Just $ T.intercalate sep strs

commaSep :: Attribute a -> Attribute [a]
commaSep = sepBy ","

spaceSep :: Attribute a -> Attribute [a]
spaceSep = sepBy " "


showA :: TextShow a => Text -> Attribute a
showA = contramap showt . strA

strA :: Text -> Attribute Text
strA name = Attribute Just (AttributeName Nothing name)

commaListA :: Text -> Attribute [Text]
commaListA = commaSep . strA

spaceListA :: Text -> Attribute [Text]
spaceListA = spaceSep . strA

boolA :: Text -> Attribute Bool
boolA name = Attribute (\b -> if b then Just "" else Nothing) (AttributeName Nothing name)

intA :: Text -> Attribute Int
intA = showA

floatA :: Text -> Attribute Float
floatA = showA

ifA :: Text -> Text -> Text -> Attribute Bool
ifA t f = contramap fromBool . strA
  where fromBool b = if b then t else f

styleA :: Text -> Attribute [(Text, Text)]
styleA name = Attribute toStyle (AttributeName Nothing name) where
  toStyle attrs = Just $ T.concat (pair <$> attrs)
  pair (attr, value) = attr <> ":" <> value <> ";"
