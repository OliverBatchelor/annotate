module Annotate.Geometry
  ( module Linear.V2
  , module Linear.Vector
  , module Annotate.Geometry
  ) where

import Annotate.Prelude

import Linear.V2
import Linear.Vector

import Control.Lens (iso, Simple, Iso)
import Data.List.NonEmpty (NonEmpty(..))

import Data.Semigroup

type Vec = V2 Float

data Box = Box { lower :: Vec, upper :: Vec } deriving (Generic, Show, Eq)
data Circle = Circle {centre :: Vec, radius :: Float} deriving (Generic, Show, Eq)
data Extents = Extents { centre :: Vec, extents :: Vec } deriving (Generic, Show, Eq)

newtype Polygon = Polygon { points :: NonEmpty Vec} deriving (Generic, Show, Eq)
newtype WideLine = WideLine { points :: NonEmpty Circle} deriving (Generic, Show, Eq)


scaleBox :: Vec -> Box -> Box
scaleBox scale = over boxExtents
    (\Extents{..} -> Extents centre (extents * scale))


mergeBoxes :: Box -> Box -> Box
mergeBoxes (Box l u) (Box l' u') = Box (liftI2 min l l') (liftI2 max u u')

instance Semigroup Box where
  (<>) = mergeBoxes


class HasBounds a where
  getBounds :: a -> Box

instance HasBounds Box where
  getBounds = id

instance HasBounds Circle where
  getBounds (Circle c r) = Box (c - r') (c + r')
    where r' = V2 r r


instance HasBounds Extents where
  getBounds Extents{..} = Box (centre - extents) (centre + extents)

instance HasBounds Polygon where
  getBounds (Polygon points) = Box (foldl1 (liftI2 min) points) (foldl1 (liftI2 max) points)

instance HasBounds WideLine where
  getBounds (WideLine points) = getBounds points

instance HasBounds Position where
  getBounds p = Box p p

instance HasBounds a => HasBounds (NonEmpty a) where
  getBounds as = sconcat (fmap getBounds as)

instance HasBounds a => HasBounds [a] where
  getBounds as = case nonEmpty as of
    Nothing -> error "HasBounds [a]: empty list"
    Just as -> getBounds (fmap getBounds as)

type Dim = (Int, Int)


fromDim :: Dim -> V2 Float
fromDim (x, y) = V2 (fromIntegral x) (fromIntegral y)

clamp :: Ord a => (a, a) -> a -> a
clamp (l, u) x = max l (min u x)

boxSize :: Box -> Vector
boxSize (Box l u) = u - l

boxCentre :: Box -> Position
boxCentre Box{..} = centroid [lower, upper]

centroid :: Foldable t => t Position -> Position
centroid ps = sum ps / fromIntegral (length ps)

boxExtents :: Simple Iso Box Extents
boxExtents = iso toExtents fromExtents where
  toExtents Box{..}       = Extents (centroid [lower, upper]) ((upper - lower) ^* 0.5)
  fromExtents = getBounds



data Corner = TopLeft | TopRight | BottomRight | BottomLeft
  deriving (Ord, Enum, Eq, Generic, Show, Bounded)

boxVertices :: Box -> (Position, Position, Position, Position)
boxVertices (Box (V2 lx ly) (V2 ux uy)) =
  ( V2 lx ly
  , V2 ux ly
  , V2 ux uy
  , V2 lx uy
  )


type Position = V2 Float
type Vector = V2 Float

type Size = V2 Float


instance FromJSON Box
instance FromJSON Circle
instance FromJSON Polygon
instance FromJSON WideLine


instance FromJSON Extents
instance FromJSON a => FromJSON (V2 a)

instance ToJSON Box
instance ToJSON Circle
instance ToJSON Polygon
instance ToJSON WideLine

instance ToJSON Extents
instance ToJSON a => ToJSON (V2 a)
