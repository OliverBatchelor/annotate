module Annotate.Geometry
  ( module Linear.V2
  , module Linear.Vector
  , module Annotate.Geometry
  ) where

import Annotate.Common

import Linear.V2
import Linear.Vector

import Control.Lens (iso, Simple, Iso)

type Vec = V2 Float

data Box = Box { lower :: Vec, upper :: Vec } deriving (Generic, Show, Eq)
data Circle = Circle {centre :: Vec, radius :: Float} deriving (Generic, Show, Eq)
data Extents = Extents { centre :: Vec, extents :: Vec } deriving (Generic, Show, Eq)

type Dim = (Int, Int)


fromDim :: Dim -> V2 Float
fromDim (x, y) = V2 (fromIntegral x) (fromIntegral y)

clamp :: Ord a => (a, a) -> a -> a
clamp (l, u) x = max l (min u x)

boxSize :: Box -> Vector
boxSize (Box l u) = u - l

centroid :: [Position] -> Position
centroid ps = sum ps / fromIntegral (length ps)

boxExtents :: Simple Iso Box Extents
boxExtents = iso toExtents fromExtents where
  toExtents Box{..}       = Extents (centroid [lower, upper]) ((upper - lower) ^* 0.5)
  fromExtents Extents{..} = Box (centre - extents) (centre + extents)


type Position = V2 Float
type Vector = V2 Float

type Size = V2 Float


instance FromJSON Box
instance FromJSON Circle

instance FromJSON Extents
instance FromJSON a => FromJSON (V2 a)

instance ToJSON Box
instance ToJSON Circle

instance ToJSON Extents
instance ToJSON a => ToJSON (V2 a)