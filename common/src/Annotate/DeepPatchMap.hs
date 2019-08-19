{-# LANGUAGE UndecidableInstances  #-}

module Annotate.DeepPatchMap (
  module Annotate.DeepPatchMap,
  Patch(..)

) where

import Reflex.Patch.Class
import Data.Align

import qualified Data.Map as Map
import Data.Map (Map)

import Control.Lens (makePrisms)
import Annotate.Prelude


data Modify p pt
  = Add pt
  | Delete
  | Modify p
  deriving (Generic, Show, Eq)


type Modifies p = Modify p (PatchTarget p)



instance (Patch p, PatchTarget p ~ pt)  => Patch (Modify p pt) where
  type PatchTarget (Modify p pt) = Maybe pt

  apply (Modify p) (Just a) = Just <$> (apply p a)
  apply (Add a) Nothing     = Just (Just a)
  apply Delete (Just _)     = Just Nothing
  apply _ _ = Nothing


newtype DeepPatchMap k p = DeepPatchMap { unDeepPatchMap  :: Map k (Modifies p) }
  deriving (Generic)

deriving instance (Ord k, Eq (PatchTarget p), Eq p) => Eq (DeepPatchMap k p)
deriving instance (Show k, Show (PatchTarget p), Ord k, Show (PatchTarget p), Show p) => Show (DeepPatchMap k p)

instance (Ord k, Patch p) => Patch (DeepPatchMap k p) where
  type PatchTarget (DeepPatchMap k p) = Map k (PatchTarget p)
  apply (DeepPatchMap p) old = Just $ flip Map.mapMaybe (align p old) $ \case
    (These (Modify p) a) -> apply p a
    (This (Add t)) -> Just t
    (That a)       -> Just a
    _ -> Nothing


makePrisms ''Modify
