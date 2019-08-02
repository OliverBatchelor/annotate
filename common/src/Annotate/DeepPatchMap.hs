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


data Modify p
  = Add (PatchTarget p)
  | Delete
  | Modify p
  deriving (Generic)

deriving instance (Eq (PatchTarget p), Eq p) => Eq (Modify p)
deriving instance (Show (PatchTarget p), Show p) => Show (Modify p)

newtype DeepPatchMap k p = DeepPatchMap { unDeepPatchMap  :: Map k (Modify p) }
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
