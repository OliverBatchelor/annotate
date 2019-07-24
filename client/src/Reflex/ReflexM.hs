module Reflex.ReflexM where

import Annotate.Prelude
import Reflex

import qualified Data.IntMap as IntMap
import Reflex.Patch.IntMap

import qualified Data.Dependent.Map as DMap
import Reflex.Patch.DMap
import Reflex.Patch.DMapWithMove


newtype ReflexM t a = ReflexM { runReflexM :: forall m. (MonadHold t m, MonadFix m) => m a }

instance Functor (ReflexM t) where
  fmap f (ReflexM a) = ReflexM (f <$> a)
  
instance Applicative (ReflexM t) where
  pure a = ReflexM (pure a)
  (<*>) (ReflexM f) (ReflexM a) = ReflexM (f <*> a) 
  
instance Monad (ReflexM t) where
  return a = ReflexM (return a)
  (>>=) (ReflexM m) f = ReflexM (m >>= runReflexM . f) 

instance MonadFix (ReflexM t) where
  {-# INLINABLE mfix #-}
  mfix f = ReflexM $ mfix (runReflexM . f)

instance MonadSample t (ReflexM t) where
  {-# INLINABLE sample #-}
  sample b = ReflexM (sample b)
    
instance MonadHold t (ReflexM t) where
    {-# INLINABLE hold #-}
    hold v0 e = ReflexM (hold v0 e)

    {-# INLINABLE holdDyn #-}
    holdDyn v0 e = ReflexM (holdDyn v0 e)

    {-# INLINABLE holdIncremental #-}
    holdIncremental v0 e = ReflexM (holdIncremental v0 e)

    {-# INLINABLE buildDynamic #-}
    buildDynamic a0 e = ReflexM (buildDynamic a0 e)

    {-# INLINABLE headE #-}
    headE e = ReflexM (headE e)


instance Reflex t => Adjustable t (ReflexM t) where
  runWithReplace a0 a' = ReflexM $ do 
    r0 <- runReflexM a0
    return (r0, pushAlways runReflexM a')
    
  traverseIntMapWithKeyWithAdjust f m0 m' = do 
    r0 <- IntMap.traverseWithKey (\k v -> f k v) m0
    return (r0,  pushAlways (traverseIntMapPatchWithKey (\k v -> runReflexM (f k v))) m')
     
  traverseDMapWithKeyWithAdjust f m0 m' = do 
    r0 <- DMap.traverseWithKey (\k v -> f k v) m0
    return (r0, pushAlways (traversePatchDMapWithKey (\k v -> runReflexM (f k v))) m')

  traverseDMapWithKeyWithAdjustWithMove f m0 m' = do 
    r0 <- DMap.traverseWithKey (\k v -> f k v) m0
    return (r0, pushAlways (traversePatchDMapWithMoveWithKey (\k v -> runReflexM (f k v))) m')
