import Annotate.Geometry


class Patch p => InversePatch p where
  -- t `apply` p `apply` (inverse t p) == t
  inverse :: PatchTarget p -> p -> Maybe p
  
  applyInverse :: PatchTarget p -> p -> Maybe (PatchTarget p, p)
  applyInverse t p = liftA2 (,) (apply p t) (inverse t p) 


class Patch p => CompressPatch p where
  compressPatch :: p -> p -> Maybe p

class (Monoid (Selection a), Eq (Selection a)) => HasSelection a where
  type Selection a
  toggle    :: Parts a -> Parts a -> Parts a
  complete  :: a -> Parts a 


  
    

instance (Ord k, Parts a) => HasParts (Map k a) where
  type Parts a = Set k

  toggle = Map.mapMaybe id alignWith f  where
    f (This a) = Just a
    f (That a) = Just a
    f (These a b) = if inner == mempty then 
      where inner = toggle a b

  parts = M.keysSet



