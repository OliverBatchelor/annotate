module Reflex.Classes
  ( module Reflex.Classes
  , module Reflex.Dom
  ) where

import Common

import qualified Reflex as R
import Reflex.Dom hiding (switchHold, switch, (=:), sample, Builder, link)

import Data.Functor
import Data.String

import Control.Applicative

import Language.Javascript.JSaddle (MonadJSM)

type GhcjsBuilder t m = (Builder t m, TriggerEvent t m, MonadJSM m, HasJSContext m, MonadJSM (Performable m), DomBuilderSpace m ~ GhcjsDomSpace, PerformEvent t m)
type Builder t m = (Adjustable t m, MonadHold t m, DomBuilder t m, MonadFix m, PostBuild t m)


runWithReplace' :: Adjustable t m => Event t (m b) -> m (Event t b)
runWithReplace' e = snd <$> runWithReplace blank e

replaceHold :: (Adjustable t m, SwitchHold t a, MonadHold t m) => m a -> Event t (m a) -> m a
replaceHold initial e = uncurry switchHold =<< runWithReplace initial e

split :: Functor f => f (a, b) -> (f a, f b)
split ab = (fst <$> ab, snd <$> ab)


class Reflex t => Switch t f where
  switch :: f (Event t a) -> Event t a

instance Reflex t => Switch t (Behavior t) where
  switch = R.switch

instance Reflex t => Switch t (Dynamic t) where
  switch = switch . current


class Reflex t => SwitchHold t a where
  switchHold :: MonadHold t m => a -> Event t a -> m a

instance Reflex t => SwitchHold t (Event t a) where
  switchHold e ee = switch <$> hold e ee

instance Reflex t => SwitchHold t (Behavior t a) where
  switchHold = R.switcher

instance Reflex t => SwitchHold t () where
    switchHold _ _ = return ()

instance (Reflex t, SwitchHold t a, SwitchHold t b) => SwitchHold t (a, b) where
    switchHold (a, b) e = liftA2 (,)
      (switchHold a (fst <$> e))
      (switchHold b (snd <$> e))


instance Reflex t => SwitchHold t (Dynamic t a) where
  switchHold d ed = do
    b <- switchHold (current d) (current <$> ed)
    e <- switchHold (updated d) (updated <$> ed)
    buildDynamic (sample b) e


class Reflex t => Sample t f where
    sample :: MonadSample t m => f t a -> m a


instance Reflex t => Sample t Behavior where
  sample = R.sample

instance Reflex t => Sample t Dynamic where
  sample = R.sample . current


infixl 4 <#>
infixl 4 <?>

(<#>) :: Reflex t => Event t (a -> b) -> Behavior t a -> Event t b
(<#>) e b = attachWith (\a f -> f a) b e

(<?>) :: FunctorMaybe f => (a -> Maybe b) -> f a -> f b
(<?>) = fmapMaybe

instance (Reflex t, Num a) => Num (Dynamic t a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate  = fmap negate
  abs     = fmap abs
  signum  = fmap signum
  fromInteger = pure . fromInteger
