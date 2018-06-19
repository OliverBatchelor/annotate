module Reflex.Classes
  ( module Reflex.Classes
  , module Reflex.Dom
  , module Reflex.Active
  , (<!>)
  ) where

import Annotate.Common

import qualified Reflex as R
import Reflex.Dom hiding (switchHold, switch, (=:), sample, Builder, link)
import Reflex.Active

import Data.Functor
import Data.Functor.Alt ((<!>))
import Data.String

import Control.Lens (Getting)
import Control.Applicative

import Language.Javascript.JSaddle (MonadJSM)

type GhcjsBuilder t m = (Builder t m, TriggerEvent t m, MonadJSM m, HasJSContext m, MonadJSM (Performable m), DomBuilderSpace m ~ GhcjsDomSpace, PerformEvent t m)
type Builder t m = (Adjustable t m, MonadHold t m, DomBuilder t m, MonadFix m, PostBuild t m)

data Updated t a = Updated a (Event t a)

instance Reflex t => Functor (Updated t) where
  fmap f (Updated initial e) = Updated (f initial) (f <$> e)

runWithReplace' :: Adjustable t m => Event t (m b) -> m (Event t b)
runWithReplace' e = snd <$> runWithReplace blank e

replaceHold :: (Adjustable t m, SwitchHold t a, MonadHold t m) => m a -> Event t (m a) -> m a
replaceHold initial e = uncurry switchHold =<< runWithReplace initial e

replaceFor ::(Adjustable t m, SwitchHold t b, MonadHold t m) => a -> Event t a -> (a -> m b) -> m b
replaceFor initial e f = replaceHold (f initial) (f <$> e)

split :: Functor f => f (a, b) -> (f a, f b)
split ab = (fst <$> ab, snd <$> ab)





class Reflex t => Switch t f where
  switch :: f (Event t a) -> Event t a

instance Reflex t => Switch t (Behavior t) where
  switch = R.switch

instance Reflex t => Switch t (Dynamic t) where
  switch = switch . current


class Reflex t => SwitchPrompt t f where
  switchPrompt :: f (Event t a) -> Event t a

instance Reflex t => SwitchPrompt t (Event t) where
  switchPrompt = R.coincidence

instance Reflex t => SwitchPrompt t (Dynamic t) where
  switchPrompt = switchPromptlyDyn



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

    let eb = current <$> ed

    b <- switchHold (current d) eb
    e <- switchHold (updated d) (updated <$> ed)
    buildDynamic (sample b) (pushAlways sample eb <!> e)



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

filterMaybe :: FunctorMaybe f => f (Maybe a) -> f a
filterMaybe = fmapMaybe id


(<?>) :: FunctorMaybe f => (a -> Maybe b) -> f a -> f b
(<?>) = fmapMaybe

(?>) :: FunctorMaybe f => Getting (First a) s a -> f s -> f a
(?>) getter f = preview getter <?> f


instance (Reflex t, Num a) => Num (Dynamic t a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate  = fmap negate
  abs     = fmap abs
  signum  = fmap signum
  fromInteger = pure . fromInteger




type Patched t p = (PatchTarget p, Event t p)


gated :: Reflex t => Monoid a => a -> Dynamic t Bool -> Dynamic t a
gated a d = ffor d $ \cond -> if cond then a else mempty



postValue :: PostBuild t m => a -> m (Event t a)
postValue a = fmap (const a) <$> getPostBuild

postCurrent :: PostBuild t m => Behavior t a -> m (Event t a)
postCurrent b = tag b <$> getPostBuild

-- Workflow related
mapTransition ::  (MonadHold t m, Reflex t) => (Event t (Workflow t m a) -> Event t (Workflow t m a)) -> Workflow t m a -> Workflow t m a
mapTransition f = mapTransition' (fmap (mapTransition f))


mapTransition' ::  (MonadHold t m, Reflex t) => (Event t (Workflow t m a) -> Event t (Workflow t m a)) -> Workflow t m a -> Workflow t m a
mapTransition' f (Workflow m) = Workflow (over _2 f <$> m)

commonTransition :: (MonadHold t m, Reflex t) => Event t (Workflow t m a) -> Workflow t m a -> Workflow t m a
commonTransition e w = mapTransition (e <!>) w
