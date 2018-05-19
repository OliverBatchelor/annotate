module Builder.Element where


import Prelude
import Reflex.Dom hiding (El', El, El_)

import qualified Data.Map as M
import Data.Map (Map)
import Data.Text (Text)

import Data.Maybe (catMaybes)
import Control.Lens ((.~), (&))

import Builder.Attribute

type El   = forall t m a. (DomBuilder t m, MonadSample t m) => [Property t] -> m a  -> m a
type El_  = forall t m a. (DomBuilder t m, MonadSample t m) => [Property t] -> m () -> m (Element EventResult (DomBuilderSpace m) t)
type El'  = forall t m a. (DomBuilder t m, MonadSample t m) => [Property t] -> m a  -> m (Element EventResult (DomBuilderSpace m) t, a)


type ElChild_  = forall t m a. (DomBuilder t m, MonadSample t m) => [Property t] -> m (Element EventResult (DomBuilderSpace m) t)


makeElem :: (DomBuilder t m, MonadSample t m) => Maybe Namespace -> Text -> [Property t] -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
makeElem namespace elemName properties child = do
  (initials, updates) <- unzip <$> traverse split properties
  let config = def & elementConfig_namespace         .~ namespace
             & elementConfig_initialAttributes .~ M.fromList (catMaybes initials)
             & case catMaybes updates of
                [] -> id
                es -> elementConfig_modifyAttributes  .~ mergeMap (M.fromList es)


  element elemName config child

  where

    split (AttrProp (Attribute f name) b) = do
      i <- initial b
      return ((name,) <$> f i, (name,) . fmapCheap f <$> event b)

    initial (StaticBinding a) = return a
    initial (DynBinding d)    = sample (current d)
    initial (EventBinding (a, e)) = return a

    event (StaticBinding a) = Nothing
    event (DynBinding d)    = Just (updated d)
    event (EventBinding (a, e)) = Just e



--
-- makeElem :: DomBuilder t m => Text -> [Property t] -> m (Element er (DomBuilderSpace m) t, a)
-- makeElem
