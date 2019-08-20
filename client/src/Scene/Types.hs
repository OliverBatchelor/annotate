{-# LANGUAGE UndecidableInstances #-}

module Scene.Types 
  ( module Scene.Types  

  ) where

import Annotate.Prelude

import Reflex.Classes

import Data.Semigroup
import Data.Default

import Control.Monad.Reader

import Annotate.Geometry
import Annotate.Common
import Annotate.Editor

import Input.Events

import Client.Common
import Scene.Canvas



data SceneInputs t = SceneInputs
  { mouseDown :: !(Button -> Event t Point)
  , mouseUp   :: !(Button -> Event t Point)
  , click     :: !(Button -> Event t Point)

  , mouseDownOn :: !(Button -> Event t DocPart)

  , wheel     :: !(Event t Float)
  , focus     :: !(Event t Bool)

  , keyUp   :: !(Key -> Event t ())
  , keyDown :: !(Key -> Event t ())
  , keyPress :: !(Key -> Event t ())

  , localKeyDown :: !(Key -> Event t ())

  , keysDown    :: !(Event t Key)
  , keysUp      :: !(Event t Key)
  , keysPressed :: !(Event t Key)

  , localKeysDown :: !(Event t Key)

  , keyboard :: !(Dynamic t (Set Key))
  , hover :: !(Dynamic t [DocPart])

  , mouse    :: !(Dynamic t Point)
  , pageMouse :: !(Dynamic t Point)

  , keyCombo  :: !(Key -> [Key] -> Event t ())

} deriving Generic



data Viewport = Viewport
  { image    :: !Dim
  , window    :: !Dim
  , pan     :: !Point
  , zoom    :: !Float
  } deriving (Generic, Eq, Show)


type Controls = (Float, V2 Float)


data Scene t = Scene
  { input    :: !(SceneInputs t)

  , editor      :: !(Dynamic t Editor)
  , selection   :: !(Dynamic t DocParts)
 
  , currentClass :: !(Dynamic t ClassId)
  , config       :: !(Dynamic t Config)
  , preferences  :: !(Dynamic t Preferences)

  , shortcut     :: !(EventSelector t Shortcut)
  , viewport     :: !(Dynamic t Viewport)
  , thresholds   :: !(Dynamic t (Float, Float))

  , query        :: !(Behavior t SceneQuery)
  } deriving (Generic)


newtype ControllerT t m a = ControllerT { unController :: DynamicWriterT t Action m a }
  deriving (Functor, Applicative, Monad, DynamicWriter t Action, MonadFix, MonadHold t, MonadSample t, MonadTrans)


deriving instance EventWriter t w m => EventWriter t w (ControllerT t m)


instance (Adjustable t m, MonadHold t m, MonadFix m) => Adjustable t (ControllerT t m) where
  runWithReplace m e = ControllerT $ runWithReplace (unController m) (unController <$> e)

  traverseIntMapWithKeyWithAdjust f v e = ControllerT $ 
    traverseIntMapWithKeyWithAdjust (\k v -> unController (f k v)) v e
  
  traverseDMapWithKeyWithAdjustWithMove f v e = ControllerT $ 
    traverseDMapWithKeyWithAdjustWithMove (\k v -> unController (f k v)) v e



runController :: (Reflex t, Monad m, MonadFix m) => ControllerT t m () -> m (Dynamic t Action)
runController (ControllerT m) = snd <$> runDynamicWriterT m