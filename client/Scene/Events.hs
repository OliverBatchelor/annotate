module Scene.Events where

import Common
import Input.Events
import Scene.Types
import Reflex.Classes

import Scene.Viewport (toLocal)
import Types

toLocal' :: Reflex t =>  Scene t -> Behavior t (Position -> Position)
toLocal' scene  = toLocal <$> current (scene ^. #viewport)
