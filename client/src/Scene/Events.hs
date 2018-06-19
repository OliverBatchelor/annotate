module Scene.Events  where

import Annotate.Common
import qualified Input.Events as E


import Scene.Types
import Reflex.Classes

import Scene.Viewport (toLocal)
import Annotate.Types

import qualified Data.Set as S



holdKeys :: (MonadFix m, Reflex t, MonadHold t m) => Event t Bool -> (Event t Key, Event t Key) -> m (Dynamic t (Set Key))
holdKeys focus (keyDown, keyUp) = foldDyn ($) S.empty $ mergeWith (.)
    [ const mempty <$ focus
    , S.insert     <$> keyDown
    , S.delete     <$> keyUp
    ]

selectEq :: (Eq a, Reflex t) => Event t a -> a -> Event t ()
selectEq e a = guard . (== a) <?> e

holdInputs :: (MonadFix m, MonadHold t m, Reflex t) => Behavior t Viewport -> E.Inputs t -> m (SceneInputs t)
holdInputs viewport inp = do

  mousePos      <- holdDyn (V2 0 0) (E.mouseMove inp)
  localMousePos <- holdDyn (V2 0 0) (toLocal <$> viewport <@> E.mouseMove inp)

  keys <- holdKeys (E.focus inp) (E.keyDown inp, E.keyUp inp)

  return
    SceneInputs
    { pageMouse =  mousePos
    , mouse     =  localMousePos

    , mouseDown = selectEq (E.mouseDown inp)
    , mouseUp = selectEq (E.mouseUp inp)
    , click = selectEq (E.click inp)

    , keysDown = E.keyDown inp
    , keysUp = E.keyUp inp

    , keyDown = selectEq (E.keyDown inp)
    , keyUp = selectEq (E.keyUp inp)

    , wheel = E.wheel inp
    , focus = E.focus inp

    , keyboard = keys
  }
