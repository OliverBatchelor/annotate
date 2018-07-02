module Scene.Events  where

import Annotate.Common
import qualified Input.Events as E


import Scene.Types
import Client.Common

import Scene.Viewport (toLocal)
import Annotate.Types

import qualified Data.Set as S
import qualified Data.Map as M



holdKeys :: (MonadFix m, Reflex t, MonadHold t m) => Event t Bool -> (Event t Key, Event t Key) -> m (Dynamic t (Set Key))
holdKeys focus (keyDown, keyUp) = foldDyn ($) S.empty $ mergeWith (.)
    [ const mempty <$ focus
    , S.insert     <$> keyDown
    , S.delete     <$> keyUp
    ]

selectEq :: (Eq a, Reflex t) => Event t a -> a -> Event t ()
selectEq e a = guard . (== a) <?> e

testCombo ::  Key -> Set Key -> (Set Key, Key) -> Maybe ()
testCombo key modifiers (held, pressed)
  | key == pressed && modifiers == S.delete key held = Just ()
  | otherwise = Nothing

holdInputs :: (MonadFix m, MonadHold t m, Reflex t)
           => Behavior t Viewport -> Event t (Map ObjId Bool) -> E.Inputs t  -> m (SceneInputs t)
holdInputs viewport hovers inp = do

  mousePos      <- holdDyn (V2 0 0) (E.mouseMove inp)
  localMousePos <- holdDyn (V2 0 0) (toLocal <$> viewport <@> E.mouseMove inp)

  keys <- holdKeys (E.focus inp) (E.keyDown inp, E.keyUp inp)
  hover <- foldDyn ($) S.empty $ mergeWith (.)
      [ const mempty <$ E.focus inp
      , S.union . M.keysSet . M.filter id <$> hovers
      , S.difference . M.keysSet . M.filter not <$> hovers
      ]
  let mouseDown = selectEq (E.mouseDown inp)

  return
    SceneInputs
    { pageMouse =  mousePos
    , mouse     =  localMousePos

    , mouseDown = mouseDown
    , mouseUp = selectEq (E.mouseUp inp)
    , click = selectEq (E.click inp)

    , keysDown = E.keyDown inp
    , keysUp = E.keyUp inp
    , keysPressed = E.keyPress inp


    , keyDown = selectEq (E.keyDown inp)
    , keyUp = selectEq (E.keyUp inp)
    , keyPress = selectEq (E.keyPress inp)

    , wheel = E.wheel inp
    , focus = E.focus inp

    , keyboard = keys
    , hover = hover

    , downOn   = \b -> current hover <@ mouseDown b
    , keyCombo = \k held -> testCombo k (S.fromList held) <?>
        (current keys `attach` E.keyDown inp)
  }
