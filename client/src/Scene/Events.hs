module Scene.Events  where

import Annotate.Prelude
import qualified Input.Events as E


import Scene.Types
import Client.Common

import Scene.Viewport (toLocal)
import Annotate.Common

import qualified Data.Set as S
import qualified Data.Map as M

import qualified Data.Dependent.Map as DM

import Data.Dependent.Map (DMap)
import Data.Dependent.Sum

import qualified Web.KeyCode as Key





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
           => Behavior t Viewport -> Event t (DocPart, SceneEvent) -> E.Inputs t  -> m (SceneInputs t)
holdInputs viewport sceneEvents inp = do

  mousePos      <- holdDyn (V2 0 0) (E.mouseMove inp)
  localMousePos <- holdDyn (V2 0 0) (toLocal <$> viewport <@> E.mouseMove inp)

  let eventType e (part, e') = if e == e' then Just part else Nothing
      sceneEvent e = eventType e <?> sceneEvents

  hover <- holdDyn Nothing $ leftmost
      [ Just          <$> sceneEvent SceneEnter
      , const Nothing <$> sceneEvent SceneLeave
      ]

  rec
    keys <- holdKeys (E.focus inp) (keysDown, E.keyUp inp)
    let keysDown = attachWithMaybe uniqueKey (current keys) (E.keyDown inp)
        uniqueKey s (k :: Key) = if S.member k s then Nothing else Just k


  let mouseDown = selectEq (E.mouseDown inp)

  return
    SceneInputs
    { pageMouse =  mousePos
    , mouse     =  localMousePos

    , mouseDown = mouseDown
    , mouseUp = selectEq (E.mouseUp inp)
    , click = selectEq (E.click inp)

    , mouseDownOn = sceneEvent SceneDown
    , mouseClickOn = sceneEvent SceneClick

    , mouseDoubleClickOn = sceneEvent SceneDoubleClick


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

    , keyCombo = \k held -> testCombo k (S.fromList held) <?>
        (current keys `attach` E.keyDown inp)
  }


matchShortcuts :: Reflex t => SceneInputs t -> Event t (DMap Shortcut Identity)
matchShortcuts SceneInputs{..} = merge $ DM.fromList
    [ (ShortUndo :=> keyCombo Key.KeyZ [Key.Control])
    , (ShortRedo :=> keyCombo Key.KeyZ [Key.Control, Key.Shift])
    , (ShortDelete :=> keyDown Key.Delete)
    , (ShortCancel :=> keyDown Key.Escape)
    ]
