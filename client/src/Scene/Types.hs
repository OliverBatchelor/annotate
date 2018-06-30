module Scene.Types where

import Annotate.Common

import Reflex.Classes
import Data.Semigroup
import Data.Default

import Annotate.Geometry
import Annotate.Types

import Input.Events

import Client.Common

data SceneInputs t = SceneInputs
  { mouseDown :: Button -> Event t ()
  , mouseUp   :: Button -> Event t ()
  , click     :: Button -> Event t ()

  , wheel     :: Event t Float
  , focus     :: Event t Bool

  , keyUp   :: Key -> Event t ()
  , keyDown :: Key -> Event t ()

  , keysDown   :: Event t Key
  , keysUp   :: Event t Key

  , keyboard :: Dynamic t (Set Key)
  , hover :: Dynamic t (Set ObjId)

  , mouse    :: Dynamic t Position
  , pageMouse :: Dynamic t Position
} deriving Generic



data Action = Action
  { cursor      :: Text
  , lock        :: Bool
  } deriving (Generic, Eq, Show)

instance Default Action where
  def = Action "default" False

data Viewport = Viewport
  { image    :: Size
  , window    :: Size
  , pan     :: Position
  , zoom    :: Float
  } deriving (Generic, Eq, Show)


type Image = (DocName, Dim)
type Controls = (Float, V2 Float)


data Scene t = Scene
  { image    :: Image
  , viewport :: Dynamic t Viewport
  , input    :: SceneInputs t

  , document :: Dynamic t Document

  , selection :: Dynamic t (Set ObjId)
  , objects  :: Patched t (PatchMap ObjId Object)

  , nextId       :: Dynamic t ObjId
  , currentClass :: Dynamic t ClassId

  } deriving (Generic)
