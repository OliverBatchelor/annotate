module Scene.Types where

import Annotate.Prelude

import Reflex.Classes
import Data.Semigroup
import Data.Default

import Annotate.Geometry
import Annotate.Common
import Annotate.EditorDocument

import Input.Events

import Client.Common

data SceneInputs t = SceneInputs
  { mouseDown :: !(Button -> Event t ())
  , mouseUp   :: !(Button -> Event t ())
  , click     :: !(Button -> Event t ())

  , mouseDownOn :: Event t DocPart
  , mouseClickOn :: Event t DocPart

  , mouseDoubleClickOn :: Event t DocPart

  , wheel     :: !(Event t Float)
  , focus     :: !(Event t Bool)

  , keyUp   :: !(Key -> Event t ())
  , keyDown :: !(Key -> Event t ())
  , keyPress :: !(Key -> Event t ())

  , keysDown    :: !(Event t Key)
  , keysUp      :: !(Event t Key)
  , keysPressed :: !(Event t Key)

  , keyboard :: !(Dynamic t (Set Key))
  , hover :: !(Dynamic t (Maybe DocPart))

  , mouse    :: !(Dynamic t Position)
  , pageMouse :: !(Dynamic t Position)

  , keyCombo  :: !(Key -> [Key] -> Event t ())

} deriving Generic



data Viewport = Viewport
  { image    :: !Size
  , window    :: !Size
  , pan     :: !Position
  , zoom    :: !Float
  } deriving (Generic, Eq, Show)


type Image = (DocName, Dim)
type Controls = (Float, V2 Float)


data Scene t = Scene
  { image    :: !Image
  , input    :: !(SceneInputs t)

  , document :: !(Dynamic t EditorDocument)

  , selection :: !(Dynamic t DocParts)
  , annotations  :: !(Incremental t (PatchMap AnnotationId Annotation))

  , nextId       :: !(Dynamic t AnnotationId)
  , currentClass :: !(Dynamic t ClassId)
  , config       :: !(Dynamic t Config)
  , preferences  :: !(Dynamic t Preferences)

  , shortcut     :: !(EventSelector t Shortcut)
  } deriving (Generic)
