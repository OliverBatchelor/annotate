module Scene.Types where

import Annotate.Prelude

import Reflex.Classes
import Data.Semigroup
import Data.Default

import Annotate.Geometry
import Annotate.Common

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
  , keyPress :: Key -> Event t ()

  , keysDown    :: Event t Key
  , keysUp      :: Event t Key
  , keysPressed :: Event t Key

  , keyboard :: Dynamic t (Set Key)
  , hover :: Dynamic t (Set AnnotationId)

  , mouse    :: Dynamic t Position
  , pageMouse :: Dynamic t Position

  , downOn   :: Button -> Event t (Set AnnotationId)
  , keyCombo  :: Key -> [Key] -> Event t ()

} deriving Generic



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

  , selection :: Dynamic t (Set AnnotationId)
  , annotations  :: Patched t (PatchMap AnnotationId Annotation)

  , nextId       :: Dynamic t AnnotationId
  , currentClass :: Dynamic t ClassId
  , config       :: Dynamic t Config
  
  , shortcut     :: Shortcut -> Event t ()
  } deriving (Generic)
