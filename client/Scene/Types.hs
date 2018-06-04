module Scene.Types where

import Common

import Reflex.Classes
import Data.Default

import Geometry
import Types

import Control.Lens (makePrisms)
import Input.Events



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

data ObjectInfo = ObjectInfo
  { isSelected :: Bool
  } deriving (Generic, Eq, Show)

instance Default ObjectInfo where
  def = ObjectInfo False

type Patched t p = (PatchTarget p, Event t p)

data Scene t = Scene
  { image    :: Image
  , viewport :: Dynamic t Viewport
  , input    :: Inputs t

  , document :: Dynamic t Document
  , objects  :: Patched t (PatchMap ObjId (ObjectInfo, Object))

  } deriving (Generic)

data ViewCommand
  = ZoomCmd Float Position
  | PanCmd Position Position
  deriving (Generic, Show)

data SceneCommand
  = ViewCmd ViewCommand
  | DocCmd DocCmd
  deriving (Generic, Show)

makePrisms ''SceneCommand
