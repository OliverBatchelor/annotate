module Scene.Types where

import Annotate.Common

import Reflex.Classes
import Data.Semigroup
import Data.Default

import Annotate.Geometry
import Annotate.Types

import Control.Lens (makePrisms)
import Input.Events


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

  , mouse    :: Dynamic t Position
  , pageMouse :: Dynamic t Position
}



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


data Scene t = Scene
  { image    :: Image
  , viewport :: Dynamic t Viewport
  , input    :: SceneInputs t

  , document :: Dynamic t Document

  , initial :: Map ObjId (ObjectInfo, Object)
  , objects  :: Incremental t (PatchMap ObjId (ObjectInfo, Object))

  , nextId       :: Dynamic t ObjId
  , currentClass :: Dynamic t ClassId

  } deriving (Generic)

data ViewCommand
  = ZoomCmd Float Position
  | PanCmd Position Position
  deriving (Generic, Show)

data AppCommand
  = ViewCmd ViewCommand
  | DocCmd DocCmd

  | SubmitCmd
  | DiscardCmd
  | NextCmd
  | DetectCmd
  deriving (Generic, Show)

instance Semigroup AppCommand where
  a <> b = a

type AppBuilder t m = (Builder t m, EventWriter t AppCommand m)

command :: AppBuilder t m => (a -> AppCommand) -> m (Event t a) -> m ()
command f m  = m >>= tellEvent . fmap f

command' :: AppBuilder t m => AppCommand -> m (Event t a) -> m ()
command' cmd = command (const cmd)


makePrisms ''AppCommand
