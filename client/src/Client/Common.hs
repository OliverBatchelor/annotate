module Client.Common
  ( module Client.Common
  , module Reflex.Classes
  , Key
  ) where

import Annotate.Common hiding ((<>))
import Annotate.Types
import Annotate.Document

import Control.Monad.Reader

import Data.Default
import Data.Semigroup
import Reflex.Classes

import Language.Javascript.JSaddle (MonadJSM)
import Control.Lens (makePrisms)

import Web.KeyCode (Key)


type GhcjsBuilder t m = (Builder t m, TriggerEvent t m, MonadJSM m, HasJSContext m, MonadJSM (Performable m), DomBuilderSpace m ~ GhcjsDomSpace, PerformEvent t m)
type Builder t m = (Adjustable t m, MonadHold t m, DomBuilder t m, MonadFix m, PostBuild t m)
type AppBuilder t m = (Builder t m, EventWriter t AppCommand m, MonadReader (AppEnv t) m)

data ViewCommand
  = ZoomView Float Position
  | PanView Position Position
  deriving (Generic, Show)

data AppCommand
  = ViewCmd ViewCommand
  | DocCmd DocCmd
  | SelectCmd (Set AnnotationId)
  | ClearCmd
  | RemoteCmd ClientMsg
  
  | SidebarCmd 

  deriving (Generic, Show)

instance Semigroup AppCommand where
  a <> b = a
  
data Action = Action
  { cursor      :: Text
  , lock        :: Bool
  } deriving (Generic, Eq, Show)

instance Default Action where
  def = Action "default" False  

data AppEnv t = AppEnv 
  { basePath :: Text 
  , commands :: Event t AppCommand
  , currentDocument :: Dynamic t (Maybe ((DocName, DocInfo), Document))
  , currentAction  :: Dynamic t Action
  
  }

localPath :: MonadReader (AppEnv t) m => Text -> m Text
localPath path = do
  base <- asks basePath
  return $ base <> "/" <> path

remoteCommand :: AppBuilder t m => (a -> ClientMsg) -> Event t a -> m ()
remoteCommand f = command (RemoteCmd . f)

docCommand :: AppBuilder t m => (a -> DocCmd) -> Event t a -> m ()
docCommand f = command (DocCmd . f)

viewCommand :: AppBuilder t m => Event t ViewCommand -> m ()
viewCommand = command ViewCmd

editCommand :: AppBuilder t m => Event t Edit -> m ()
editCommand  = docCommand DocEdit


command :: AppBuilder t m => (a -> AppCommand) -> Event t a -> m ()
command f  = tellEvent . fmap f

command' :: AppBuilder t m => AppCommand -> Event t a -> m ()
command' cmd = command (const cmd)


commandM :: AppBuilder t m => (a -> AppCommand) -> m (Event t a) -> m ()
commandM f m  = m >>= command f

commandM' :: AppBuilder t m => AppCommand -> m (Event t a) -> m ()
commandM' cmd = commandM (const cmd)


clearAnnotations :: Document -> DocCmd
clearAnnotations = DocEdit . Delete . allAnnotations

makePrisms ''AppCommand
