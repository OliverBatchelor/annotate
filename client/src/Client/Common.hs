module Client.Common where

import Annotate.Common hiding ((<>))
import Annotate.Types

import Control.Monad.Reader

import Data.Semigroup
import Reflex.Classes

import Language.Javascript.JSaddle (MonadJSM)
import Control.Lens (makePrisms)


type GhcjsBuilder t m = (Builder t m, TriggerEvent t m, MonadJSM m, HasJSContext m, MonadJSM (Performable m), DomBuilderSpace m ~ GhcjsDomSpace, PerformEvent t m)
type Builder t m = (Adjustable t m, MonadHold t m, DomBuilder t m, MonadFix m, PostBuild t m)

type AppBuilder t m = (Builder t m, EventWriter t AppCommand m, MonadReader AppEnv m)

data ViewCommand
  = ZoomView Float Position
  | PanView Position Position
  deriving (Generic, Show)

data AppCommand
  = ViewCmd ViewCommand
  | DocCmd DocCmd
  | SelectCmd (Set ObjId)

  | SubmitCmd
  | DiscardCmd
  | NextCmd
  | DetectCmd

  deriving (Generic, Show)

instance Semigroup AppCommand where
  a <> b = a

data AppEnv = AppEnv { basePath :: Text }

localPath :: MonadReader AppEnv m => Text -> m Text
localPath path = do
  base <- asks basePath
  return $ base <> "/" <> path

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


makePrisms ''AppCommand
