module Client.Common
  ( module Client.Common
  , module Annotate.Editor
  , module Reflex.Classes
  , Render(..)
  , Key
  ) where

import Annotate.Prelude 
import Annotate.Common
import Annotate.Editor
import Annotate.Colour

import Control.Monad.Reader

import Data.Default
import Reflex.Classes

import Scene.Canvas (Render(..))

import qualified Data.Map as Map
import qualified Data.Text as T

import Data.GADT.Compare.TH

import Language.Javascript.JSaddle (MonadJSM)
import Control.Lens (makePrisms)

import Web.KeyCode (Key)
import Text.Printf

import Linear.V3(V3(..))

type Builder t m = (Adjustable t m, MonadHold t m, DomBuilder t m, MonadFix m, PostBuild t m
                   , TriggerEvent t m, HasJSContext m, DomRenderHook t m, MonadIO (Performable m), MonadJSM (Performable m), MonadJSM m, DomBuilderSpace m ~ GhcjsDomSpace,  PerformEvent t m)

type AppEvent t = Event t [AppCommand]
type AppBuilder t m = (Builder t m, CommandWriter t m, MonadReader (AppEnv t) m)

type GhcjsBuilder t m = Builder t m
type GhcjsAppBuilder t m = AppBuilder t m

data ViewCommand
  = ZoomView Float Point
  | PanView Point Point
  deriving (Generic, Show)

data Dialog = ClassDialog DocParts
            | ErrorDialog ErrCode
            | SaveDialog (DocName, ImageCat) DocName

  deriving (Generic, Show)


data PrefCommand
  = ZoomBrush Float
  | SetOpacity Float
  | SetFontSize Int
  | SetBorder Float
  | SetGamma Float
  | SetBrightness Float
  | SetContrast Float

  | SetControlSize Float
  | SetInstanceColors Bool
  | ShowClass (ClassId, Bool)

  | SetNms Float
  | SetMinThreshold Float
  | SetDetections Int
  | SetThreshold Float
  | SetLowerThreshold Float
  
  | SetPrefs Preferences
  | SetSort SortCommand

  | SetAutoDetect Bool

  | SetAssignMethod AssignmentMethod
  | SetTrainRatio Int

  | SetShowConfidence Bool
  | SetReviewing Bool
   

  deriving (Generic, Show)


data SortCommand
  = SetSortKey SortKey
  | SetImageSelection ImageSelection
  | SetReverseSelection Bool
  | SetReverse Bool
  | SetFilter FilterOption
  | SetNegFilter Bool
  | SetSearch Text
  deriving (Generic, Show)


data AppCommand
  = ViewCmd ViewCommand
  | EditCmd EditCmd
  | SelectCmd DocParts
  | ClearCmd

  | SubmitCmd SubmitType
  | OpenCmd DocName (Maybe SubmitType)

  | ConfigCmd ConfigUpdate

  | DialogCmd Dialog
  | ClassCmd (Set AnnotationId) ClassId
  | PrefCmd PrefCommand
  | TrainerCmd UserCommand

  | NavCmd Navigation 

  deriving (Generic, Show)


data SceneEvent
  = SceneEnter
  | SceneLeave
  | SceneDown
  | SceneClick
  | SceneDoubleClick

    deriving (Generic, Show, Eq)

data Shortcut a where
  ShortCancel :: Shortcut ()
  ShortUndo   :: Shortcut ()
  ShortRedo   :: Shortcut ()
  ShortDelete :: Shortcut ()
  ShortSelect :: Shortcut Bool
  ShortArea      :: Shortcut ()
  ShortSelectAll :: Shortcut ()
  ShortClass     :: Shortcut ()
  ShortSetClass     :: Shortcut Int

type Cursor = Text
type Image = (DocName, Dim)


data Action = Action
  { cursor      :: Last (Cursor, Bool)
  , edit        :: [Edit]
  , overlay     :: Render ()
  } deriving (Generic)


instance Default Action where
  def = Action
    { cursor  = mempty
    , edit    = mempty
    , overlay = pure ()
    }

instance Semigroup Action where
  (<>) (Action cursor edit overlay) (Action cursor' edit' overlay') = Action 
    { cursor  = cursor <> cursor'
    , edit    = edit <> edit'
    , overlay = overlay <> overlay'
    }

instance Monoid Action where
  mempty  = def
  mconcat actions = Action 
    { cursor  = mconcat $ view #cursor <$> actions
    , edit    = mconcat $ view #edit <$> actions
    , overlay = mconcat $ view #overlay <$> actions
    }


data AppEnv t = AppEnv
  { basePath :: Text
  , document :: (Dynamic t (Maybe Document))
  , editor :: (Dynamic t (Maybe Editor))
  , modified :: Dynamic t Bool

  , config :: (Dynamic t Config)
  , preferences :: (Dynamic t Preferences)
  , currentClass :: (Dynamic t ClassId)
  , docSelected  :: (Dynamic t (Maybe DocName))
  , shortcut     :: (EventSelector t Shortcut)
  , cancel       :: (Event t ())

  , selection    ::  (Dynamic t DocParts)
  , collection :: (Dynamic t Collection) 
  , loaded     :: Event t Document
  , detections :: Event t [Detection]
  , trainerStatus :: Dynamic t TrainerStatus

  , clock        :: Dynamic t UTCTime

  } deriving Generic



localPath :: MonadReader (AppEnv t) m => m (Text -> Text)
localPath = do
  base <- asks basePath
  return $ ((base <> "/") <>)


imagePath :: MonadReader (AppEnv t) m => m (Text -> Text)
imagePath = do 
  base <- asks basePath
  return $ ((base <> "/images/") <>)
  
  
newtype Shortcuts t = Shortcuts (forall a. Shortcut a -> Event t a)

askShortcuts :: (Reflex t, MonadReader (AppEnv t) m) => m (Shortcuts t)
askShortcuts = do
  selector <- view #shortcut
  return (Shortcuts (select selector))

askShortcut :: (Reflex t, MonadReader (AppEnv t) m) => Shortcut a -> m (Event t a)
askShortcut s = do
  (Shortcuts shortcut)  <- askShortcuts
  return $ shortcut s
  

askClasses :: AppBuilder t m => m (Dynamic t (Map ClassId ClassConfig))
askClasses = fmap (view #classes) <$> view #config


lookupClass :: AppBuilder t m => Dynamic t ClassId -> m (Dynamic t (Maybe ClassConfig))
lookupClass classId = do
  classes <- askClasses
  return $ Map.lookup <$> classId <*> classes


type CommandWriter t m = (EventWriter t [AppCommand] m, Reflex t)

docCommand :: CommandWriter t m => (a -> EditCmd) -> Event t a -> m ()
docCommand f = command (EditCmd . f)

viewCommand :: CommandWriter t m => Event t ViewCommand -> m ()
viewCommand = command ViewCmd

sortCommand :: CommandWriter t m => Event t SortCommand -> m ()
sortCommand = command (PrefCmd . SetSort)

prefCommand :: CommandWriter t m => Event t PrefCommand -> m ()
prefCommand = command PrefCmd

trainerCommand :: CommandWriter t m => Event t UserCommand -> m ()
trainerCommand = command TrainerCmd


editCommand :: CommandWriter t m => Event t Edit -> m ()
editCommand  = docCommand DocEdit


command :: CommandWriter t m => (a -> AppCommand) -> Event t a -> m ()
command f  = tellEvent . fmap (pure . f)

command' :: CommandWriter t m => AppCommand -> Event t a -> m ()
command' cmd = command (const cmd)


commandM :: CommandWriter t m => (a -> AppCommand) -> m (Event t a) -> m ()
commandM f m  = m >>= command f

commandM' :: CommandWriter t m => AppCommand -> m (Event t a) -> m ()
commandM' cmd = commandM (const cmd)

showText :: Show a => a -> Text
showText = T.pack . show

clearAnnotations :: EditCmd
clearAnnotations = DocEdit EditClearAll

printFloat :: Float -> Text
printFloat = T.pack . printf "%.2f"

printFloat0 :: Float -> Text
printFloat0 = T.pack . printf "%.0f"

makePrisms ''AppCommand
makePrisms ''SceneEvent

deriveGCompare ''Shortcut
deriveGEq ''Shortcut


