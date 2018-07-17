module Server.Options where

import Annotate.Prelude
import System.Console.CmdArgs.Implicit

default (Int, Float)

data Options = Options
  { database :: String
  , pipe     :: String
  , create   :: Maybe String
  , exportJson :: Maybe String
  , importJson :: Maybe String
  } deriving (Data, Typeable, Show)


options :: Options
options =
  Options { database   = def  &= argPos 1
          , create     = def  &= help "create a new database from images in folder"
          , pipe       = def  &= help "location of named pipe to communicate with trainer"
          , importJson     = def  &= help "import state from JSON file" &= name "import"
          , exportJson     = def  &= help "export state to JSON file"   &= name "export"
          } &= summary "Annotation server"


getArgs :: IO Options
getArgs = cmdArgs options
