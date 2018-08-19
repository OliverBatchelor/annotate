module Main where


import GHC.Conc
import System.IO
import System.Directory

import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as Text

import qualified Data.ByteString.Lazy                      as BS
-- import Data.ByteString.Lazy (ByteString)

import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as Static


import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS

import Servant
import Servant.Utils.StaticFiles

import Annotate.Common
import qualified Annotate.Common as Ann

import qualified Server.Options as Opt

import Server.Store
import Server.Client
import Server.Trainer

import Server.Document
import Server.Common

import Image


-- Initialisation and servant server configuration
type Api =
  "clients" :> Raw
  :<|> "images" :> Raw
  :<|> Raw


server :: FilePath -> Env -> Server Api
server root env =
  withDefault (clientServer env)
    :<|> serveDirectoryWebApp root
    :<|> serveDirectoryWebApp "html"


withDefault :: WS.ServerApp -> Server Raw
withDefault ws = Tagged $ WS.websocketsOr WS.defaultConnectionOptions ws backupApp
  where backupApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a WebSocket request"


startLogger :: Handle -> IO (TChan LogMsg)
startLogger handle = do
  logChan   <- atomically newTChan

  forkIO $ forever $ do
    msg <- atomically $ readTChan logChan
    hPutStrLn handle msg

  return logChan


readRoot :: Log Store -> IO FilePath
readRoot store = Text.unpack . view (#config . #root) <$> atomically (readLog store)


main :: IO ()
main = do
  Opt.Options {..} <- Opt.getArgs
  logChan <- startLogger stdout

  create' <- forM create $ \root -> do
    createDirectoryIfMissing True root
    return (initialStore (def & #root .~ (fromString root)))

  import' <- forM importJson $ \file ->
    BS.readFile file >>= fmap importCollection . tryDecode

  store <- case create' <|> import' of
    Just initial -> freshLog initial database
    Nothing      -> openLog database >>= either (throw . LogError) return

  root <- readRoot store

  clients   <- atomically (newTVar M.empty)
  documents <- atomically (newTVar M.empty)

  trainer <- atomically $ newTVar Nothing

  atomically $ do
    config <- view #config <$> readLog store
    existing <- M.keysSet . view #images <$> readLog store
    images <- unsafeIOToSTM (findNewImages config root existing)
    updateLog store (CmdImages images)

  let env = Env {..}

  forM_ exportJson $ \file -> do
    state <- atomically $ do
      writeLog env $ "Exporting store to: " <> file
      readLog store

    BS.writeFile file (encodePretty (exportCollection state))

  let port' = fromMaybe 3000 port
  atomically $ writeLog env ("Anotate server listening on port " <> show port')

  forkIO $ WS.runServer "127.0.0.1" 2160 $ trainerServer env
  Warp.run port' $ serve (Proxy @ Api) (server root env)
