module Server.Client where

import Server.Common

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Concurrent.STM
import GHC.Conc

import Control.Concurrent.Log

import Data.ByteString.Lazy (ByteString)
import qualified Network.WebSockets             as WS

import Server.Document
import Server.Store

nextClient :: Map ClientId Client -> ClientId
nextClient m = fromMaybe 0 (succ . fst . fst <$>  M.maxViewWithKey m)

userPreferences :: UserId -> Store -> Preferences
userPreferences k store = fromMaybe def $ preview (#preferences . ix k) store

sendHello :: Env -> ClientId -> UserId -> STM ()
sendHello env clientId userId = do
  store <- readLog (env ^. #store)
  sendClient env clientId (ServerHello clientId (userPreferences userId store) (store ^. #config))

connectClient :: Env -> WS.Connection ->  IO ClientId
connectClient env conn = do
  chan <- sendThread conn
  atomically $ do
    clientId <- nextClient <$> readTVar (env ^. #clients)

    modifyTVar (env ^. #clients) (M.insert clientId (Client chan Nothing 0))
    writeLog env $ "connected: " <> show clientId
    return clientId


broadcastConfig :: Env -> STM ()
broadcastConfig env = do
  config <- view #config <$> readLog (env ^. #store)
  broadcast env (ServerConfig config)

clientDisconnected :: Env -> ClientId -> IO ()
clientDisconnected env clientId = atomically $ do
  withClient (env ^. #clients) clientId $ \Client {..} -> do
    writeTChan connection Nothing

    closeDocument env clientId
    time <- getCurrentTime'
    broadcast env (ServerOpen Nothing clientId time)

  modifyTVar (view #clients env) (M.delete clientId)
  writeLog env $ "disconnected: " <> show clientId


clientOpen :: Env -> ClientId -> NavId -> DocName -> STM ()
clientOpen env clientId navId k = do
  mDoc <- lookupDoc k <$> readLog (env ^. #store)
  case mDoc of
    Nothing  -> sendClient env clientId $
      ServerError (ErrNotFound navId k)
    Just doc -> do
      openDocument env clientId k
      sendClient env clientId (ServerDocument navId doc)


clientLoop :: Env -> WS.Connection -> ClientId -> UserId -> IO ()
clientLoop env conn clientId userId = do
  atomically $ sendHello env clientId userId
  forever $ do
    str <- WS.receiveData conn
    atomically $ case eitherDecode str of
      Left err  -> do
        writeLog env (show clientId <> " <- error decoding " <> show str <> ", " <> show err)
        sendClient env clientId (ServerError (ErrDecode (fromString err)))

      Right msg -> do
        writeLog env (show clientId <> " <- " <> show msg)
        processMsg env clientId userId msg

processMsg :: Env -> ClientId -> UserId -> ClientMsg -> STM ()
processMsg env@Env{store} clientId userId msg = do
  time <- getCurrentTime'
  case msg of
    ClientNav navId nav -> case nav of
      NavTo k  -> clientOpen env clientId navId k
      NavNext  -> do
        prefs <- userPreferences userId <$> readLog store
        clientOpenNext env clientId (prefs ^. #ordering) navId

    ClientSubmit doc -> void $ do
      updateLog store (CmdSubmit doc time)
      sendTrainer env (TrainerUpdate (doc ^. #name) (Just (exportImage doc)))

    ClientDetect k -> do
      prefs <- userPreferences userId <$> readLog store
      running <- sendTrainer env (TrainerDetect (Just clientId) k (prefs ^. #detection))
      unless running $
        sendClient env clientId (ServerError ErrNotRunning)

    ClientPreferences preferences ->
      updateLog store (CmdPreferences 0 preferences)

    ClientConfig (ConfigClass k mClass) -> do
      updateLog store (CmdClass k mClass)
      broadcastConfig env

    ClientCollection -> do
      collection <- getCollection <$> readLog (env ^. #store)
      sendClient env clientId (ServerCollection collection)


clientOpenNext :: Env -> ClientId -> ImageOrdering -> NavId -> STM ()
clientOpenNext env clientId ordering navId =
  withClient_ (env ^. #clients) clientId $ \Client {document} -> do
    maybeDoc <- findNext env ordering document
    case maybeDoc of
      Just k    -> clientOpen env clientId navId k
      Nothing   -> sendClient env clientId $
        ServerError (ErrEnd navId)



-- Web clients connect to this server
clientServer :: Env -> WS.ServerApp
clientServer env pending = do
  conn <- WS.acceptRequest pending
  clientId <- connectClient env conn

  WS.forkPingThread conn 30
  finally
    (clientLoop env conn clientId 0)
    (clientDisconnected env clientId)
