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

sendHello :: Env -> ClientId -> STM ()
sendHello env clientId = do
  config <- view #config <$> readLog (env ^. #store)
  sendClient env clientId (ServerHello clientId config)

connectClient :: Env -> WS.Connection ->  IO ClientId
connectClient env conn = do
  chan <- sendThread conn
  atomically $ do
    clientId <- nextClient <$> readTVar (env ^. #clients)
    modifyTVar (env ^. #clients) (M.insert clientId (Client chan Nothing))
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


clientLoop :: Env -> WS.Connection -> ClientId -> IO ()
clientLoop env conn clientId = do
  atomically $ sendHello env clientId
  forever $ do
    str <- WS.receiveData conn
    atomically $ case eitherDecode str of
      Left err  -> do
        writeLog env (show clientId <> " <- error decoding " <> show str <> ", " <> show err)
        sendClient env clientId (ServerError (ErrDecode (fromString err)))

      Right msg -> do
        writeLog env (show clientId <> " <- " <> show msg)
        processMsg env clientId msg

processMsg :: Env -> ClientId -> ClientMsg -> STM ()
processMsg env@Env{store} clientId msg = do
  time <- getCurrentTime'
  case msg of
    ClientNav navId nav -> case nav of
      (NavTo k)          -> clientOpen env clientId navId k
      (NavNext ordering) -> clientOpenNext env clientId navId ordering

    ClientSubmit doc -> void $ do
      updateLog store (CmdSubmit doc time)
      sendTrainer env (TrainerUpdate (doc ^. #name) (Just (exportImage doc)))



    ClientDetect k params -> do
      running <- sendTrainer env (TrainerDetect clientId k params)
      unless running $
        sendClient env clientId (ServerError ErrNotRunning)

    ClientConfig (ConfigClass k mClass) -> do
      updateLog store (CmdClass k mClass)
      broadcastConfig env

    ClientCollection -> do
      collection <- getCollection <$> readLog (env ^. #store)
      sendClient env clientId (ServerCollection collection)




clientOpenNext :: Env -> ClientId -> NavId -> ImageOrdering -> STM ()
clientOpenNext env clientId navId ordering =
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
    (clientLoop env conn clientId)
    (clientDisconnected env clientId)
