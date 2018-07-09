module Server.Client where

import Server.Common
import Annotate.Document

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Concurrent.STM
import GHC.Conc

import Control.Concurrent.Log

import Data.ByteString.Lazy (ByteString)
import qualified Network.WebSockets             as WS

import Server.Document

nextClient :: Map ClientId Client -> ClientId
nextClient m = fromMaybe 0 (succ . fst . fst <$>  M.maxViewWithKey m)

sendHello :: Env -> ClientId -> STM ()
sendHello env clientId = sendClient env clientId (ServerHello clientId)

connectClient :: Env -> WS.Connection ->  IO ClientId
connectClient env conn = do
  chan <- sendThread conn
  atomically $ do
    clientId <- nextClient <$> readTVar (env ^. #clients)
    modifyTVar (env ^. #clients) (M.insert clientId (Client chan Nothing))
    writeLog env $ "connected: " <> show clientId
    return clientId


clientDisconnected :: Env -> ClientId -> IO ()
clientDisconnected env clientId = atomically $ do
  withClient (env ^. #clients) clientId $ \Client {..} -> do
    writeTChan connection Nothing

    closeDocument env clientId
    time <- getCurrentTime'
    broadcast env (ServerOpen Nothing clientId time)

  modifyTVar (view #clients env) (M.delete clientId)
  writeLog env $ "disconnected: " <> show clientId


clientOpen :: Env -> ClientId -> DocName -> STM ()
clientOpen env clientId k = do
  (mInfo, mDoc) <- lookupDoc k <$> readLog (env ^. #store)
  for_ mInfo $ \info -> do
    openDocument env clientId k
    sendClient env clientId (ServerDocument k info (fromMaybe emptyDoc mDoc))


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
    ClientOpen k    -> clientOpen env clientId k
    ClientCmd k cmd -> modifyDocument env k cmd

    ClientSubmit k cat doc -> do
      updateLog store (CmdSubmit k doc time)
      updateLog store (CmdCategory k cat)
      nextImage env clientId (Just k)

    ClientDiscard k -> do
      updateLog store (CmdCategory k Discard)
      nextImage env clientId (Just k)

    ClientNext current -> nextImage env clientId current

    ClientDetect k -> do
      running <- sendTrainer env (TrainerDetect clientId k)
      unless running $
        sendClient env clientId (ServerError ErrNotRunning)


nextImage :: Env -> ClientId -> Maybe DocName -> STM ()
nextImage env clientId current = do
  maybeDoc <- findNext env current
  case maybeDoc of
    Just k -> clientOpen env clientId k
    Nothing      -> sendClient env clientId ServerEnd



-- Web clients connect to this server
clientServer :: Env -> WS.ServerApp
clientServer env pending = do
  conn <- WS.acceptRequest pending
  clientId <- connectClient env conn

  WS.forkPingThread conn 30
  finally
    (clientLoop env conn clientId)
    (clientDisconnected env clientId)