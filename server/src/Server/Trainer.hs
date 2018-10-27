module Server.Trainer where

import Server.Common
import Server.Store (exportCollection)
-- import Server.Client (detectNext)
import Server.Document

import qualified Data.Map as M

import Control.Concurrent.STM
import Control.Concurrent.Log

import qualified Network.WebSockets             as WS


connectTrainer :: Env -> WS.Connection -> IO ()
connectTrainer env conn = do
  chan <- sendThread conn

  atomically $ do
    closeTrainer env
    writeLog env "trainer connected"
    writeTVar (env ^. #trainer) (Just $ Trainer chan)

closeTrainer :: Env -> STM ()
closeTrainer env = do
  sendTrainer' env Nothing
  writeTVar (env ^. #trainer) Nothing

trainerState :: Env -> STM TrainerState
trainerState Env{store} = view #trainer <$> readLog store

lookupKey :: Eq k => [(k, a)] -> k -> Maybe (k, a)
lookupKey xs k = (k,) <$> lookup k xs

trainerLoop :: Env -> WS.Connection ->  IO ()
trainerLoop env@Env{store} conn = do
  atomically $ do
    dataset <- exportCollection <$> readLog store
    sendTrainer env (TrainerInit dataset)

  runLoop

    where
      runLoop = forever $ do
        str <- WS.receiveData conn
        atomically $ case (eitherDecode str) of
          Left err  -> do
            writeLog env ("trainer <- error decoding " <> show str <> ", " <> show err)
          Right msg -> do
            writeLog env ("trainer <- " <> show msg)
            processMsg msg

      processMsg = \case
        TrainerDetections req k detections netId -> do
          updateLog store $ CmdDetections [(k, detections)] netId
          case req of
            DetectClient clientId ->
                sendClient' env clientId (ServerDetection k detections)
            DetectLoad navId clientId -> withDocument env k $ \doc ->
                sendClient' env clientId (ServerDocument navId doc)
            DetectPre -> return()

        TrainerReqError req k err ->
          case req of
            DetectClient clientId -> sendClient' env clientId (ServerError (ErrTrainer err))
            DetectLoad navId clientId -> withDocument env k $ \doc ->
                sendClient' env clientId (ServerDocument navId doc)
            DetectPre -> return()

        TrainerError err ->
          writeLog env ("trainer error: " <> show err)

        TrainerCheckpoint (run, epoch) score best ->
          updateLog store $ CmdCheckpoint (run, epoch) score best
          -- withClientEnvs env detectNext



trainerServer :: Env -> WS.ServerApp
trainerServer env pending = do
  conn <- WS.acceptRequest pending
  connectTrainer env conn

  WS.forkPingThread conn 30
  finally
    (trainerLoop env conn)
    (atomically $ do
      writeTVar (env ^. #trainer) Nothing
      writeLog env ("trainer closed"))
