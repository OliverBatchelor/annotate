module Server.Trainer where

import Server.Common
import Server.Store (exportCollection)

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
        TrainerDetections dest k detections netId ->
          case dest of
            Just clientId -> sendClient env clientId (ServerDetection k detections)
            Nothing       -> return () -- TODO, store detections

        TrainerReqError clientId err ->
          sendClient env clientId (ServerError (ErrTrainer err))

        TrainerError err ->
          writeLog env ("trainer error: " <> show err)

        TrainerCheckpoint (run, epoch) score best ->
          updateLog store $ CmdCheckpoint (run, epoch) score best




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
