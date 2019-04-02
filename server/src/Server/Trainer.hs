module Server.Trainer where

import Server.Common
import Server.Export (exportImage, updateImage)
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
    writeTVar (env ^. #trainer) (Just $ Trainer chan StatusPaused)
    sendTrainerStatus env


closeTrainer :: Env -> STM ()
closeTrainer env@Env{trainer} = do
  open <- isJust <$> readTVar trainer
  when open $ do
    sendTrainer' env Nothing
    writeTVar trainer Nothing
    sendTrainerStatus env


trainerState :: Env -> STM TrainerState
trainerState Env{store} = view #trainer <$> readLog store


lookupKey :: Eq k => [(k, a)] -> k -> Maybe (k, a)
lookupKey xs k = (k,) <$> lookup k xs

isUsed :: Document -> Bool
isUsed doc = category /= CatDiscard
  where category = doc ^. #info . #category

 
sendTrainerStatus :: Env -> STM ()
sendTrainerStatus env = do 
  status <- trainerStatus env
  broadcast env (ServerStatus status)

updateDetections :: Env -> Map DocName Detections -> STM ()
updateDetections env@Env{store} m = do 
  updateLog store $ CmdDetections m
  docs <- lookupDocuments env (M.keys m)
  broadcast env $
    ServerUpdateDetections (M.mapMaybe (view (#info . #detections)) docs)

logTrainerMsg :: FromTrainer -> Bool
logTrainerMsg (TrainerProgress _) = False
logTrainerMsg _ = True

trainerLoop :: Env -> WS.Connection ->  IO ()
trainerLoop env@Env{store} conn = do
  atomically $ do
    store <- readLog store
    sendTrainer env (TrainerInit (store ^. #config))

    for_ (store ^. #images) $ \doc -> 
      void $ sendTrainer env (TrainerImport (doc ^. #name) (updateImage doc))

  runLoop

    where
      runLoop = forever $ do 
        str <- WS.receiveData conn
        atomically $ case (eitherDecode str) of
          Left err  -> do
            writeLog env ("trainer <- error decoding " <> unpackBS str <> ", " <> err)
          Right msg -> do
            when (logTrainerMsg msg) $
              writeLog env ("trainer <- " <> truncate (show msg))

            processMsg msg

      processMsg :: FromTrainer -> STM ()
      processMsg = \case
        TrainerDetectRequest req k detections -> do
          updateDetections env $ M.singleton k detections 

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
          updateLog store $ CmdCheckpoint $ Checkpoint (run, epoch) score best
          -- withClientEnvs env detectNext

        TrainerDetections m -> updateDetections env m          

        TrainerTraining summary -> do
          updateLog store $ CmdTraining summary
          docs <- lookupDocuments env (M.keys summary)

          broadcast env $
            ServerUpdateTraining (view (#info . #training) <$> docs)

        TrainerProgress progress -> do

          modifyTVar (env ^. #trainer) (traverse . #status .~ maybe StatusPaused StatusTraining progress)
          sendTrainerStatus env
          


trainerServer :: Env -> WS.ServerApp
trainerServer env pending = do
  conn <- WS.acceptRequest pending
  connectTrainer env conn

  WS.forkPingThread conn 30
  finally
    (trainerLoop env conn)
    (atomically $ do
      closeTrainer env
      writeLog env ("trainer closed"))
