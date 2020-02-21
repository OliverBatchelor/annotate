module Server.Client where

import Server.Common

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Concurrent.STM
import GHC.Conc

import Control.Concurrent.Log

import Data.ByteString.Lazy (ByteString)

import qualified Data.ByteString.Lazy  as BS
import qualified Network.WebSockets             as WS

import Server.Document
import Server.Store
import Server.Export

nextClient :: Map ClientId Client -> ClientId
nextClient m = fromMaybe 0 (succ . fst . fst <$>  M.maxViewWithKey m)

userPreferences :: UserId -> Store -> Preferences
userPreferences k store = fromMaybe def $ preview (#preferences . ix k) store

sendHello :: ClientEnv ->  STM ()
sendHello env@ClientEnv{store, clientId, userId} = do
  
  status <- trainerStatus (upcast env)
  store <- readLog store

  sendClient env (ServerHello clientId (userPreferences userId store) (store ^. #config) status)

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

clientDisconnected :: ClientEnv -> IO ()
clientDisconnected env@ClientEnv{clientId} = atomically $ do
  withClient env $ \Client {..} -> do
    writeTChan connection Nothing

    closeDocument env
    time <- getCurrentTime'
    broadcast (upcast env) (ServerOpen Nothing clientId time)

  modifyTVar (view #clients env) (M.delete clientId)
  writeLog (upcast env) $ "disconnected: " <> show clientId



clientLoop :: ClientEnv ->  WS.Connection -> IO ()
clientLoop env conn = do
  atomically $ sendHello env
  forever $ do
    str <- WS.receiveData conn
    atomically $ case eitherDecode str of
      Left err  -> do
        clientLog env (" <- error decoding " <> unpackBS str <> ", " <> err)
        sendClient env (ServerError (ErrDecode (fromString err)))

      Right msg -> do
        clientLog env (" <- " <> truncate (show msg))
        processMsg env msg
  
broadcastSubmission :: Env -> DocName -> SubmitType -> STM ()
broadcastSubmission env k method = do
  mDoc <- lookupDocument (upcast env) k       
  for_ mDoc $ \doc -> do
    sendTrainer (upcast env)  (TrainerUpdate k method  (Just (updateTrainer doc)))
    broadcastInfo (upcast env) k (doc ^. #info)

processMsg :: ClientEnv -> ClientMsg -> STM ()
processMsg env@ClientEnv{store, clientId, userId} msg = do
  time <- getCurrentTime'
  case msg of
    ClientNav navId nav -> do
      case nav of
        NavTo k  -> navTo   env navId k
        NavNext  -> navNext env navId

    ClientSubmit submission -> void $ do
      mDoc <- lookupDocument (upcast env) (submission ^. #name)   
      for_ mDoc $ \doc -> case checkSubmission submission doc of 
        Just err -> sendClient env (ServerError err)
        Nothing -> do
          updateLog store (CmdSubmit userId submission time)
          broadcastSubmission (upcast env) (submission ^. #name) (submission ^. #method)
      

    ClientDetect k review -> do
      prefs <- userPreferences userId <$> readLog store
      running <- detectRequest env (DetectClient clientId) k review
      unless running $
        sendClient env (ServerError ErrNotRunning)

    ClientPreferences preferences ->
      updateLog store (CmdPreferences userId preferences)


    ClientConfig (ConfigClass k mClass) -> do
      updateLog store (CmdClass k mClass)
      broadcastConfig (upcast env)

    ClientCollection -> do
      collection <- getCollection <$> readLog store
      sendClient env (ServerCollection collection)

    ClientCommand cmd -> void $ 
      sendTrainer (upcast env) (UserCommand cmd)


bestNetwork :: Env -> STM NetworkId
bestNetwork Env{store} = bestModel . view #trainer <$> readLog store

currentNetwork :: Env -> STM NetworkId
currentNetwork Env{store} = currentModel . view #trainer <$> readLog store

detectRequest :: ClientEnv -> DetectRequest -> DocName -> BasicAnnotationMap -> STM Bool
detectRequest env@ClientEnv{userId, store} req k review = do
  prefs <- userPreferences userId <$> readLog store
  sendTrainer (upcast env) (TrainerDetect req k review (prefs ^. #detection))


navTo :: ClientEnv -> NavId -> DocName -> STM ()
navTo env navId k = do
  net <- currentNetwork (upcast env)

  lookupDocument (upcast env) k >>= \case 
    Nothing  -> sendClient env (ServerError (ErrNotFound navId k))
    Just doc -> do
      openDocument env k      
      hasTrainer <- trainerConnected env

      -- unsafeIOToSTM $ print (net, latestDetect doc)
      -- if latestDetect doc /= Just net && hasTrainer
      if hasTrainer
        then void $ detectRequest env (DetectLoad navId (env ^. #clientId)) k (reviewAnnotations doc)
        else sendClient env (ServerDocument navId doc)

reviewAnnotations :: Document -> BasicAnnotationMap
reviewAnnotations doc = case doc ^. #info . #category of
  CatNew      -> mempty
  CatDiscard  -> mempty
  _ -> doc ^. #annotations

latestDetect :: Document -> Maybe NetworkId
latestDetect = preview (#detections . traverse . #stats . traverse . #networkId)

navNext :: ClientEnv -> NavId -> STM ()
navNext env navId = do
  next <- clientDoc env >>= nextFrom env
  case next of
    []      ->  sendClient env $ ServerError (ErrEnd navId)
    (k : _) -> navTo env navId k

clientDoc :: ClientEnv -> STM (Maybe DocName)
clientDoc env = join <$> withClient env (return . view #document)

nextFrom :: ClientEnv -> Maybe DocName -> STM [DocName]
nextFrom env k = do
  prefs <- userPreferences (env ^. #userId) <$> readLog (env ^. #store)
  findNext (upcast env) (prefs ^. #sortOptions) k


-- Web clients connect to this server
clientServer :: Env -> WS.ServerApp
clientServer env pending = do
  conn <- WS.acceptRequest pending
  clientId <- connectClient env conn

  let clEnv = clientEnv env clientId 0

  WS.forkPingThread conn 30
  finally
    (clientLoop clEnv conn)
    (clientDisconnected clEnv)
