module Server.Document where

import Server.Common
import Server.Store

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Concurrent.STM
import Control.Concurrent.Log

import Control.Lens
import Annotate.Sorting

findNext :: Env -> SortOptions -> Maybe DocName ->  STM [DocName]
findNext Env{..} sortOptions current = do
  images <- view #images <$> readLog store
  openDocs  <- readTVar documents

  return $ filter (inUse openDocs) $ sorting (M.toList (view #info <$> images))
    where
      sorting = fmap fst . nextImages sortOptions current
      inUse openDocs k = not (M.member k openDocs)

withDocument :: Env -> DocName -> (Document -> STM ()) -> STM ()
withDocument env k f = lookupDocument env k >>= traverse_ f


openDocument :: ClientEnv -> DocName -> STM ()
openDocument env@ClientEnv {..} k = do
  closeDocument env

  writeLog (upcast env) ("opening " <> show clientId <> ", " <> show k)

  modifyTVar clients (ix clientId . #document .~ Just k)
  modifyTVar documents (M.alter addClient k)

  time <- getCurrentTime'
  broadcast (upcast env) (ServerOpen (Just k) clientId time)

    where
      addClient = \case
        Just cs -> Just $ ordNub (clientId:cs)
        Nothing -> Just [clientId]

clientDocument :: ClientId -> Traversal' (Map ClientId Client) DocName
clientDocument clientId = ix clientId . #document . traverse


broadcastUpdate :: Env -> DocName -> STM ()
broadcastUpdate env k = withDocument env k $ 
  broadcastInfo env k . view #info

broadcastInfo :: Env -> DocName -> DocInfo -> STM ()
broadcastInfo env k = broadcast env . ServerUpdateInfo k 

closeDocument :: ClientEnv -> STM ()
closeDocument env@ClientEnv{..}  = preview (clientDocument clientId) <$> readTVar clients >>= traverse_ withDoc
  where
    withDoc k = do
        writeLog (upcast env) ("closing " <> show clientId <> ", " <> show k)

        refs <- M.lookup k <$> readTVar documents
        modifyTVar documents (M.update removeClient k)


    removeClient cs = case filter (/= clientId) cs of
        []  -> Nothing
        cs' -> Just cs'


ordNub = S.toList . S.fromList


-- modifyDocument :: Env -> DocName -> EditCmd -> STM ()
-- modifyDocument env@Env {..} k cmd = do
--
--   time <- getCurrentTime'
--   updateLog store (CmdDoc k cmd time)
--
--   clients <- getEditing <$> readTVar documents
--   for_ clients $ \clientId ->
--     sendClient env clientId (ServerCmd k cmd)
--
--   where
--     getEditing = fromMaybe [] . M.lookup k
