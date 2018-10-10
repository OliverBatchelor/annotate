module Server.Document where

import Server.Common
import Server.Store

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Concurrent.STM
import Control.Concurrent.Log

import Control.Lens


minSet :: Ord a => Set a -> Maybe a
minSet s
  | null s        = Nothing
  | otherwise     = Just (S.elemAt 0 s)

nextCircular :: Ord a => Set a -> a -> Maybe a
nextCircular s x = S.lookupGT x s' <|> minSet s'
  where s' = S.delete x s

nextSet :: Ord k => [k] -> Maybe k -> Maybe k
nextSet items = (\case
    Nothing      -> minSet s
    Just current -> nextCircular s current)
      where s = S.fromList items


findNext' :: ImageOrdering -> Map DocName Document ->  Map DocName [ClientId] -> Maybe DocName -> Maybe DocName
findNext' ordering images openDocs current = case ordering of
    OrderSequential -> unNatural <$> nextSet sorted (makeNaturalKey <$> current)
    OrderMixed      -> unKey <$> nextSet mixed (hashKey <$> current)

  where
    editable = filter isFresh (M.elems images)
    sorted = view (#info . #naturalKey) <$> editable
    mixed = getHash <$> editable

    getHash Document{info, name} = HashedKey name (unHash $ view #hashedName info) 

    isFresh Document{info, name} =
        info ^. #category == New &&
        not (M.member name openDocs)


findNext :: Env -> ImageOrdering -> Maybe DocName -> STM (Maybe DocName)
findNext Env{..} ordering maybeCurrent =
  findNext' ordering <$> (view #images <$> readLog store) <*> readTVar documents <*> pure maybeCurrent


openDocument :: Env -> ClientId -> DocName -> STM ()
openDocument env@Env {..} clientId k = do
  closeDocument env clientId

  writeLog env ("opening " <> show clientId <> ", " <> show k)

  modifyTVar clients (ix clientId . #document .~ Just k)
  modifyTVar documents (M.alter addClient k)

  time <- getCurrentTime'
  broadcast env (ServerOpen (Just k) clientId time)

    where
      addClient = \case
        Just cs -> Just $ ordNub (clientId:cs)
        Nothing -> Just [clientId]

clientDocument :: ClientId -> Traversal' (Map ClientId Client) DocName
clientDocument clientId = ix clientId . #document . traverse


closeDocument :: Env -> ClientId -> STM ()
closeDocument env@Env {..} clientId  = preview (clientDocument clientId) <$> readTVar clients >>= traverse_ withDoc
  where
    withDoc k = do
        writeLog env ("closing " <> show clientId <> ", " <> show k)

        refs <- M.lookup k <$> readTVar documents
        modifyTVar documents (M.update removeClient k)

        mDoc <- lookupDoc k <$> readLog store
        forM_ mDoc $ broadcast env . ServerUpdateInfo k . view #info

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
