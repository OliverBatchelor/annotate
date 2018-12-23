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

lookupNext :: Ord a => Set a -> a -> Maybe a
lookupNext s x = S.lookupGT x s <|> minSet s

maybeNext :: Ord a => Set a -> Maybe a -> Maybe a
maybeNext s = \case
    Nothing -> minSet s
    Just x  -> lookupNext (S.delete x s) x


nextSet :: Ord a => Set a -> Maybe a -> [a]
nextSet s = \case
    Nothing -> S.toList s
    Just x  -> S.toList ys <> S.toList xs
      where (xs, _, ys) = S.splitMember x s

prevSet :: Ord a => Set a -> Maybe a -> [a]
prevSet s = reverse . nextSet s
      


findNext' :: ImageOrdering ->  Map DocName Document ->  Map DocName [ClientId] -> Maybe DocName -> [DocName]
findNext' ordering images openDocs current = case ordering of
    OrderSequential -> unNatural <$>  nextSet sorted (makeNaturalKey <$> current)
    OrderBackwards  -> unNatural <$>  prevSet sorted (makeNaturalKey <$> current)
    OrderMixed      -> unKey <$> nextSet mixed (hashKey <$> current)

  where

    editable = filter isFresh (M.elems images)
    sorted = S.fromList (view (#info . #naturalKey) <$> editable)
    mixed = S.fromList (getHash <$> editable)

    getHash Document{info, name} = HashedKey name (unHash $ view #hashedName info)

    isFresh Document{info, name} =
        info ^. #category == New &&
        not (M.member name openDocs)


findNext :: Env -> ImageOrdering -> Maybe DocName ->  STM [DocName]
findNext Env{..} ordering maybeCurrent =
  findNext' ordering <$> (view #images <$> readLog store) <*> readTVar documents <*> pure maybeCurrent


withDocument :: Env -> DocName -> (Document -> STM ()) -> STM ()
withDocument env k f = do
  store <- readLog (env ^. #store)
  traverse_ f (lookupDoc k store)


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


closeDocument :: ClientEnv -> STM ()
closeDocument env@ClientEnv{..}  = preview (clientDocument clientId) <$> readTVar clients >>= traverse_ withDoc
  where
    withDoc k = do
        writeLog (upcast env) ("closing " <> show clientId <> ", " <> show k)

        refs <- M.lookup k <$> readTVar documents
        modifyTVar documents (M.update removeClient k)

        mDoc <- lookupDoc k <$> readLog store
        forM_ mDoc $ broadcast (upcast env) . ServerUpdateInfo k . view #info

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
