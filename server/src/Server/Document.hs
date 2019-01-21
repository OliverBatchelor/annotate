module Server.Document where

import Server.Common
import Server.Store

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Concurrent.STM
import Control.Concurrent.Log

import Control.Lens

import Data.List (splitAt, elemIndex, takeWhile, dropWhile)


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
      

rotate :: Int -> [a] -> [a]
rotate n xs = bs <> as 
  where (as, bs) = splitAt n xs


rotateFrom :: Eq a => Maybe a -> [a] -> [a]
rotateFrom current ks = fromMaybe ks $ do
  k <- current
  i <- elemIndex k ks
  return (drop 1 (rotate i ks))


selectionKey :: ImageSelection -> (Bool, SortKey)
selectionKey = \case 
    SelSequential rev -> (not rev, SortName)
    SelRandom         -> (True, SortRandom)
    SelDetections rev -> (not rev, SortDetections)
    SelLoss           -> (True, SortLossMax)

selectingMax :: ImageSelection -> Bool
selectingMax = \case
  SelRandom       -> False
  SelSequential _ -> False
  _             -> True

sortSelection :: SortOptions -> [(DocName, DocInfo)] -> [(DocName, DocInfo)]
sortSelection SortOptions{..} = sortImagesBy (negFilter, filtering) search (selectionKey selection)
  

sortAll :: SortOptions ->  Map DocName Document  -> [DocName]
sortAll sortOptions = fmap fst . sortSelection sortOptions . M.toList . M.map (view #info)
         

findNext :: Env -> SortOptions -> Maybe DocName ->  STM [DocName]
findNext Env{..} sortOptions current = do
  images <- view #images <$> readLog store
  openDocs  <- readTVar documents

  return $ filter (inUse openDocs) $ nextFrom (sortAll sortOptions images)
    where
      nextFrom = if selectingMax (sortOptions ^. #selection) 
          then dropWhile ((== current) . Just) else rotateFrom current

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
