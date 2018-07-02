module Stream where

import Annotate.Common
import Types

import Pipes
import qualified Pipes as P

import Pipes.Aeson (DecodingError)
import Pipes.Aeson.Unchecked (decoded)
import Pipes.ByteString (fromHandle)

import Control.Concurrent
import Control.Concurrent.STM

import System.Posix.Files
import System.FilePath

import Data.ByteString.Lazy as L
import System.IO

runWriter :: ToJSON a => TChan (Maybe a) -> Handle -> IO ThreadId
runWriter chan handle = forkIO run where
  run = do
    ma <- atomically $ readTChan chan
    traverse_ (\a -> L.hPut handle (encode a) >> run) ma

runReader :: (ToJSON a, FromJSON a) => (DecodingError -> a) -> TChan a -> Handle -> IO ThreadId
runReader fromError chan handle = forkIO $ do
  r <- runEffect $
    P.for (fromHandle handle ^. decoded) (liftIO . send)

  case r of
    Left (err, _) -> send (fromError err)
    _ -> return ()

  where
    send = atomically . writeTChan chan


createFifoPair :: FilePath -> IO ()
createFifoPair path = do
  createFifo (path <.> "in")
  createFifo (path <.> "out")

createFifo :: FilePath -> IO ()
createFifo path = do
  exists <- fileExist path
  if exists
    then checkPermission path >> assertPipe path
    else createNamedPipe path ownerModes


assertPipe :: FilePath -> IO ()
assertPipe path = do
  status <- getFileStatus path
  unless (isNamedPipe status) $
    throw $ FileError ("file exists but is not named pipe: " <> path)

checkPermission :: FilePath -> IO ()
checkPermission path = do
  access <- fileAccess path True True False
  unless access $
    throw $ FileError ("do not have read-write permission on: " <> path)
