module Image (imageInfo, findNewImages, findImages) where

import Prelude (lines)
import Annotate.Prelude
import Annotate.Common

import qualified Data.Map as M
import qualified Data.Set as S

import System.Process (readProcessWithExitCode)

import qualified Data.ByteString.Lazy.Char8 as BS
--import Control.Monad.Combinators

import qualified Data.Text as Text
import System.FilePath
import System.Directory
import System.Posix.Files

import Data.Time
import Data.Time.Format

import Text.Read (readMaybe)

import Server.Common (defaultInfo)


getExif :: FilePath -> IO (Map String String)
getExif path = do
  (exit, out, _) <- readProcessWithExitCode "exiftool" ["-J", path] ""
  return $ fromMaybe mempty $ decode' (BS.pack out)


getImageInfo :: TimeZone -> Map String String -> Maybe (ImageInfo)
getImageInfo timeZone exif = do
  w <- prop "ImageWidth" >>= readMaybe
  h <- prop "ImageHeight" >>= readMaybe
  return $ ImageInfo (w, h) (prop "DateTimeOriginal" >>= toDate)
  where
    prop k = M.lookup k exif

    toDate :: String -> Maybe UTCTime
    toDate str = do 
      local <- parseTimeM True defaultTimeLocale "%Y:%m:%d %H:%M:%S" str
      return $ localTimeToUTC timeZone local

imageInfo :: FilePath -> DocName -> IO (Maybe ImageInfo)
imageInfo root filename = do
  timeZone <- getCurrentTimeZone
  properties <- getExif path
  return $ getImageInfo timeZone properties
      where
        path = root </> Text.unpack filename


validExtension :: [String] -> FilePath -> Bool
validExtension exts filename = any (\e -> fmap toLower e == ext) exts where
  ext = fmap toLower (takeExtension filename)


findImages' :: Config -> FilePath -> IO [FilePath]
findImages' config root = do
  contents <- fmap fromString <$> listDirectory root

  files <- filterM (fmap isRegularFile . getFileStatus . toAbs) contents
  directories <- filterM (fmap isDirectory . getFileStatus . toAbs) contents 
  
  subDirs <- for directories $ \d -> do
      images <- findImages' config (root </> d)
      return (fmap (d </>) images)
  
  return (mconcat (filter (validExtension exts) files : subDirs))
    where exts = Text.unpack <$> config ^. #extensions
          toAbs = (root </>)

findImages :: Config -> FilePath -> IO [DocName]
findImages config root = fmap fromString <$> findImages' config root
  

findNewImages :: Config -> FilePath -> Set DocName -> IO [(DocName, Maybe ImageInfo)]
findNewImages config root existing = do
  images <- findImages config root
  for (filter (`S.notMember` existing) images) $ 
    (\k -> (k,) <$> imageInfo root k)

