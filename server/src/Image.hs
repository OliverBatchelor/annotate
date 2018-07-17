module Image (imageInfo, findNewImages, findImages) where

import Annotate.Prelude
import Annotate.Common

import qualified Data.Map as M
import qualified Data.Set as S

import System.Process (readProcessWithExitCode)

import Text.Megaparsec hiding (some, many)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
--import Control.Monad.Combinators

import qualified Data.Text as Text
import System.FilePath
import System.Directory


defaultInfo :: Dim -> DocInfo
defaultInfo dim = DocInfo
  { modified = Nothing
  , category = New
  , imageSize = dim
  }


imageInfo :: FilePath -> DocName -> IO (Maybe DocInfo)
imageInfo root filename = do
  (exit, out, _) <- readProcessWithExitCode "identify" [path] ""
  return $ case exit of
    ExitSuccess -> toInfo <$> parseMaybe parseIdentify out
    _           -> Nothing

    where
      toInfo (_, _, dim) = defaultInfo dim
      path = root </> Text.unpack filename


validExtension :: [String] -> FilePath -> Bool
validExtension exts filename = any (\e -> fmap toLower e == ext) exts where
  ext = fmap toLower (takeExtension filename)


findImages :: Config -> FilePath -> IO [DocName]
findImages config root = do
  contents <- fmap fromString <$> listDirectory root
  return $ fromString <$> filter (validExtension exts) contents
    where exts = Text.unpack <$> config ^. #extensions

findNewImages :: Config -> FilePath -> Set DocName -> IO [(DocName, DocInfo)]
findNewImages config root existing = do
  images <- findImages config root

  catMaybes <$> for (filter (`S.notMember` existing) images) 
    (\image -> fmap (image, ) <$> imageInfo root image)


type Parser = Parsec Void String

-- example: /home/oliver/trees/_DSC2028.JPG JPEG 1600x1064 1600x1064+0+0 8-bit sRGB 958KB 0.000u 0:00.000
parseIdentify :: Parser (FilePath, String, Dim)
parseIdentify = do
  filename <- parseFilename
  space
  code <- fileCode
  space
  dim <- parseDim
  void $ many anyChar
  return (filename, code, dim)


parseFilename :: Parser String
parseFilename = takeWhile1P Nothing (not . isSpace) <?> "word"

fileCode :: Parser String
fileCode = some letterChar <?> "file code"

parseDim :: Parser Dim
parseDim = do
  w <- decimal
  void $ char 'x'
  h <- decimal
  return (w, h)


-- imageInfo :: FilePath -> IO (Maybe DocInfo)
-- imageInfo filename = do
--   info <- docInfo <$> Codec.readImageWithMetadata filename
--   info <$ print (filename, info)
--   where
--     docInfo (Left _)              = Nothing
--     docInfo (Right (_, metadata)) = toInfo <$>
--       (Codec.lookup Width metadata) <*> (Codec.lookup Height metadata)
--
--     toInfo w h = DocInfo Nothing False (fromIntegral w, fromIntegral h)
