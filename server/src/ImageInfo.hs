module ImageInfo (imageInfo) where

import Annotate.Common
import Annotate.Types

import System.Process (readProcessWithExitCode)

import Text.Megaparsec hiding (some, many)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)
--import Control.Monad.Combinators

import qualified Data.Text as Text
import System.FilePath


defaultInfo :: Dim -> DocInfo
defaultInfo dim = DocInfo
  { modified = Nothing
  , category = New
  , imageSize = dim
  }

type Parser = Parsec Void String

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


imageInfo :: FilePath -> DocName -> IO (Maybe DocInfo)
imageInfo root filename = do
  (exit, out, _) <- readProcessWithExitCode "identify" [path] ""
  return $ case exit of
    ExitSuccess -> toInfo <$> parseMaybe parseIdentify out
    _           -> Nothing

    where
      toInfo (_, _, dim) = defaultInfo dim
      path = root </> Text.unpack filename
