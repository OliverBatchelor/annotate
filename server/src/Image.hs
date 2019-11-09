module Image (imageInfo, findNewImages, findImages) where

  import Prelude (lines)
  import Annotate.Prelude
  import Annotate.Common
  
  import qualified Data.Map as M
  import qualified Data.Set as S
  
  import System.Process (readProcessWithExitCode)
  
  import qualified Data.ByteString.Lazy.Char8 as BS
  import Text.Megaparsec hiding (some, many)
  import Text.Megaparsec.Char
  import Text.Megaparsec.Char.Lexer (decimal)
  --import Control.Monad.Combinators
  
  import qualified Data.Text as Text
  import System.FilePath
  import System.Directory
  import System.Posix.Files
  
  import Data.Time
  import Data.Time.Format
  
  import Text.Read (readMaybe)
  import qualified Data.Text as T
  
  import Server.Common (defaultInfo)
  import Data.Aeson.TH
  import Data.Aeson (Object, Value(..), eitherDecode', FromJSON(..))
  
  import Data.Scientific (floatingOrInteger)
    
  data ExifValue = 
      ExifText String |
      ExifInt Int | 
      ExifDouble Double | 
      ExifBool Bool  |
      ExifOther Value
        deriving (Show)

  instance FromJSON ExifValue where
    parseJSON (Number v) = case floatingOrInteger v of 
      Right n -> return $ ExifInt n
      Left r  -> return $ ExifDouble r
    parseJSON (String s)     = return $ ExifText (T.unpack s)
    parseJSON (Bool b)       = return $ ExifBool b
    parseJSON v = return $ ExifOther v


  getExif :: FilePath -> IO (Maybe (Map String ExifValue))
  getExif path = do
    (exit, out, _) <- readProcessWithExitCode "exiftool" (["-J", path] <> fmap ("-" <>) fields) ""
    return $ mconcat <$> decode' (BS.pack out)
      where
        fields = ["ImageWidth", "ImageHeight", "DateTimeOriginal"]
  
  getImageInfo :: TimeZone -> Map String ExifValue -> Maybe ImageInfo
  getImageInfo timeZone exif = do
    (ExifInt w) <- prop "ImageWidth"
    (ExifInt h) <- prop "ImageHeight"
    return $ ImageInfo (w, h) (prop "DateTimeOriginal" >>= toDate)
    where
      prop k = M.lookup k exif
  
      toDate :: ExifValue -> Maybe UTCTime
      toDate (ExifText str) = do 
        local <- parseTimeM True defaultTimeLocale "%Y:%m:%d %H:%M:%S" str
        return $ localTimeToUTC timeZone local
      toDate _ = Nothing
  
  imageInfo :: FilePath -> DocName -> IO (Maybe ImageInfo)
  imageInfo root filename = do
    timeZone <- getCurrentTimeZone
    properties <- getExif path
    let info = (properties >>= getImageInfo timeZone)
    print info
    return info
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
  
  
  type Parser = Parsec Void String
  
  -- example: 3927x500;2018:12:20 13:22:53%
  parseIdentify :: Parser (Dim, Maybe LocalTime)
  parseIdentify = do
    dim <- parseDim
    void $ char ';'
    datestr <- optional $ takeWhile1P (Just "date") (/= ';')
    void $ char ';'
    return (dim, join (fmap toDate datestr))
      where
        toDate :: String -> Maybe LocalTime
        toDate = parseTimeM True defaultTimeLocale "%Y:%m:%d %H:%M:%S"
  
  
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
  
