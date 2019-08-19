module Server.Export where

import Annotate.Prelude
import Server.Common

import qualified Data.Map as Map
import qualified Data.Set as S

import Control.Concurrent.Log
import qualified Data.Text as Text

import Data.List (transpose, (!!), takeWhile, dropWhile, scanl)
import Control.Lens (hasn't, _Cons, makePrisms, has)

import Data.Maybe (fromJust)
import Annotate.Editor  
import Server.Store

data AnnEntry = AnnEdit AnnotationPatch 
              | AnnDetected Detection
              | AnnReview Detection
              | AnnMissed Detection
              | AnnSaved BasicAnnotation
    deriving (Show, Generic)



makePrisms ''AnnEntry  
makePrisms ''AnnStatus


exportCollection :: Store -> TrainCollection
exportCollection Store{..} = TrainCollection
  { config = config
  , images = Map.elems (exportImage <$> images)
  }


importCollection :: TrainCollection -> Store
importCollection TrainCollection{..} = Store
  { config = config
  , images = Map.fromList (importImage <$> images)
  , trainer = def
  , preferences = def
  }

importImage :: TrainImage -> (DocName, Document)
importImage TrainImage{..} = (imageFile, document) where
  document = emptyDoc imageFile info
    & #annotations .~ annotations

  info = (defaultInfo imageFile image)
    {modified = Nothing, category = category, numAnnotations = length annotations, image = image}

  image = ImageInfo {size = imageSize, creation = imageCreation}

updateImage :: Document -> TrainImage
updateImage Document{..} = TrainImage
  { imageFile = name
  , imageSize = info ^. #image . #size
  , naturalKey = info ^. #naturalKey
  , imageCreation = info ^. #image . #creation
  , category  = info ^. #category
  , annotations = annotations
  , sessions = []
  , summaries = []
  , detections = detections
  } 

  
initialEntry :: Annotation -> Maybe AnnEntry 
initialEntry ann = do
  (tag, detection) <- ann ^. #detection
  case tag of 
      Detected -> Just $ AnnDetected detection
      Missed   -> Just $ AnnMissed detection
      Review   -> Just $ AnnReview detection
      _ -> Nothing





getActions :: Editor -> [DocumentPatch]
getActions final = case foldM f (final, []) (final ^. #undos) of
    Left err -> error (show err)
    Right (_, acts) -> acts

    where
      f (editor, acts) patch = do 
        (inverse, editor') <- applyPatch patch editor
        return (editor', inverse : acts)




exportImage :: Document -> TrainImage
exportImage doc = (updateImage doc) 
  {  sessions  = doc ^. #sessions
  ,  summaries = []
  }


debugReplay (session, result) = if null diff then Nothing else Just diff   where
  (_, replayed)  =  replay session
  diff = approxDiffF replayed result


withLog f filename = readLogFile filename >>= \case 
  Left err -> print err
  Right db -> f db

debugHistory :: FilePath -> IO ()
debugHistory = withLog $ \Store{images} -> do
  for_ images $ \image -> do 
    forM_ (preview (#sessions . _head) image) $ \session -> do
      putStrLn $ "Image: " <> show (image ^. #name) <> " - " <> show (image ^. (#info . #category))
      -- forM_ (sessionSummaries session) $ \summary -> do
      --   -- traverse_ print history
      --   print summary
      putStrLn "-------------"


debugReplays :: FilePath -> IO ()
debugReplays = withLog $ \Store{images} -> do 
  for_ images $ \image -> 
    for_ (documentSessions image) $ \(session, result) -> 
      for_ (debugReplay (session, result)) $ \(diff) -> do
        putStrLn $ "Image: " <> show (image ^. #name) <> " - " <> show (image ^. (#info . #category))
        let (initial, cmds) = replays session

        for_ (Map.toList diff) $ \(k, d) -> do
          putStrLn $ "Initial: " <> show (Map.lookup k (initial ^. #annotations))

          case d of 
            This a -> putStrLn $  "In replay: " <> show a
            That a -> putStrLn $ "In result: " <> show a
            These a b -> do 
              putStrLn $ "In replay: " <> show a
              putStrLn $ "In result: " <> show b

        putStrLn "------history---------"
        traverse_ print (session ^. #history)

        putStrLn "------edits---------"
        for cmds $ \(cmd, editor) -> do 
          putStrLn $ "Command: " <> show cmd
          for_ (editor ^. #undos) $ \undo -> 
              putStrLn $ "  " <> show undo

