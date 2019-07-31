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



summarise :: [AnnEntry] ->  AnnSummary
summarise history = AnnSummary 
  { status = case (preview _last history) of
    (Just (AnnSaved ann))   -> StatusActive ann
    (Just (AnnEdit Delete)) -> StatusDeleted
    _ -> StatusThresholded

  , createdBy = case history of
    (AnnEdit (Add _) : _) -> CreatedAdd
    (AnnDetected d : AnnEdit (SetTag (Confirmed True)) : _) -> CreatedConfirm d
    (AnnDetected d : _) -> CreatedDetect d
    _ -> error $ "unknown creation method: " <> show history
  
  , changedClass = any (has (_AnnEdit . _SetClass)) history
  , transformed = any (has (_AnnEdit . _Transform)) history
  , actions = length $ filter (has _AnnEdit) history
  }


getActions :: Editor -> [DocumentPatch]
getActions final = case foldM f (final, []) (final ^. #undos) of
    Left err -> error (show err)
    Right (_, acts) -> acts

    where
      f (editor, acts) patch = do 
        (inverse, editor') <- applyPatch patch editor
        return (editor', inverse : acts)

sessionSummaries :: Session -> Map AnnotationId AnnSummary
sessionSummaries = fmap summarise . sessionHistories 

sessionHistories :: Session -> Map AnnotationId [AnnEntry]
sessionHistories session = sessionHistories' session mempty
      
sessionHistories' :: Session -> Map AnnotationId [AnnEntry] -> Map AnnotationId [AnnEntry]
sessionHistories' session existing = Map.mapWithKey saved histories where

    initial = Map.mapMaybe initialEntry (openSession "" session ^. #annotations)
    (editor, result) = replay session
    saved k history = case Map.lookup k result of
      Nothing   ->  history
      Just ann  ->  snoc history (AnnSaved ann)

    undos = fmap (pure . AnnEdit) <$> 
        catMaybes (preview _PatchAnns <$> getActions editor)

    histories = foldl (Map.unionWith (<>)) existing ((pure <$> initial) : undos)



imageHistories :: Document -> Map AnnotationId [AnnEntry]
imageHistories Document{sessions} = foldl f mempty sessions where
  f prev session  = sessionHistories' session (Map.filter isSaved prev)
  isSaved = has (_last . _AnnSaved)


exportImage :: Document -> TrainImage
exportImage doc = (updateImage doc) 
  {  sessions  = doc ^. #sessions
  ,  summaries = maybe [] Map.elems $ sessionSummaries <$> doc ^? (#sessions . _head)
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
      forM_ (sessionSummaries session) $ \summary -> do
        -- traverse_ print history
        print summary
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

