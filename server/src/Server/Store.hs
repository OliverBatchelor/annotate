module Server.Store where

import Annotate.Prelude
import Server.Common

import qualified Data.Map as Map
import qualified Data.Set as S

import Data.SafeCopy

import Control.Concurrent.Log
import qualified Data.Text as Text

import Data.List (transpose, (!!), takeWhile, dropWhile, scanl)
import Control.Lens (hasn't, _Cons)

import Data.Maybe (fromJust)
import Annotate.Editor



$(deriveSafeCopy 0 'base ''DetectionTag)
$(deriveSafeCopy 0 'base ''BasicAnnotation)

$(deriveSafeCopy 4 'base ''Annotation)

$(deriveSafeCopy 0 'base ''Hash32)


$(deriveSafeCopy 0 'base ''V2)
$(deriveSafeCopy 0 'base ''Box)
$(deriveSafeCopy 0 'base ''Circle)
$(deriveSafeCopy 0 'base ''Polygon)
$(deriveSafeCopy 0 'base ''WideLine)

$(deriveSafeCopy 2 'base ''Detection)
$(deriveSafeCopy 0 'base ''TrainSummary)
$(deriveSafeCopy 1 'base ''Detections)


$(deriveSafeCopy 4 'base ''DetectionStats)

$(deriveSafeCopy 0 'base ''Margins)

$(deriveSafeCopy 0 'base ''Checkpoint)
$(deriveSafeCopy 0 'base ''ImageCat)
$(deriveSafeCopy 3 'base ''Submission)


$(deriveSafeCopy 1 'base ''SubmitType)


$(deriveSafeCopy 15 'base ''Document)
$(deriveSafeCopy 2 'base ''NaturalKey)


$(deriveSafeCopy 0 'base ''TrainStats)
$(deriveSafeCopy 0 'base ''Shape)

$(deriveSafeCopy 7 'base ''DocInfo)

$(deriveSafeCopy 0 'base ''ImageInfo)


$(deriveSafeCopy 0 'base ''Config)


$(deriveSafeCopy 2 'base ''ClassConfig)

$(deriveSafeCopy 0 'base ''ShapeConfig)

$(deriveSafeCopy 2 'base ''Edit)


$(deriveSafeCopy 0 'base ''HistoryEntry)

$(deriveSafeCopy 0 'base ''OpenSession)
$(deriveSafeCopy 0 'base ''Session)
$(deriveSafeCopy 0 'base ''OpenType)

$(deriveSafeCopy 0 'base ''Modify)

$(deriveSafeCopy 0 'base ''DocumentPatch)
$(deriveSafeCopy 0 'base ''AnnotationPatch)

$(deriveSafeCopy 2 'base ''Store)
$(deriveSafeCopy 2 'base ''Command)

$(deriveSafeCopy 0 'base ''TrainerState)
$(deriveSafeCopy 0 'base ''ModelState)

$(deriveSafeCopy 0 'base ''AssignmentMethod)


$(deriveSafeCopy 7 'base ''Preferences)

$(deriveSafeCopy 1 'base ''DisplayPreferences)

$(deriveSafeCopy 0 'base ''DetectionParams)
$(deriveSafeCopy 0 'base ''SortKey)
$(deriveSafeCopy 0 'base ''FilterOption)

$(deriveSafeCopy 5 'base ''SortOptions)

$(deriveSafeCopy 0 'base ''ImageSelection)


instance (SafeCopy p, SafeCopy (PatchTarget p), Ord k, SafeCopy k) => SafeCopy (DeepPatchMap k p) where
  putCopy (DeepPatchMap m) = contain $ safePut m
  getCopy = contain $ DeepPatchMap <$> safeGet


docInfo :: DocName -> Traversal' Store DocInfo
docInfo k = #images . ix k . #info

updateInfo :: Document -> UTCTime -> (Store -> Store)
updateInfo doc time = over (docInfo k) $ \info ->
  info & #modified .~ Just time & #numAnnotations .~ length (doc ^. #annotations)
    where k = view #name doc

updateImageInfo :: (DocName, Maybe ImageInfo) ->  (Map DocName Document -> Map DocName Document)
updateImageInfo (k, Nothing)   = Map.delete k
updateImageInfo (k, Just info) = Map.alter f k where
  f Nothing      = Just (emptyImage k info)
  f (Just image) = Just (image & #info . #image .~ info) 

updateDocument :: Document -> (Store -> Store)
updateDocument doc = #images . at (doc ^. #name) .~ Just doc


updateStats :: Maybe DetectionStats -> DetectionStats -> DetectionStats 
updateStats Nothing stats = stats
updateStats (Just stats) stats' = stats' 
  { counts = stats' ^. #counts <|> stats ^. #counts 
  , frameVariation = stats' ^. #frameVariation <|> stats ^. #frameVariation
  }

updateDetections :: (DocName, Detections) -> Map DocName Document -> Map DocName Document
updateDetections (k, detections) = over (ix k) $ \doc -> doc 
  & #detections         .~ Just detections
  & #info . #detections .~ Just (updateStats (doc ^. (#info . #detections)) stats)
    where stats = detections ^. #stats


interp :: Fractional a => a -> a -> a -> a
interp t x total = t * x + (1 - t) * total

trainingStats :: [TrainSummary] -> TrainStats 
trainingStats training = TrainStats 
  { lossMean = sum losses / fromIntegral (length losses)
  , lossRunning = maybe 0 (foldr1 (interp 0.3)) (nonEmpty losses)
  } where 
    losses = view #loss <$> training
    

addTraining :: (DocName, [TrainSummary]) ->  Map DocName Document -> Map DocName Document
addTraining (k, summary) = over (ix k) addSummary where
  addSummary doc = doc 
    & #training .~ training 
    & #info . #training  .~ trainingStats training
      where training = take 20 (summary <> doc ^. #training)

emptyImage :: DocName -> ImageInfo -> Document
emptyImage k info = emptyDoc k (defaultInfo k info)

emptyDoc :: DocName -> DocInfo -> Document
emptyDoc k info = Document 
  { name = k
  , info = info 
  , annotations = mempty 
  , sessions = [] 
  , detections = Nothing
  , training = []
  }

selectCategory :: UserId -> Store -> ImageCat
selectCategory user store = (case (prefs ^. #assignMethod) of 
  AssignAuto    -> assignCat (prefs ^. #trainRatio) (countSubmitted $ store ^. #images)
  AssignCat cat -> cat)
    where  prefs = lookupPreferences store user

countSubmitted = Map.size . Map.filter notNew
  where notNew =  (/= CatNew) . view (#info . #category)


assignCat :: Int -> Int -> ImageCat
assignCat trainRatio n = if n `mod` (trainRatio + 1) == trainRatio 
  then CatValidate
  else CatTrain


checkSubmission :: Submission -> Document -> Maybe ErrCode
checkSubmission Submission{session, annotations} doc = ErrSubmit <$> 
    (check (session ^. #initial ~= doc ^. #annotations) "Failed to match initial annotations"
    <|> check (checkReplay (session, annotations)) "Annotation replay failed consistency")
  
  where check b msg = if not b then Just msg else Nothing


checkStore :: Store -> [(DocName, ImageCat)]
checkStore Store{images} = Map.toList $ view (#info . #category) <$> 
  Map.filter (not . checkReplays) images


lookupPreferences :: Store -> UserId -> Preferences
lookupPreferences store user = fromMaybe def (store ^. #preferences . at user)

submitDocument :: UserId -> UTCTime -> Submission -> (Store -> Store)
submitDocument user time Submission{..} store = store & #images . ix name %~ \doc -> 
  case method of
    SubmitNew     -> doc 
        & #sessions .~ []
        & storeChanges 
        & #info . #category .~ (selectCategory user store) 
        & #info . #reviews .~ 0

    SubmitDiscard -> doc & storeChanges 
      & #info . #category .~ CatDiscard
      & #info . #reviews .~ 0

    SubmitConfirm cat -> doc & storeChanges
      & #info . #reviews %~ (+1) 
      & #info . #category %~ maybe id const cat

    SubmitAutoSave -> doc & storeChanges

  where 
    storeChanges doc = doc 
      & #annotations .~ annotations
      & #sessions %~ (`snoc` session)

      & #info %~ \info -> info 
        & #modified .~ Just time
        & #numAnnotations .~ length annotations


  
instance Persistable Store where
  type Update Store = Command

  update (CmdUpdate doc time) = updateInfo doc time . updateDocument doc
  update (CmdImages new)        = over #images (Map.union new') where
    new' = Map.mapWithKey emptyDoc (Map.fromList new)

  update (CmdUpdateImages updates) = over #images $ 
    (flip (foldr updateImageInfo) updates)
 

  update (CmdCategory k cat)      = docInfo k . #category .~ cat
  update (CmdClass k conf)  = over (#config . #classes) (Map.alter (const conf) k)
  update (CmdSetRoot path)  = #config . #root .~ path

  update (CmdCheckpoint cp) = over #trainer $ checkpoint cp

  update (CmdPreferences user preferences) = over #preferences (Map.insert user preferences)
  update (CmdDetections detections) = over #images $
    (flip (foldr updateDetections)) (Map.toList detections)

  update (CmdSubmit user submission time) = submitDocument user time submission 
  update (CmdTraining summaries) = over #images $ 
    (flip (foldr addTraining)) (Map.toList summaries)



applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen True f = f
applyWhen False _ = id

checkpoint :: Checkpoint -> TrainerState -> TrainerState
checkpoint Checkpoint{..} state = if run /= state ^. #run then reset else update
    where
      update = state & #current .~ model
                     & applyWhen isBest (#best .~ model)

      reset = TrainerState model model run
      model = ModelState Nothing epoch score

      (run, epoch) = networkId

initialStore :: Config -> Store
initialStore config = Store
  { config = config
  , images = Map.empty
  , trainer = def
  , preferences = mempty
  }
