

type AnnotationId = Int
type ClassId = Int

data Annotation shape = Annotation shape
  { shape      :: !shape
  , label      :: !ClassId
  , confidence :: !Float
  } deriving (Generic, Show, Eq)


instance Selection shape => Selection (Annotation shape) where
  type Parts (Annotation shape) = Parts shape
  toggle = toggle @shape
  complete = complete . view #shape


instance ApproxEq shape => Annotation shape where
  (~=) (Annotation s l c) (Annotation s' l' c') = s ~= s' && l == l' && d == d'
  
  
type AnnotationMap shape = Map AnnotationId (Annotation shape)
type ReviewMap shape = Map AnnotationId (These (Annotation shape) (Annotation shape))

type Review shape = (Maybe (Parts shape)), Annotation shape)



type HistoryPair edit = (UTCTime, DocCommand edit)  

data Edit anns
  = EditSetClass ClassId (Parts anns)
  | EditDelete (Parts anns)
  | EditTransform Rigid (Parts anns)
  | EditAdd anns
  | EditConfirm (Parts anns)
  deriving (Generic, Show, Eq)


data DocCommand edit
  = DocEdit edit
  | DocUndo 
  | DocRedo 
  | DocThreshold Float
  | DocTimeout Float
    deriving (Show, Eq, Generic)

    

data DocumentState shape = DocumentState 
  { annotations :: AnnotationMap shape
  , threshold   :: Float
  , nextId      :: AnnotationId
  } deriving (Show,  Generic, Hashable)

data Session shape = Session shape
  { initial    :: DocumentState shape
  , review     :: Reviews shape
  , time       :: UTCTime
  , history    :: [HistoryPair (Edit (AnnotationMap shape))]
  } deriving (Show, Generic)      

  
data Submission shape = Submission shape 
  { name        :: DocName
  , state       :: DocumentState
  , session     :: Session
  , method      :: SubmitType
  } deriving (Generic, Show)


class (Patch p, Patched ann ~ p, PatchTarget p ~ ann) => Patchable ann where
  type Patched ann 

instance Selection Circle where
  type Parts a = ()
  toggle _ _ = ()
  parts    _ = ()

instance Selection Box where
  type Parts a = ()
  toggle _ _ = ()
  parts    _ = ()
  



data CirclePatch = CircleRigid Rigid

data BoxPatch 
    = BoxRigid Rigid
    | BoxDragCorner Corner Vec
    deriving (Generic, Show)

instance Patchable Circle where
  Patched Circle = CirclePatch

instance Patchable Box where
  Patched Box = BoxPatch