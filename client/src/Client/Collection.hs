module Client.Collection where

import Annotate.Prelude hiding (div)
import qualified Annotate.Prelude as Prelude

import Annotate.Common hiding (label)
import Annotate.Sorting

import Client.Common
import Scene.Types

import qualified Web.KeyCode as Key

import Data.List.PointedList (tryPrevious, tryNext, PointedList(..))

import qualified Data.Map as Map
import Data.Map (Map)

import Data.List (break, drop, dropWhile)
import Reflex

neighbourFrames :: DocName ->  Map DocName DocInfo -> ([Image], [Image])
neighbourFrames k images = trim $ break  ((== k) . fst) sorted where
  trim = bimap id (dropWhile (\image -> fst image == k))

  sorted = toImage <$> sortImages (SortName, False) (Map.toList images)
  toImage (k, info) = (k, info ^. #image . #size)
  

neighbourhood :: Document -> Map DocName DocInfo -> PointedList Image
neighbourhood Document{name, info} docs = PointedList prev image next
  where
    image = (name, info ^. #image . #size)
    (prev, next) = neighbourFrames name docs


nextFrames :: DocName -> Map DocName DocInfo -> [(DocName, DocInfo)]
nextFrames k images = drop 1 $ dropWhile ((/= k) . fst) sorted
  where sorted = sortImages (SortName, False) (Map.toList images)

nextFrame :: DocName -> Map DocName DocInfo -> Maybe DocName
nextFrame k  = fmap fst . preview _head . nextFrames k

prevFrames :: DocName -> Map DocName DocInfo -> [(DocName, DocInfo)]
prevFrames k images = drop 1 $ dropWhile ((/= k) . fst) sorted
  where sorted = sortImages (SortName, True) (Map.toList images)

prevFrame :: DocName -> Map DocName DocInfo -> Maybe DocName
prevFrame k  = fmap fst . preview _head . prevFrames k    

slideShow :: (Reflex t, MonadHold t m, MonadFix m) => SceneInputs t -> PointedList a -> m (Dynamic t a)
slideShow SceneInputs{keyCombo, keyUp} list = do

  position <- foldDyn ($) list $ mergeWith (.) 
    [ tryPrevious <$ keyCombo Key.ArrowLeft [Key.Control]
    , tryNext     <$ keyCombo Key.ArrowRight [Key.Control]
    , const list <$ keyUp Key.Control ]

  return (_focus <$> position)