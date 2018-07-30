module Annotate.Prelude
  ( module X
  ) where


import Control.Applicative as X
import Control.Monad as X
import Control.Monad.Trans as X
import Control.Category as X

import Control.Monad.Identity as X (Identity, runIdentity)
import Control.Monad.Reader.Class as X (MonadReader (..), asks)
import Control.Monad.State.Class as X (MonadState(..), gets)
import Control.Monad.Writer.Class as X (MonadWriter (..))
import Control.Monad.Fix as X (MonadFix (..))

import Control.Exception  as X (Exception(..), throw, finally)

import Control.Monad.IO.Class as X

import Data.Text as X (Text)
import Data.Functor as X
import Data.Functor.Contravariant as X
import Data.Monoid as X (Monoid(..), mempty, First(..), Last(..))
import Data.Semigroup as X (Semigroup(..), (<>))
import Data.Foldable as X
import Data.Traversable as X

import Control.Lens as X
  ( (%~), (^.), (^?), (.~), (&), (<&>)
  , over, view, preview
  , Lens, Lens', Traversal, Traversal'
  , at, ix, _1, _2, _3, _4
  , _Just, _Nothing, _Left, _Right

  , ifor, itraverse
  )
import Data.Aeson as X
  (ToJSON(..), FromJSON(..), FromJSONKey(..), ToJSONKey(..)
  , decode, decode', encode, decodeStrict, decodeStrict', eitherDecode, eitherDecodeStrict)

import Data.List as X (intersperse, filter, zip, zip3, zipWith, zipWith3, lookup, take, drop, elem, uncons)
import Data.List.NonEmpty as X (NonEmpty(..), nonEmpty)
import Data.Maybe as X (fromMaybe, maybe, catMaybes, Maybe (..), maybeToList, isJust, isNothing)
import Data.Either as X (either, Either (..), isLeft, isRight)

import Data.Int as X
import Data.Word as X
import Data.Bool as X

import Data.Char as X
import Data.Void as X

import Data.Set as X (Set)
import Data.Map as X (Map)

import Data.Dependent.Map as X (DMap)
import Data.Dependent.Sum as X (DSum(..))

import Data.Typeable as X


import GHC.Generics as X (Generic(..))
import Prelude as X (
  Read (..), Show(..), Eq(..), Ord(..), Enum(..), Floating(..), Integral(..), Num(..),
  Real(..), RealFloat(..), Fractional(..), Floating(..), Bounded(..), realToFrac,
  Integer, Char, Float, Int, Double, String, FilePath, IO,
  curry, uncurry, flip, const, fst, snd, fromIntegral,
  ($), undefined, error, subtract, print, putStr, putStrLn)

import Data.String as X (IsString(..))
import Data.Time.Clock as X

import Data.Generics.Labels as X ()
import GHC.OverloadedLabels as X

import System.Exit as X (ExitCode(..))
