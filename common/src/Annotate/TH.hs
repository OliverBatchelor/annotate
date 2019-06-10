
module Annotate.TH where

import Prelude
import Data.Monoid

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

-- instance FromJSON ShapeConfig where parseJSON = Aeson.genericParseJSON options
mkElems ::  Maybe String -> [ElementType] -> Q [Dec]
mkElems ns elems = concat <$> traverse gen elems  where
    gen (E name) = concat <$> traverse (\f -> f ns name) [mkElem', mkElem_, mkElem]
    gen (C name) = mkChild_ ns name

helper :: String -> Type -> Exp -> [Dec]
helper elemName typ f = [ SigD n typ, ValD (VarP n) (NormalB f) [], PragmaD (InlineP n Inline FunLike AllPhases) ]
  where n = mkName elemName

mkElem' :: Maybe String -> String -> Q [Dec]
mkElem' ns elemName = helper (elemName <> "'") <$> [t| Elem' |] <*> [| makeElem' ns elemName  |]