
module Annotate.TH where

import Prelude
import Data.Monoid

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

--import qualified Data.Flat as Flat

makeInstances :: [Name] -> Q [Dec]
makeInstances types = concat <$> traverse jsonInstance types



jsonInstance :: Name -> Q [Dec]
jsonInstance name = [d| 
      instance Aeson.FromJSON $t where parseJSON = Aeson.genericParseJSON options 
      instance Aeson.ToJSON $t   where toJSON = Aeson.genericToJSON options

 --     instance Flat.Flat $t 
  |] 
 
    where t = return $ ConT name
