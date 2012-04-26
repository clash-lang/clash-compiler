{-# OPTIONS_GHC -fno-warn-orphans #-}
module CLaSH.Core.Util where

import Data.Maybe                     (fromMaybe)
import Data.Hashable                  (Hashable(..))
import qualified Data.HashMap.Lazy as HashMap
import Unbound.LocallyNameless        (unembed)
import Unbound.LocallyNameless.Name   (Name(..))

import CLaSH.Core.DataCon (dcWorkId)
import CLaSH.Core.Term    (Term(..),TmName)
import CLaSH.Core.Type    (Type,Kind,TyName)
import CLaSH.Core.Var     (varType)

instance Hashable (Name a) where
  hash (Nm _ (str,int)) = hashWithSalt (hash int) str
  hash (Bn _ i0 i1)     = hash i0 `hashWithSalt` i1

type Delta = HashMap.HashMap TyName Kind
type Gamma = HashMap.HashMap TmName Type

termType ::
  Gamma
  -> Term
  -> Type
termType gamma e = case e of
  Var x           -> fromMaybe (error $ "termType: " ++ show x ++ " not found") $
                       HashMap.lookup x gamma
  Data dc         -> unembed . varType . dcWorkId $ dc
  Case scrut alts -> undefined
