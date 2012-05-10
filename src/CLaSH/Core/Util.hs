{-# OPTIONS_GHC -fno-warn-orphans #-}
module CLaSH.Core.Util where

import Data.Maybe                     (fromMaybe)
import Data.Hashable                  (Hashable(..))
import qualified Data.HashMap.Lazy as HashMap
import Unbound.LocallyNameless        (runFreshM,unbind,unembed,unrec)
import Unbound.LocallyNameless.Name   (Name(..))

import CLaSH.Core.DataCon (dcWorkId)
import CLaSH.Core.Literal (literalType)
import CLaSH.Core.Prim    (primType)
import CLaSH.Core.Term    (Pat(..),Term(..),TmName)
import CLaSH.Core.Type    (Type,Kind,TyName,mkFunTy,mkForAllTy,applyTy,
  splitFunTy_maybe)
import CLaSH.Core.Var     (Id,varName,varType)

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
  Var x       -> fromMaybe (error $ "termType: " ++ show x ++ " not found") $
                   HashMap.lookup x gamma
  Data dc     -> snd . fromMaybe (error "no work id") . dcWorkId $ dc
  Literal l   -> literalType l
  Prim p      -> primType p
  Lam b       -> let (v,e') = runFreshM $ unbind b
                     ety    = termType
                                (HashMap.insert (varName v)
                                                (unembed $ varType v) gamma)
                                e'
                 in mkFunTy (unembed $ varType v) ety
  TyLam b     -> let (tv,e') = runFreshM $ unbind b
                     ety     = termType gamma e'
                 in mkForAllTy tv ety
  App _ _     -> case collectArgs e of
                   (fun, args) -> applyTypeToArgs (termType gamma fun) args
  TyApp e' ty -> termType gamma e' `applyTy` ty
  Letrec b    -> let (xes,e') = runFreshM $ unbind b
                     gamma'   = foldl (\g v ->
                                        HashMap.insert
                                          (varName v)
                                          (unembed $ varType v) g
                                     ) gamma (map fst $ unrec xes)
                 in termType gamma' e'
  Case _ alts -> let (p,e') = runFreshM $ unbind (head alts)
                     gamma' = foldl (\g v ->
                                      HashMap.insert
                                        (varName v)
                                        (unembed $ varType v) g
                                    ) gamma (patIds p)
                 in termType gamma' e'

collectArgs ::
  Term
  -> (Term, [Either Term Type])
collectArgs = go []
  where
    go args (App e1 e2) = go (Left e2:args) e1
    go args (TyApp e t) = go (Right t:args) e
    go args e           = (e, args)

applyTypeToArgs :: Type -> [Either Term Type] -> Type
applyTypeToArgs opTy [] = opTy
applyTypeToArgs opTy (Right ty:args) = applyTypeToArgs (opTy `applyTy` ty)
                                         args
applyTypeToArgs opTy (Left _:args)   = case splitFunTy_maybe opTy of
  Just (_,resTy) -> applyTypeToArgs resTy args
  Nothing        -> error "applyTypeToArgs splitFunTy: not a funTy"

patIds :: Pat -> [Id]
patIds (DataPat _ ids) = ids
patIds _               = []
