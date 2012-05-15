module CLaSH.Core.Util where

import Data.Maybe                     (fromMaybe)
import qualified Data.HashMap.Lazy as HashMap
import Unbound.LocallyNameless        (bind,embed,runFreshM,unbind,unembed,unrec)

import CLaSH.Core.DataCon (dataConWorkId)
import CLaSH.Core.Literal (literalType)
import CLaSH.Core.Prim    (Prim(..),primType)
import CLaSH.Core.Term    (Pat(..),Term(..),TmName)
import CLaSH.Core.Type    (Type,Kind,TyName,mkFunTy,mkForAllTy,applyTy,
  splitFunTy,isFunTy)
import CLaSH.Core.Var     (Var(..),TyVar,Id,varName,varType)
import CLaSH.Util

type Delta = HashMap.HashMap TyName Kind
type Gamma = HashMap.HashMap TmName Type

termType ::
  Gamma
  -> Term
  -> Type
termType gamma e = case e of
  Var x       -> fromMaybe (error $ $(curLoc) ++ "termType: " ++ show x ++ " not found") $
                   HashMap.lookup x gamma
  Data dc     -> snd . dataConWorkId $ dc
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
applyTypeToArgs opTy (Left _:args)   = case splitFunTy opTy of
  Just (_,resTy) -> applyTypeToArgs resTy args
  Nothing        -> error $ $(curLoc) ++ "applyTypeToArgs splitFunTy: not a funTy"

patIds :: Pat -> [Id]
patIds (DataPat _ ids) = ids
patIds _               = []

mkTyVar ::
  Delta
  -> TyName
  -> TyVar
mkTyVar delta tyName = TyVar tyName (embed tyKind)
  where
    tyKind = fromMaybe (error $ $(curLoc) ++ "mkTyVar: " ++ show tyName ++ " not found in: " ++ show delta) $
               HashMap.lookup tyName delta

mkId ::
  Gamma
  -> TmName
  -> Id
mkId gamma tmName = Id tmName (embed tmType)
  where
    tmType = fromMaybe (error $ $(curLoc) ++ "mkId: " ++ show tmName ++ " not found in: " ++ show gamma) $
               HashMap.lookup tmName gamma

mkTyLams ::
  Term
  -> [TyVar]
  -> Term
mkTyLams = foldr (TyLam `dot` bind)

mkLams ::
  Term
  -> [Id]
  -> Term
mkLams = foldr (Lam `dot` bind)

mkApps ::
  Term
  -> [Term]
  -> Term
mkApps = foldl App

mkTyApps ::
  Term
  -> [Type]
  -> Term
mkTyApps = foldl TyApp

isFun ::
  Gamma
  -> Term
  -> Bool
isFun g = isFunTy . termType g

isLam ::
  Term
  -> Bool
isLam (Lam _) = True
isLam _       = False

isVar ::
  Term
  -> Bool
isVar (Var _) = True
isVar _       = False

isCon ::
  Term
  -> Bool
isCon (Data _) = True
isCon _        = False

isPrimCon ::
  Term
  -> Bool
isPrimCon (Prim (PrimCon _)) = True
isPrimCon _                  = False

isPrimFun ::
  Term
  -> Bool
isPrimFun (Prim (PrimFun _ _)) = True
isPrimFun _                    = False
