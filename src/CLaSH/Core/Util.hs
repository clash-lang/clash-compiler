{-# LANGUAGE ViewPatterns  #-}
module CLaSH.Core.Util where

import Data.Maybe (fromMaybe)
import Data.HashMap.Lazy (HashMap)
import Unbound.LocallyNameless (Fresh,bind,embed,runFreshM,unbind,unembed,string2Name,unrebind)

import CLaSH.Core.DataCon (dcWorkId,dcWrapIdM)
import CLaSH.Core.Literal (literalType)
import CLaSH.Core.Pretty  (showDoc)
import CLaSH.Core.Prim    (Prim(..),primType)
import CLaSH.Core.Term    (Pat(..),Term(..),TmName)
import CLaSH.Core.Type    (Type(..),Kind,TyName,TypeView(..),tyView,mkFunTy,mkForAllTy,splitFunTy,isFunTy,
  applyTy)
import CLaSH.Core.Var     (Var(..),TyVar,Id,varType)
import CLaSH.Util

type Gamma = HashMap TmName Type
type Delta = HashMap TyName Kind

termType ::
  (Functor m, Fresh m)
  => Term
  -> m Type
termType e = case e of
  Var t _     -> return t
  Data False dc -> return . snd . dcWorkId $ dc
  Data True dc  -> return . snd . (fromMaybe (error $ $(curLoc) ++ "No WrapId")) . dcWrapIdM $ dc
  Literal l   -> return $ literalType l
  Prim p      -> return $ primType p
  Lam b       -> do (v,e') <- unbind b
                    mkFunTy (unembed $ varType v) <$> termType e'
  TyLam b     -> do (tv,e') <- unbind b
                    mkForAllTy tv <$> termType e'
  App _ _     -> case collectArgs e of
                   (fun, args) -> (termType fun) >>=
                                  (`applyTypeToArgs` args)
  TyApp e' ty -> (termType e') >>= (`applyTy` ty)
  Letrec b    -> do (_,e') <- unbind b
                    termType e'
  Case _ ty _ -> return ty

collectArgs ::
  Term
  -> (Term, [Either Term Type])
collectArgs = go []
  where
    go args (App e1 e2) = go (Left e2:args) e1
    go args (TyApp e t) = go (Right t:args) e
    go args e           = (e, args)

collectBndrs ::
  Fresh m
  => Term
  -> m ([Either Id TyVar], Term)
collectBndrs e = go [] e
  where
    go bs (Lam b) = do
      (v,e') <- unbind b
      go (Left v:bs) e'
    go bs (TyLam b) = do
      (tv,e') <- unbind b
      go (Right tv:bs) e'
    go bs e' = return (reverse bs,e')

applyTypeToArgs :: Fresh m => Type -> [Either Term Type] -> m Type
applyTypeToArgs opTy []              = return opTy
applyTypeToArgs opTy (Right ty:args) = applyTy opTy ty >>=
                                       (`applyTypeToArgs` args)
applyTypeToArgs opTy (Left e:args)   = case splitFunTy opTy of
  Just (_,resTy) -> applyTypeToArgs resTy args
  Nothing        -> error $ $(curLoc) ++ "applyTypeToArgs splitFunTy: not a funTy:\n" ++ "opTy: " ++ showDoc opTy ++ "\nTerm: " ++ showDoc e ++ "\nOtherArgs: " ++ show args

patIds :: Pat -> [Id]
patIds (DataPat _ ids) = snd $ unrebind ids
patIds _               = []

mkTyVar ::
  Kind
  -> TyName
  -> TyVar
mkTyVar tyKind tyName = TyVar tyName (embed tyKind)

mkId ::
  Type
  -> TmName
  -> Id
mkId tmType tmName = Id tmName (embed tmType)

mkAbstraction ::
  Term
  -> [Either Id TyVar]
  -> Term
mkAbstraction = foldr (either (Lam `dot` bind) (TyLam `dot` bind))

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

mkTmApps ::
  Term
  -> [Term]
  -> Term
mkTmApps = foldl App

mkTyApps ::
  Term
  -> [Type]
  -> Term
mkTyApps = foldl TyApp

mkApps ::
  Term
  -> [Either Term Type]
  -> Term
mkApps = foldl (\e a -> either (App e) (TyApp e) a)

isFun ::
  (Functor m, Fresh m)
  => Term
  -> m Bool
isFun t = fmap isFunTy $ termType t

isLam ::
  Term
  -> Bool
isLam (Lam _) = True
isLam _       = False

isLet ::
  Term
  -> Bool
isLet (Letrec _) = True
isLet _          = False

isVar ::
  Term
  -> Bool
isVar (Var _ _) = True
isVar _         = False

isCon ::
  Term
  -> Bool
isCon (Data _ _) = True
isCon _          = False

isPrim ::
  Term
  -> Bool
isPrim (Prim _) = True
isPrim _        = False

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

mapSyncTerm ::
  Type
  -> Term
mapSyncTerm (ForAllTy tvATy) =
  let (aTV,bTV,tyView -> FunTy _ (tyView -> FunTy aTy bTy)) = runFreshM $ do
                { (aTV',ForAllTy tvBTy) <- unbind tvATy
                ; (bTV',funTy)          <- unbind tvBTy
                ; return (aTV',bTV',funTy) }
      fName = string2Name "f"
      xName = string2Name "x"
      fTy = mkFunTy aTy bTy
      fId = Id fName (embed fTy)
      xId = Id xName (embed aTy)
  in TyLam $ bind aTV $
     TyLam $ bind bTV $
     Lam   $ bind fId $
     Lam   $ bind xId $
     App (Var fTy fName) (Var aTy xName)

mapSyncTerm ty = error $ $(curLoc) ++ show ty

syncTerm ::
  Type
  -> Term
syncTerm (ForAllTy tvTy) =
  let (aTV,tyView -> FunTy _ aTy) = runFreshM $ unbind tvTy
      xName = string2Name "x"
      xId = Id xName (embed aTy)
  in TyLam $ bind aTV $
     Lam   $ bind xId $
     Var   aTy xName

syncTerm ty = error $ $(curLoc) ++ show ty

splitCombineTerm ::
  Bool
  -> Type
  -> Term
splitCombineTerm b (ForAllTy tvTy) =
  let (aTV,tyView -> FunTy dictTy (tyView -> FunTy inpTy outpTy)) = runFreshM $ unbind tvTy
      dictName = string2Name "splitCombineDict"
      xName    = string2Name "x"
      nTy      = if b then inpTy else outpTy
      dId      = Id dictName (embed dictTy)
      xId      = Id xName    (embed nTy)
      newExpr  = TyLam $ bind aTV $
                 Lam   $ bind dId $
                 Lam   $ bind xId $
                 Var nTy xName
  in newExpr

splitCombineTerm _ ty = error $ $(curLoc) ++ show ty
