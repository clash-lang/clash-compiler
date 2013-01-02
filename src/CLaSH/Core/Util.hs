module CLaSH.Core.Util where

import Data.Maybe                     (fromMaybe)
import qualified Data.HashMap.Lazy as HashMap
import Unbound.LocallyNameless        (Fresh,bind,embed,runFreshM,unbind,unembed,unrec,string2Name)

import CLaSH.Core.DataCon (dcWorkId)
import CLaSH.Core.Literal (literalType)
import CLaSH.Core.Prim    (Prim(..),primType)
import CLaSH.Core.Term    (Pat(..),Term(..),TmName)
import CLaSH.Core.Type    (Kind,TyName,mkFunTy,mkForAllTy,splitFunTy,isFunTy,
  applyTy)
import CLaSH.Core.TypeRep (Type(..))
import CLaSH.Core.Var     (Var(..),TyVar,Id,varName,varType)
import CLaSH.Util

type Delta = HashMap.HashMap TyName Kind
type Gamma = HashMap.HashMap TmName Type

termType ::
  Fresh m
  => Gamma
  -> Term
  -> m Type
termType gamma e = case e of
  Var x       -> return $ fromMaybe (error $ $(curLoc) ++ "termType: " ++ show x ++ " not found: " ++ show (HashMap.keys gamma)) $
                   HashMap.lookup x gamma
  Data dc     -> return . snd . dcWorkId $ dc
  Literal l   -> return $ literalType l
  Prim p      -> return $ primType p
  Lam b       -> do (v,e') <- unbind b
                    eTy    <- termType
                                (HashMap.insert (varName v)
                                                (unembed $ varType v) gamma)
                                e'
                    return $ mkFunTy (unembed $ varType v) eTy
  TyLam b     -> do (tv,e') <- unbind b
                    eTy     <- termType gamma e'
                    return $ mkForAllTy tv eTy
  App _ _     -> case collectArgs e of
                   (fun, args) -> (termType gamma fun) >>=
                                  (`applyTypeToArgs` args)
  TyApp e' ty -> (termType gamma e') >>= (`applyTy` ty)
  Letrec b    -> do (xes,e') <- unbind b
                    let gamma' = foldl (\g v ->
                                         HashMap.insert
                                           (varName v)
                                           (unembed $ varType v) g)
                                       gamma (map fst $ unrec xes)
                    termType gamma' e'
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
  Nothing        -> error $ $(curLoc) ++ "applyTypeToArgs splitFunTy: not a funTy: " ++ show (opTy,e,args)

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
  => Gamma
  -> Term
  -> m Bool
isFun g t = fmap isFunTy $ termType g t

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

mapSyncTerm ::
  Type
  -> Term
mapSyncTerm (ForAllTy tvATy) =
  let (aTV,bTV,FunTy _ (FunTy aTy bTy)) = runFreshM $ do
                { (aTV',ForAllTy tvBTy) <- unbind tvATy
                ; (bTV',funTy)          <- unbind tvBTy
                ; return (aTV',bTV',funTy) }
      fId = Id (string2Name "f") (embed $ FunTy aTy bTy)
      xId = Id (string2Name "x") (embed aTy)
  in TyLam $ bind aTV $
     TyLam $ bind bTV $
     Lam   $ bind fId $
     Lam   $ bind xId $
     App (Var $ varName fId) (Var $ varName xId)

mapSyncTerm ty = error $ $(curLoc) ++ show ty

syncTerm ::
  Type
  -> Term
syncTerm (ForAllTy tvTy) =
  let (aTV,FunTy _ aTy) = runFreshM $ unbind tvTy
      xId = Id (string2Name "x") (embed aTy)
  in TyLam $ bind aTV $
     Lam   $ bind xId $
     Var   $ varName xId

syncTerm ty = error $ $(curLoc) ++ show ty
