{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns  #-}
module CLaSH.Normalize.Util where

import Control.Lens                ((.=),(%=))
import qualified Control.Lens      as Lens
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.List as      List
import Unbound.LocallyNameless     (Fresh,aeq,embed,unbind,unembed)

import CLaSH.Core.DataCon (dataConInstArgTys)
import CLaSH.Core.Term    (Term(..),TmName)
import CLaSH.Core.TyCon   (TyCon(..),tyConDataCons)
import CLaSH.Core.Type    (Type(..),TypeView(..),tyView,isFunTy)
import CLaSH.Core.Util    (Gamma,collectArgs,termType)
import CLaSH.Core.Var     (Var(..),Id)
import CLaSH.Netlist.Util (representableType,splitNormalized)
import CLaSH.Normalize.Types
import CLaSH.Rewrite.Types
import CLaSH.Rewrite.Util

nonRep ::
  Type
  -> Bool
nonRep = not . representableType

isBoxTy ::
  Type
  -> Bool
isBoxTy = isBoxTy' []

isBoxTy' ::
  [Type]
  -> Type
  -> Bool
isBoxTy' ts ty@(tyView -> TyConApp tc tys)
  | ty `notElem` ts = any (\t -> isBoxTy' (ty:ts) t || isFunTy t)
                          (conArgs tc tys)
  | otherwise       = False
isBoxTy' _ _        = False

isPolyFunTy ::
  Fresh m
  => Type
  -> m Bool
isPolyFunTy (tyView -> FunTy _ _) = return True
isPolyFunTy (ForAllTy tvT)        = unbind tvT >>= (isPolyFunTy . snd)
isPolyFunTy _                     = return False

conArgs :: TyCon -> [Type] -> [Type]
conArgs tc tys = bigUnionTys $ map (flip dataConInstArgTys tys)
               $ tyConDataCons tc
  where
    bigUnionTys :: [[Type]] -> [Type]
    bigUnionTys []   = []
    bigUnionTys tyss = foldl1 (List.unionBy aeq) tyss

alreadyInlined ::
  TmName
  -> NormalizeMonad Bool
alreadyInlined f = do
  cf <- Lens.use curFun
  inlinedHM <- Lens.use inlined
  case HashMap.lookup cf inlinedHM of
    Nothing -> do
      return False
    Just inlined' -> do
      if (f `elem` inlined')
        then return True
        else do
          return False

commitNewInlined :: NormRewrite
commitNewInlined _ e = R $ liftR $ do
  cf <- Lens.use curFun
  nI <- Lens.use newInlined
  inlinedHM <- Lens.use inlined
  case HashMap.lookup cf inlinedHM of
    Nothing -> inlined %= (HashMap.insert cf nI)
    Just _  -> inlined %= (HashMap.adjust (`List.union` nI) cf)
  newInlined .= []
  return e

fvs2bvs ::
  Gamma
  -> [TmName]
  -> [Id]
fvs2bvs gamma = map (\n -> Id n (embed $ gamma HashMap.! n))

isClosed ::
  (Functor m, Fresh m)
  => Term
  -> m Bool
isClosed e = do
  ty <- termType e
  fmap not $ isPolyFunTy ty

isConstant :: Term -> Bool
isConstant e = case collectArgs e of
  (Data _ _, args) -> all (either isConstant (const True)) args
  (Prim _, args)   -> all (either isConstant (const True)) args
  (Literal _,_)    -> True
  _                -> False

getWrappedF :: (Fresh m,Functor m) => Term -> m (Maybe Term)
getWrappedF body = do
      (funArgs,lbs,_) <- splitNormalized body
      case lbs of
        [(_,bExpr)] -> return $! uncurry (reduceArgs funArgs) (collectArgs $ unembed bExpr)
        _           -> return Nothing

reduceArgs :: [Id] -> Term -> [Either Term Type] -> Maybe Term
reduceArgs []  appE []                            = Just appE
reduceArgs (_:_) _  []                            = Nothing
reduceArgs ids appE (Right ty:args)               = reduceArgs ids (TyApp appE ty) args
reduceArgs (id1:ids) appE (Left (Var _ nm):args)  | varName id1 == nm = reduceArgs ids appE args
                                                  | otherwise         = Nothing
reduceArgs ids appE (Left arg:args)               | isConstant arg    = reduceArgs ids (App appE arg) args
                                                  | otherwise         = Nothing
