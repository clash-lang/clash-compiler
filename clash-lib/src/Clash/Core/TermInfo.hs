{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Core.TermInfo where

import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)

import Clash.Core.HasType
import Clash.Core.Term
import Clash.Core.TyCon (tyConDataCons, isTupleTyConLike, TyConMap)
import Clash.Core.Type
import Clash.Core.Var
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Util.Interpolate as I

termSize :: Term -> Word
termSize (Var {})     = 1
termSize (Data {})    = 1
termSize (Literal {}) = 1
termSize (Prim {})    = 1
termSize (Lam _ e)    = termSize e + 1
termSize (TyLam _ e)  = termSize e
termSize (App e1 e2)  = termSize e1 + termSize e2
termSize (TyApp e _)  = termSize e
termSize (Cast e _ _) = termSize e
termSize (Tick _ e)   = termSize e
termSize (Let (NonRec _ x) e) = termSize x + termSize e
termSize (Let (Rec xs) e) = sum (bodySz:bndrSzs)
 where
  bndrSzs = map (termSize . snd) xs
  bodySz  = termSize e
termSize (Case subj _ alts) = sum (subjSz:altSzs)
 where
  subjSz = termSize subj
  altSzs = map (termSize . snd) alts

multPrimErr :: PrimInfo -> String
multPrimErr primInfo =  [I.i|
  Internal error in multiPrimInfo': could not produce MultiPrimInfo. This
  probably means a multi result blackbox's result type was not a tuple.
  PrimInfo:

    #{primInfo}
|]

splitMultiPrimArgs ::
  HasCallStack =>
  MultiPrimInfo ->
  [Either Term Type] ->
  ([Either Term Type], [Id])
splitMultiPrimArgs MultiPrimInfo{mpi_resultTypes} args0 = (args1, resArgs1)
 where
  resArgs1 = [id_ | Left (Var id_) <- resArgs0]
  (args1, resArgs0) = splitAt (length args0 - length mpi_resultTypes) args0

-- | Same as 'multiPrimInfo', but produced an error if it could not produce a
-- 'MultiPrimInfo'.
multiPrimInfo' :: HasCallStack => TyConMap -> PrimInfo -> MultiPrimInfo
multiPrimInfo' tcm primInfo =
  fromMaybe (error (multPrimErr primInfo)) (multiPrimInfo tcm primInfo)

-- | Produce 'MutliPrimInfo' for given primitive
multiPrimInfo :: TyConMap -> PrimInfo -> Maybe MultiPrimInfo
multiPrimInfo tcm primInfo
  | (_primArgs, primResTy) <- splitFunForallTy (primType primInfo)
  , TyConApp tupTcNm tupEls <- tyView primResTy
    -- XXX: Hardcoded for tuples
  , isTupleTyConLike tupTcNm
  , Just tupTc <- UniqMap.lookup tupTcNm tcm
  , [tupDc] <- tyConDataCons tupTc
  = Just $ MultiPrimInfo
    { mpi_primInfo = primInfo
    , mpi_resultDc = tupDc
    , mpi_resultTypes = tupEls }
multiPrimInfo _ _ = Nothing

-- | Does a term have a function type?
isFun :: TyConMap -> Term -> Bool
isFun m t = isFunTy m (inferCoreTypeOf m t)

-- | Does a term have a function or polymorphic type?
isPolyFun :: TyConMap -> Term -> Bool
isPolyFun m t = isPolyFunCoreTy m (inferCoreTypeOf m t)

-- | Is a term a recursive let-binding?
isLet :: Term -> Bool
isLet Let{} = True
isLet _ = False

-- | Is a term a variable reference?
isVar :: Term -> Bool
isVar (Var {}) = True
isVar _        = False

isLocalVar :: Term -> Bool
isLocalVar (Var v) = isLocalId v
isLocalVar _ = False

-- | Is a term a datatype constructor?
isCon :: Term -> Bool
isCon (Data {}) = True
isCon _         = False

-- | Is a term a primitive?
isPrim :: Term -> Bool
isPrim (Prim {}) = True
isPrim _         = False

-- | Is a term a tick?
isTick :: Term -> Bool
isTick Tick{} = True
isTick _ = False

-- | Is a term a cast?
isCast :: Term -> Bool
isCast (Cast {}) = True
isCast _         = False
