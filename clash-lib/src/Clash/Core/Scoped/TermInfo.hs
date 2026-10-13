{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Predicates on and measures of scoped Clash Core terms.

  Note that 'isVar' holds for local and global variables alike, while
  'isLocalVar' and 'isGlobalVar' tell them apart.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Core.Scoped.TermInfo
  ( -- * Measures
    termSize
  , termSizeSmallerThan
    -- * Predicates
  , isFun
  , isPolyFun
  , isLet
  , isVar
  , isLocalVar
  , isGlobalVar
  , isCon
  , isPrim
  , isLiteral
  , isTick
  , isCast
  , isClosed
    -- * Multi-result primitives
  , MultiPrimInfo (..)
  , multiPrimInfo
  , multiPrimInfo'
  , withMultiPrimResultTypes
  , splitMultiPrimArgs
  ) where

import Data.Maybe (fromMaybe, isJust)
import GHC.Stack (HasCallStack)

import Clash.Core.DataCon (DataCon)
import Clash.Core.TyCon (TyConMap, isTupleTyConLike, tyConDataCons)
import qualified Clash.Data.UniqMap as UniqMap

import Clash.Core.Scoped.FreeVars (hasFreeVars)
import Clash.Core.Pretty ()
import Clash.Core.Scoped.Subst (openTyBinder)
import Clash.Core.Scoped.HasType (inferCoreTypeOf)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (collectArgs)
import Clash.Core.Scoped.Type
  (SplitFunForall (..), TypeView (..), isFunTy, isPolyFunCoreTy, splitFunForallTy, tyView)
import Clash.Core.Scoped.TypeEnv (TypeEnv)

-- * Multi-result primitives

-- | A multi-result primitive, see 'multiPrimInfo'
data MultiPrimInfo = MultiPrimInfo
  { mpi_primInfo :: PrimInfo
  , mpi_resultDc :: DataCon
  -- ^ The data constructor of the tuple that the primitive returns
  , mpi_resultCount :: Int
  -- ^ The number of its results
  }

-- | The multi-result primitive, if the result type of the type of the
-- primitive (its 'primType') is a tuple.
multiPrimInfo :: TyConMap -> PrimInfo -> Maybe MultiPrimInfo
multiPrimInfo tcm p
  | SplitFunForall _ res <- splitFunForallTy (primType p)
  , TyConApp tupTcNm tupEls <- tyView res
    -- XXX: Hardcoded for tuples
  , isTupleTyConLike tupTcNm
  , Just tupTc <- UniqMap.lookup tupTcNm tcm
  , [tupDc] <- tyConDataCons tupTc
  = Just (MultiPrimInfo p tupDc (length tupEls))
  | otherwise
  = Nothing

-- | Same as 'multiPrimInfo', but errors if the primitive has no tuple result
multiPrimInfo' :: HasCallStack => TyConMap -> PrimInfo -> MultiPrimInfo
multiPrimInfo' tcm p = fromMaybe (error msg) (multiPrimInfo tcm p)
 where
  msg =
    "Internal error in multiPrimInfo': could not produce MultiPrimInfo. This "
      <> "probably means a multi result blackbox's result type was not a tuple. "
      <> "PrimInfo:\n\n" <> show p

-- | The types of the results of a multi-result primitive: the types of the
-- tuple that its 'primType' returns, in the scope of its quantified type
-- variables
withMultiPrimResultTypes
  :: forall r. HasCallStack
  => PrimInfo
  -> (forall l. Distinct l => Scope l -> [Type l] -> r)
  -> r
withMultiPrimResultTypes p k = go emptyScope (primType p)
 where
  go :: Distinct x => Scope x -> Type x -> r
  go sc ty = case ty of
    ForAllTy b res -> openTyBinder sc b res $ \sc' _ res' -> go sc' res'
    (tyView -> FunTy _ res) -> go sc res
    (tyView -> TyConApp _ tys) -> k sc tys
    _ -> error ("withMultiPrimResultTypes: not a tuple: " <> show (primType p))

-- * Measures

-- | The size of a term
termSize :: Term n -> Word
termSize = \case
  Var {} -> 1
  GlobalVar {} -> 1
  Data {} -> 1
  Literal {} -> 1
  Prim {} -> 1
  Lam _ e -> termSize e + 1
  TyLam _ e -> termSize e
  App e1 e2 -> termSize e1 + termSize e2
  TyApp e _ -> termSize e
  Cast e _ _ -> termSize e
  Tick _ e -> termSize e
  Let (NonRec _ x) e -> termSize x + termSize e
  Let (Rec _ xs) e -> sum (termSize e : map termSize xs)
  Case subj _ alts -> sum (termSize subj : [termSize a | Alt _ a <- alts])

-- | A term in some scope
data SomeTerm where
  SomeTerm :: Term n -> SomeTerm

-- | @termSizeSmallerThan n t@ is @'termSize' t < n@, but stops traversing @t@
-- as soon as the size reaches @n@
termSizeSmallerThan :: Word -> Term n -> Bool
termSizeSmallerThan n t0
  | n == 0 = False
  | otherwise = isJust (go (n - 1) [SomeTerm t0])
 where
  -- The remaining budget after spending the summed 'termSize' of the terms,
  -- or 'Nothing' if the budget doesn't cover it
  go :: Word -> [SomeTerm] -> Maybe Word
  go budget [] = Just budget
  go budget (SomeTerm t:rest) = case t of
    Var {} -> spend budget rest
    GlobalVar {} -> spend budget rest
    Data {} -> spend budget rest
    Literal {} -> spend budget rest
    Prim {} -> spend budget rest
    Lam _ e -> spend budget (SomeTerm e:rest)
    TyLam _ e -> go budget (SomeTerm e:rest)
    App e1 e2 -> go budget (SomeTerm e1:SomeTerm e2:rest)
    TyApp e _ -> go budget (SomeTerm e:rest)
    Cast e _ _ -> go budget (SomeTerm e:rest)
    Tick _ e -> go budget (SomeTerm e:rest)
    Let (NonRec _ x) e -> go budget (SomeTerm x:SomeTerm e:rest)
    Let (Rec _ xs) e -> go budget (map SomeTerm xs ++ SomeTerm e:rest)
    Case subj _ alts -> go budget (SomeTerm subj : [SomeTerm a | Alt _ a <- alts] ++ rest)

  spend 0 _ = Nothing
  spend budget rest = go (budget - 1) rest

-- * Predicates

-- | Does a term have a function type?
isFun :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> Term n -> Bool
isFun tenv sc t = isFunTy tenv sc (inferCoreTypeOf tenv sc t)

-- | Does a term have a function or polymorphic type?
isPolyFun :: (HasCallStack, Distinct n) => TypeEnv -> Scope n -> Term n -> Bool
isPolyFun tenv sc t = isPolyFunCoreTy tenv sc (inferCoreTypeOf tenv sc t)

-- | Is a term a let-expression?
isLet :: Term n -> Bool
isLet Let {} = True
isLet _ = False

-- | Is a term a local or global variable?
isVar :: Term n -> Bool
isVar Var {} = True
isVar GlobalVar {} = True
isVar _ = False

-- | Is a term a local variable?
isLocalVar :: Term n -> Bool
isLocalVar Var {} = True
isLocalVar _ = False

-- | Is a term a global variable?
isGlobalVar :: Term n -> Bool
isGlobalVar GlobalVar {} = True
isGlobalVar _ = False

-- | Is a term a data constructor?
isCon :: Term n -> Bool
isCon Data {} = True
isCon _ = False

-- | Is a term a primitive?
isPrim :: Term n -> Bool
isPrim Prim {} = True
isPrim _ = False

-- | Is a term a literal, or a data constructor or primitive applied to
-- literals and types?
isLiteral :: Term n -> Bool
isLiteral e = case collectArgs e of
  (Data _, args) -> all (either isLiteral (const True)) args
  (Prim _, args) -> all (either isLiteral (const True)) args
  (Literal _, _) -> True
  _ -> False

-- | Is a term a tick?
isTick :: Term n -> Bool
isTick Tick {} = True
isTick _ = False

-- | Is a term a cast?
isCast :: Term n -> Bool
isCast Cast {} = True
isCast _ = False

-- | Does a term have no free local variables, neither term nor type
-- variables? Global variables don't count.
isClosed :: Term n -> Bool
isClosed = not . hasFreeVars

-- * Multi-result primitives

-- | Split the arguments of a multi-result primitive in its normal arguments,
-- and the variables it assigns its results to. The latter are the arguments
-- that are local variables ('Var').
splitMultiPrimArgs
  :: MultiPrimInfo
  -> [Either (Term n) (Type n)]
  -> ([Either (Term n) (Type n)], [Term n])
splitMultiPrimArgs mpi args0 = (args1, [v | Left v@Var {} <- resArgs])
 where
  (args1, resArgs) = splitAt (length args0 - mpi_resultCount mpi) args0
