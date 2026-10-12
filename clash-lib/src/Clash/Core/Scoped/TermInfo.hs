{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Predicates on and measures of scoped Clash Core terms: ports of
  "Clash.Core.TermInfo".

  Note that 'isVar' holds for local and global variables alike, like its
  counterpart on Clash Core, while 'isLocalVar' and 'isGlobalVar' tell them
  apart.

  The functions on multi-result primitives that only look at a
  'C.PrimInfo' are the ones of "Clash.Core.TermInfo": primitives are not
  scoped.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

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
  , C.MultiPrimInfo (..)
  , C.multiPrimInfo
  , C.multiPrimInfo'
  , splitMultiPrimArgs
  ) where

import Data.Maybe (isJust)
import GHC.Stack (HasCallStack)

import qualified Clash.Core.Term as C
import qualified Clash.Core.TermInfo as C

import Clash.Core.Scoped.FreeVars (hasFreeVars)
import Clash.Core.Scoped.HasType (inferCoreTypeOf)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (collectArgs)
import Clash.Core.Scoped.Type (isFunTy, isPolyFunCoreTy)
import Clash.Core.Scoped.TypeEnv (TypeEnv)

-- * Measures

-- | The size of a term, see 'C.termSize'
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
-- as soon as the size reaches @n@, see 'C.termSizeSmallerThan'
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
-- literals and types? See 'Clash.Netlist.BlackBox.isLiteral'.
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
-- and the variables it assigns its results to, see 'C.splitMultiPrimArgs'.
-- The latter are the arguments that are local variables ('Var').
splitMultiPrimArgs
  :: C.MultiPrimInfo
  -> [Either (Term n) (Type n)]
  -> ([Either (Term n) (Type n)], [Term n])
splitMultiPrimArgs mpi args0 = (args1, [v | Left v@Var {} <- resArgs])
 where
  (args1, resArgs) = splitAt (length args0 - length (C.mpi_resultTypes mpi)) args0
