{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Helpers to test the scoped evaluator ("Clash.Core.Scoped.Evaluator") on
  terms of scoped Clash Core.

  = Results

  The evaluator gives its result bound by the bindings it allocated
  ('Clash.Core.Scoped.Evaluator.bindAllocated'). Tests compare:

  * the result itself, up to alpha-equivalence;
  * its body, if it doesn't refer to the bindings ('allocatedBody');
  * or the value it denotes ('assertSameValue'): the result with the bindings
    unfolded into the body ('unfoldAllocated'), and the types of the
    coercions in it dropped ('dropCoercionTypes'). Reductions build the
    coercion arguments of e.g. @Cons@ with types of their own.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Test.Clash.Scoped.Evaluator
  ( -- * Evaluating
    whnfIn
  , nfDataIn
    -- * Results
  , allocatedBody
  , unfoldAllocated
  , dropCoercionTypes
  , sameValue
  , assertSameValue
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit (Assertion, assertBool)

import Clash.Driver.Types (Binding)

import Clash.Core.Scoped.Alpha (aeqTm)
import Clash.Core.Scoped.Evaluator (bindAllocated, nfData, whnf)
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.FreeVars (anyBoundOccursIn, bindNameBinders)
import Clash.Core.Scoped.NameMap (GlobalEnv, emptyNameMap, nameMapFromList)
import Clash.Core.Pretty ()
import Clash.Core.Scoped.Subst (openBind, substTm, teleIdVars, tmVarsSubst, unsinkTerm)
import Clash.Core.Scoped.Syntax

import Test.Clash.Rewrite (showPprU)

-- * Evaluating

-- | Evaluate a term in a scope to WHNF, with the given environment and global
-- bindings, and whether it is the subject of a case-expression. Gives the
-- result, bound by the bindings the evaluator allocated. E.g. in a test scope
-- ('Test.Clash.Scoped.TestScope'):
--
-- > withTerm "(case 1 of { 1 -> (y :: Int); _ -> 2 }) :: Int" $ \ts e ->
-- >   assertTermIs ts "y" (whnfIn env emptyGlobalEnv False (testScope ts) e)
whnfIn
  :: Distinct n
  => Env -> GlobalEnv (Binding (Term 'VoidS)) -> Bool -> Scope n -> Term n -> Term n
whnfIn env bs isSubj = evalIn (\m0 k -> whnf env isSubj m0 k) bs

-- | Evaluate a term in a scope as far as it is data ('nfData'), see 'whnfIn'
nfDataIn
  :: Distinct n
  => Env -> GlobalEnv (Binding (Term 'VoidS)) -> Scope n -> Term n -> Term n
nfDataIn env = evalIn (nfData env)

-- | Run the evaluator of scoped Clash Core on a term in a scope, see 'whnfIn'
evalIn
  :: Distinct n
  => (forall r. Machine n n -> (forall l. DExt n l => Machine n l -> r) -> r)
  -> GlobalEnv (Binding (Term 'VoidS))
  -> Scope n
  -> Term n
  -> Term n
evalIn eval bs sc t =
  eval (initMachine sc emptyNameMap bs (IntMap.empty, 0) t) $ \m -> bindAllocated m (mTerm m)

-- * Results

-- | The body of the bindings the evaluator allocated around a result, if it
-- doesn't refer to them. Otherwise the result itself.
allocatedBody :: Term n -> Term n
allocatedBody = \case
  t@(Let b e) -> fromMaybe t (unsinkTerm (bindNameBinders b) e)
  t -> t

-- | Unfold the bindings the evaluator allocated around a result: substitute
-- their right-hand sides for them in the body, as long as it refers to them,
-- up to the given number of times. The bindings are dropped if the body no
-- longer refers to them.
unfoldAllocated :: Distinct n => Scope n -> Int -> Term n -> Term n
unfoldAllocated sc depth = \case
  Let b0 e0 -> openBind sc b0 e0 $ \sc' b e ->
    let bs = bindNameBinders b
        s = tmVarsSubst sc' (nameMapFromList (bindings b))
        go i x
          | i > 0 && anyBoundOccursIn bs x = go (i - 1) (substTm s x)
          | otherwise = x
        e' = go depth e
    in fromMaybe (Let b e') (unsinkTerm bs e')
  t -> t
 where
  bindings :: DExt n l => Bind n l -> [(TmName l, Term l)]
  bindings = \case
    NonRec b x -> [(tmNameOf b, sink x)]
    Rec tele xs -> [(x, rhs) | (Var _ _ x, rhs) <- zip (teleIdVars tele) xs]

-- | Drop the types of the coercion arguments, applications of the primitive
-- @_CO_@ to their type (see 'Clash.Core.Scoped.Util.primCo')
dropCoercionTypes :: Term n -> Term n
dropCoercionTypes = \case
  TyApp (Prim p) _ | primName p == "_CO_" -> Prim p
  Lam b e -> Lam b (dropCoercionTypes e)
  TyLam b e -> TyLam b (dropCoercionTypes e)
  App e1 e2 -> App (dropCoercionTypes e1) (dropCoercionTypes e2)
  TyApp e ty -> TyApp (dropCoercionTypes e) ty
  Let (NonRec b x) e -> Let (NonRec b (dropCoercionTypes x)) (dropCoercionTypes e)
  Let (Rec bs xs) e -> Let (Rec bs (map dropCoercionTypes xs)) (dropCoercionTypes e)
  Case e ty alts -> Case (dropCoercionTypes e) ty [Alt p (dropCoercionTypes a) | Alt p a <- alts]
  Cast e t1 t2 -> Cast (dropCoercionTypes e) t1 t2
  Tick t e -> Tick t (dropCoercionTypes e)
  e -> e

-- | Whether two results of the evaluator are the same value: equal up to
-- alpha-equivalence after unfolding the bindings around them twice
-- ('unfoldAllocated') and dropping the types of coercions
-- ('dropCoercionTypes')
sameValue :: Distinct n => Scope n -> Term n -> Term n -> Bool
sameValue sc t1 t2 = aeqTm (norm t1) (norm t2)
 where
  norm = dropCoercionTypes . unfoldAllocated sc 2

-- | Assert that a result of the evaluator is the given value, see 'sameValue'
assertSameValue :: (HasCallStack, Distinct n) => Scope n -> Term n -> Term n -> Assertion
assertSameValue sc expected actual =
  assertBool
    ("Expected (as a value):\n" <> showPprU expected <> "\nbut got:\n" <> showPprU actual)
    (sameValue sc expected actual)
