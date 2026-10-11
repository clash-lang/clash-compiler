{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Helpers to test the scoped evaluator ("Clash.Core.Scoped.Evaluator")
  against the evaluator of Clash Core. Shared by the unit tests and
  @clash-differential@.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Clash.Scoped.Evaluator
  ( -- * Evaluating
    scopedWhnf
    -- * Comparing results
  , readback
  , canonicalCoercions
  , termSize
  ) where

import qualified Control.Lens as Lens
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List

import qualified Clash.Core.FreeVars as C
import qualified Clash.Core.Subst as C
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C
import qualified Clash.Core.VarEnv as C
import Clash.Driver.Types (BindingMap)

import Clash.Core.Scoped.Convert (convScope, fromClashTerm, toClashTerm, withConvEnv)
import Clash.Core.Scoped.Evaluator (bindAllocated, whnf)
import Clash.Core.Scoped.Evaluator.Types
import Clash.Core.Scoped.NameMap (emptyNameMap)

-- * Evaluating

-- | Evaluate a Clash term to WHNF with the evaluator of scoped Clash Core, in
-- the scope of its free variables, with the given environment and global
-- bindings. Gives the result, bound by the bindings the evaluator allocated,
-- as Clash Core: its free variables keep their uniques.
scopedWhnf :: Env -> BindingMap -> Bool -> C.Term -> C.Term
scopedWhnf env bm isSubj t = withConvEnv tvs ids $ \cenv ->
  let m0 = initMachine (convScope cenv) emptyNameMap bm (IntMap.empty, 0) (fromClashTerm cenv t)
  in whnf env isSubj m0 $ \m -> toClashTerm (bindAllocated m (mTerm m))
 where
  tvs = C.dependencySortTyVars (List.nubBy sameUniq (Lens.toListOf C.termFreeTyVars t))
  ids = List.nubBy sameUniq (Lens.toListOf C.freeLocalIds t)
  sameUniq :: C.Var a -> C.Var a -> Bool
  sameUniq v1 v2 = C.varUniq v1 == C.varUniq v2

-- * Comparing results

-- | Unfold the top-level @letrec@ of a result a number of times, and replace
-- the variables it binds that remain by a placeholder. The evaluators name
-- and order the bindings they allocate differently, and the evaluator of
-- Clash Core keeps the value a binding was updated with, where the scoped one
-- may keep the right-hand side it allocated.
readback :: Int -> C.Term -> C.Term
readback depth t0 = case t0 of
  C.Letrec bs body ->
    let env = C.mkVarEnv bs
        fvs = C.mkInScopeSet (Lens.foldMapOf C.freeLocalVars C.unitVarSet t0 <> C.mkVarSet (map fst bs))
        unfold = C.substTm "readback" (C.extendIdSubstList (C.mkSubst fvs) bs)
        cut = C.substTm "readback" (C.extendIdSubstList (C.mkSubst fvs) [(v, cutPrim) | (v, _) <- bs])
        go 0 e = cut e
        go i e
          | not (any (`C.elemVarEnv` env) (Lens.toListOf C.freeLocalIds e)) = e
          | termSize e > 200000 = cut e
          | otherwise = go (i - 1 :: Int) (unfold e)
    in go depth body
  _ -> t0
 where
  cutPrim = C.Prim (C.PrimInfo "<heap>" (C.ConstTy C.Arrow) C.WorkNever C.SingleResult C.NoUnfolding)

-- | Make the coercion arguments of data constructors the same: those that the
-- scoped reductions build are applications of @_CO_@ to their type (see
-- 'Clash.Core.Scoped.Util.primCo'), those of Clash Core have the type in the
-- primitive.
canonicalCoercions :: C.Term -> C.Term
canonicalCoercions = go
 where
  go = \case
    C.TyApp (C.Prim p) _ | C.primName p == "_CO_" -> coPrim
    C.Prim p | C.primName p == "_CO_" -> coPrim
    C.Lam v e -> C.Lam v (go e)
    C.TyLam v e -> C.TyLam v (go e)
    C.App e1 e2 -> C.App (go e1) (go e2)
    C.TyApp e ty -> C.TyApp (go e) ty
    C.Let (C.NonRec v x) e -> C.Let (C.NonRec v (go x)) (go e)
    C.Let (C.Rec xs) e -> C.Let (C.Rec [(v, go x) | (v, x) <- xs]) (go e)
    C.Case e ty alts -> C.Case (go e) ty [(p, go a) | (p, a) <- alts]
    C.Cast e t1 t2 -> C.Cast (go e) t1 t2
    C.Tick t e -> C.Tick t (go e)
    e -> e
  coPrim = C.Prim (C.PrimInfo "_CO_" (C.ConstTy C.Arrow) C.WorkNever C.SingleResult C.NoUnfolding)

-- | Number of term nodes (forces the term, not the types)
termSize :: C.Term -> Int
termSize = go 0
 where
  go !acc = \case
    C.Lam _ e -> go (acc + 1) e
    C.TyLam _ e -> go (acc + 1) e
    C.App e1 e2 -> go (go (acc + 1) e1) e2
    C.TyApp e _ -> go (acc + 1) e
    C.Let (C.NonRec _ x) e -> go (go (acc + 1) x) e
    C.Let (C.Rec xs) e -> go (List.foldl' go (acc + 1) (map snd xs)) e
    C.Case e _ alts -> List.foldl' go (go (acc + 1) e) (map snd alts)
    C.Cast e _ _ -> go (acc + 1) e
    C.Tick _ e -> go (acc + 1) e
    _ -> acc + 1
