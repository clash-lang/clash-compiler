{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.Scoped.ViaFoil": running scoped transformations
  inside a traversal of Clash Core and the reverse, and evaluating scoped terms
  with the evaluator of Clash Core.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Rewrite.Scoped.ViaFoil (tests) where

import qualified Control.Lens as Lens
import Data.Default (def)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as List
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.FreeVars as CF
import Clash.Core.Evaluator.Types
  (Evaluator (..))
import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (..))
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as CT
import qualified Clash.Core.Var as C
import Clash.Driver.Types (DebugOpts (..), debugSilent)
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), TransformContext (..))
import Clash.Unique (Unique)

import Clash.Core.Scoped.Convert (fromClashTermClosed)
import Clash.Core.Scoped.Subst (freshen)
import Clash.Core.Scoped.Syntax
import Clash.Rewrite.Scoped
  (ScopedRewrite, ScopedTransform (..), changed, tfScope)
import qualified Clash.Rewrite.Scoped as S
import Clash.Rewrite.Scoped.Globals (toClashTermFresh)
import Test.Clash.Scoped.ViaFoil (rootVars)

import Clash.Tests.Rewrite.Util (whnfContextTests)
import Test.Clash.Rewrite
  ( assertAlphaEq, assertErrorContainsIO, firstUniques, globalIntVar
  , heapLookupEvaluator, inScopeOf, intId, intLit, intTy, letBinders
  , pairDataCon, rewriteEnvWithDebug, runRewriteTest )
import Test.Clash.Scoped (runViaFoil, somePrim)

-- * Running scoped transformations

-- | Debug options that enable the invariant checks
invariantsEnv :: RewriteEnv
invariantsEnv = rewriteEnvWithDebug debugSilent{dbg_invariants = True}

-- | A transformation that changes nothing
idT :: ScopedRewrite extra
idT = ScopedTransform (\_ e -> pure e)

-- | A transformation that renames every binder, and signals a change
renameT :: ScopedRewrite extra
renameT = ScopedTransform (\ctx e -> changed (freshen (tfScope ctx) e))

-- | Changes literals without signalling it
sneakyT :: ScopedRewrite extra
sneakyT = ScopedTransform $ \_ -> \case
  Literal (IntLiteral i) -> pure (Literal (IntLiteral (i + 1)))
  e -> pure e

x1, y2, z3 :: C.Id
x1 = intId User "x" 1
y2 = intId User "y" 2
z3 = intId User "z" 3

-- | @\\x -> let y = f x z in case y of { 1 -> x; _ -> y }@, with @z@ free
term :: C.Term
term =
  C.Lam x1
    (C.Let (C.NonRec y2 (C.mkApps (globalIntVar User "f" 4) [Left (C.Var x1), Left (C.Var z3)]))
      (C.Case (C.Var y2) intTy
        [(C.LitPat (IntLiteral 1), C.Var x1), (C.DefaultPat, C.Var y2)]))

-- | The context of 'term': @z@ is in scope
termCtx :: TransformContext
termCtx = TransformContext (inScopeOf [z3]) [C.LamBody z3]

-- | The uniques of all binders of a term
binderUniques :: C.Term -> [Unique]
binderUniques = map C.varUniq . allBinders
 where
  allBinders = \case
    C.Lam v e -> v : allBinders e
    C.TyLam _ e -> allBinders e
    C.App e1 e2 -> allBinders e1 <> allBinders e2
    C.TyApp e _ -> allBinders e
    C.Let (C.NonRec v x) e -> v : allBinders x <> allBinders e
    C.Let (C.Rec xs) e -> map fst xs <> concatMap (allBinders . snd) xs <> allBinders e
    C.Case e _ alts -> allBinders e <> concat [snd (C.patIds p) <> allBinders a | (p, a) <- alts]
    C.Cast e _ _ -> allBinders e
    C.Tick _ e -> allBinders e
    _ -> []

viaFoilTests :: [TestTree]
viaFoilTests =
  [ testCase "an identity transformation changes nothing and signals nothing" $ do
      (res, st, ch) <- runViaFoil invariantsEnv (def :: RewriteState NormalizeState) termCtx idT term
      -- The original term, not a converted copy
      show res @?= show term
      ch @?= False
      _transformCounter st @?= 0

  , testCase "a renaming transformation gives binders fresh uniques" $ do
      let st0 :: RewriteState NormalizeState
          st0 = def
      (res, st1, ch) <- runViaFoil invariantsEnv st0 termCtx renameT term
      ch @?= True
      assertAlphaEq term res
      let uniques = binderUniques res
          -- The uniques the supply hands out after the transformation
          later = firstUniques 10000 (_uniqSupply st1)
      assertBool "binders are distinct" (List.nub uniques == uniques)
      assertBool ("binders collide with the in-scope set: " <> show uniques)
        (C.varUniq z3 `notElem` uniques)
      assertBool "binders collide with the supply"
        (null (uniques `List.intersect` later))
      -- The free variable is the original one
      assertBool "free variable kept" (z3 `elem` Lens.toListOf CF.freeLocalIds res)

  , testCase "an unsignalled change is reported" $
      assertErrorContainsIO "Expression changed without notice" $
        (\(t, _, _) -> t) <$> runViaFoil invariantsEnv (def :: RewriteState NormalizeState) termCtx
          (S.topdownR sneakyT) (C.App (globalIntVar User "f" 4) (intLit 1))

  , testCase "rootVars adds the free variables of the context's let-bindings" $ do
      -- let x = y in let w = z in \u -> x
      let w = intId User "w" 5
          u = intId User "u" 6
          (_, ids) = rootVars
            [C.LamBody u, C.LetBody [(w, C.Var z3)], C.LetBody [(x1, C.Var y2)]]
            (C.Var x1)
      List.sort (map C.varUniq ids) @?= List.sort (map C.varUniq [x1, y2])
  ]

-- * toClashTermFresh

toClashTermFreshTests :: [TestTree]
toClashTermFreshTests =
  [ testCase "toClashTermFresh gives binders fresh uniques" $ do
      let closed = C.Lam y2 (C.Lam x1 (C.Var x1))
      (res, st, _) <- runRewriteTest def (def :: RewriteState NormalizeState)
        (toClashTermFresh (fromClashTermClosed closed))
      assertAlphaEq closed res
      let uniques = binderUniques res
          later = firstUniques 10000 (_uniqSupply st)
      assertBool "binders are distinct" (List.nub uniques == uniques)
      assertBool "binders collide with the supply"
        (null (uniques `List.intersect` later))
  ]

-- * whnfRW

-- | Reduce a term to WHNF, in a Clash Core context given innermost frame
-- first, with the given variables in scope. See @whnfInContext@ in
-- "Clash.Tests.Rewrite.Util".
whnfInContext :: Evaluator -> [C.Id] -> C.Context -> C.Term -> IO C.Term
whnfInContext eval inScope ctx e = do
  (t, _, _) <- runViaFoil env st (TransformContext (inScopeOf inScope) ctx) rw e
  pure t
 where
  env = invariantsEnv{_evaluator = eval}
  st :: RewriteState NormalizeState
  st = def{_globalHeap = (IntMap.empty, 0)}
  rw = ScopedTransform $ \ctx' t -> S.whnfRW False ctx' t (\_ v -> changed v)

-- | A primitive that is never work-free
workPrim :: C.PrimInfo
workPrim = somePrim{C.primWorkInfo = C.WorkAlways, C.primType = intTy}

-- | @(\\z -> MkPair z z) workPrim@: the evaluator allocates the argument,
-- and the result, a constructor application, refers to it
allocTerm :: C.Term
allocTerm =
  C.App (C.Lam zA (C.mkApps (C.Data pairDataCon) [Left (C.Var zA), Left (C.Var zA)]))
        (C.Prim workPrim)
 where
  zA = intId User "z" 30

-- | The cases of "whnfRW uses the context's let-bindings (#2571)" in
-- "Clash.Tests.Rewrite.Util", in a Clash Core context
contextTests :: [TestTree]
contextTests = whnfContextTests (whnfInContext heapLookupEvaluator)

whnfTests :: [TestTree]
whnfTests =
  [ testGroup "the scoped evaluator" $ contextTests <>
      -- The result mentions a variable that is not free in the term. Like the
      -- evaluator of Clash Core, the scoped one names the result after the
      -- variable it looked up.
      [ testCase "\\y -> let x = y in x" $
          whnfInContext heapLookupEvaluator [x, y]
            [C.LetBody [(x, C.Var y)], C.LamBody y] (C.Var x)
            >>= assertAlphaEq
                  (C.Tick (C.NameMod C.PrefixName (CT.LitTy (CT.SymTy "x_"))) (C.Var y))

        -- The scoped part of the context: the transformation goes under the
        -- let and the lambda itself
      , testCase "let-bindings of the scoped context are used" $ do
          -- let x = 3 in \y -> x
          let tm = C.Let (C.NonRec x (intLit 3)) (C.Lam y (C.Var x))
              onVars = ScopedTransform $ \ctx -> \case
                e@(Var {}) -> S.whnfRW False ctx e $ \_ r -> case r of
                  Literal _ -> changed r
                  _ -> pure (sink e)
                e -> pure e
          (res, _, ch) <- runViaFoil invariantsEnv{_evaluator = heapLookupEvaluator}
            (def{_globalHeap = (IntMap.empty, 0)} :: RewriteState NormalizeState)
            (TransformContext (inScopeOf []) []) (S.topdownR onVars) tm
          ch @?= True
          assertAlphaEq (C.Let (C.NonRec x (intLit 3)) (C.Lam y (intLit 3))) res

      , testCase "the bindings the evaluator allocates are bound around the result" $ do
          res <- whnfInContext heapLookupEvaluator [x, y] [] allocTerm
          case res of
            C.Letrec [(h, C.Prim p)] (C.App (C.App (C.Data dc) (C.Var h1)) (C.Var h2))
              | h == h1, h == h2, C.primName p == C.primName workPrim, dc == pairDataCon
              -> pure ()
            _ -> assertFailure ("unexpected result: " <> show res)
          -- Allocated bindings are fresh in the context
          assertBool "fresh" (all (`notElem` map C.varUniq [x, y]) (map C.varUniq (letBinders res)))

      , testCase "without a change, the allocated bindings are dropped" $ do
          let keep = ScopedTransform $ \ctx t -> S.whnfRW False ctx t (\_ _ -> pure (sink t))
          (res, _, ch) <- runViaFoil invariantsEnv{_evaluator = heapLookupEvaluator}
            (def{_globalHeap = (IntMap.empty, 0)} :: RewriteState NormalizeState)
            (TransformContext (inScopeOf []) []) keep allocTerm
          ch @?= False
          show res @?= show allocTerm
      ]
  ]
 where
  x = intId User "x" 1
  y = intId User "y" 2

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.Scoped.ViaFoil"
  [ testGroup "viaFoil" viaFoilTests
  , testGroup "toClashTermFresh" toClashTermFreshTests
  , testGroup "whnfRW" whnfTests
  ]
