{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.Scoped.ViaFoil": running scoped transformations
  inside a traversal of Clash Core, and evaluating scoped terms with the
  evaluator of Clash Core.
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
  (Evaluator (..), Machine (..), heapInsert, setTerm)
import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (..))
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as CT
import qualified Clash.Core.Var as C
import Clash.Core.VarEnv (extendInScopeSet, uniqAway)
import Clash.Driver.Types (DebugOpts (..), debugSilent)
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), TransformContext (..))
import Clash.Unique (Unique)
import Clash.Util.Supply (freshId)

import Clash.Core.Scoped.Subst (freshen)
import Clash.Core.Scoped.Syntax
import Clash.Rewrite.Scoped
  (ScopedRewrite, ScopedTransform (..), changed, tfScope)
import qualified Clash.Rewrite.Scoped as S
import Clash.Rewrite.Scoped.ViaFoil (rootVars)

import Test.Clash.Rewrite
  ( assertAlphaEq, assertErrorContainsIO, firstUniques, globalIntVar
  , heapLookupEvaluator, inScopeOf, intId, intLit, intTy, letBinders
  , pairDataCon, rewriteEnvWithDebug, tyVar, localId )
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

-- * whnfRW

-- | Reduce a term to WHNF with the 'S.whnfRW' adapter, in a Clash Core
-- context given innermost frame first, with the given variables in scope.
-- See @whnfInContext@ in "Clash.Tests.Rewrite.Util".
whnfInContext :: Evaluator -> [C.Id] -> C.Context -> C.Term -> IO C.Term
whnfInContext eval inScope ctx e = do
  (t, _, _) <- runViaFoil env st (TransformContext (inScopeOf inScope) ctx) rw e
  pure t
 where
  env = invariantsEnv{_evaluator = eval}
  st :: RewriteState NormalizeState
  st = def{_globalHeap = (IntMap.empty, 0)}
  rw = ScopedTransform $ \ctx' t -> S.whnfRW False ctx' t (\_ v -> changed v)

-- | A global that the evaluator of 'allocEvaluator' allocates its argument
-- for
allocG :: C.Id
allocG = C.mkGlobalId intTy (C.varName (intId User "alloc" 20))

-- | Like 'heapLookupEvaluator', but steps @alloc e@ to @f h@, where @h@ is a
-- new heap binding of @e@
allocEvaluator :: Evaluator
allocEvaluator = heapLookupEvaluator
  { step = \m tcm -> case mTerm m of
      C.App (C.Var g) arg | C.varUniq g == C.varUniq allocG ->
        let (u, sup) = freshId (mSupply m)
            h = uniqAway (mScopeNames m) (intId System "h" u)
            m1 = m{mSupply = sup, mScopeNames = extendInScopeSet (mScopeNames m) h}
        in Just (setTerm (C.App (globalIntVar User "f" 4) (C.Var h))
                   (heapInsert C.LocalId h arg m1))
      _ -> step heapLookupEvaluator m tcm
  }

-- | A primitive that is never work-free
workPrim :: C.PrimInfo
workPrim = somePrim{C.primWorkInfo = C.WorkAlways, C.primType = intTy}

whnfTests :: [TestTree]
whnfTests =
  -- The cases of "whnfRW uses the context's let-bindings (#2571)" in
  -- "Clash.Tests.Rewrite.Util"
  [ testCase "let x = 3 in \\y -> x" $
      whnfInContext heapLookupEvaluator [x, y] [C.LamBody y, C.LetBody [(x, intLit 3)]] (C.Var x)
        >>= assertAlphaEq (intLit 3)
  , testCase "let x = 3 in \\x -> x" $
      whnfInContext heapLookupEvaluator [x] [C.LamBody x, C.LetBody [(x, intLit 3)]] (C.Var x)
        >>= assertAlphaEq (C.Var x)
  , testCase "let x = y in \\y -> x" $
      whnfInContext heapLookupEvaluator [x, y] [C.LamBody y, C.LetBody [(x, C.Var y)]] (C.Var x)
        >>= assertAlphaEq (C.Var x)
  , testCase "let x = y in case s of MkPair y z -> x" $
      whnfInContext heapLookupEvaluator [x, y, z, s]
        [ C.CaseAlt (C.DataPat pairDataCon [] [y, z])
        , C.LetBody [(x, C.Var y)] ]
        (C.Var x)
        >>= assertAlphaEq (C.Var x)
  , testCase "let x = 3 in letrec { x = 4; w = x } in .." $
      whnfInContext heapLookupEvaluator [x, w]
        [C.LetBinding w [x, w], C.LetBody [(x, intLit 3)]] (C.Var x)
        >>= assertAlphaEq (C.Var x)
  , testCase "let x = y in let y = 5 in x" $
      whnfInContext heapLookupEvaluator [x, y]
        [C.LetBody [(y, intLit 5)], C.LetBody [(x, C.Var y)]] (C.Var x)
        >>= assertAlphaEq (C.Var x)
  , testCase "let x = (v :: a) in /\\a -> x" $
      whnfInContext heapLookupEvaluator [x, v]
        [C.TyLamBody a, C.LetBody [(x, C.Var v)]] (C.Var x)
        >>= assertAlphaEq (C.Var x)

    -- The result mentions a variable that is not free in the term
  , testCase "\\y -> let x = y in x" $
      whnfInContext heapLookupEvaluator [x, y] [C.LetBody [(x, C.Var y)], C.LamBody y] (C.Var x)
        >>= assertAlphaEq (C.Var y)

    -- The scoped part of the context: the transformation goes under the let
    -- and the lambda itself
  , testCase "let-bindings of the scoped context are used" $ do
      -- let x = 3 in \y -> x
      let tm = C.Let (C.NonRec x (intLit 3)) (C.Lam y (C.Var x))
          onVars = ScopedTransform $ \ctx -> \case
            e@(Var {}) -> S.whnfRW False ctx e $ \_ r -> case r of
              Literal _ -> changed r
              _ -> pure r
            e -> pure e
      (res, _, ch) <- runViaFoil invariantsEnv{_evaluator = heapLookupEvaluator}
        (def{_globalHeap = (IntMap.empty, 0)} :: RewriteState NormalizeState)
        (TransformContext (inScopeOf []) []) (S.topdownR onVars) tm
      ch @?= True
      assertAlphaEq (C.Let (C.NonRec x (intLit 3)) (C.Lam y (intLit 3))) res

  , testCase "the bindings the evaluator allocates are bound around the result" $ do
      let e = C.App (C.Var allocG) (C.Prim workPrim)
      res <- whnfInContext allocEvaluator [] [] e
      case res of
        C.Let (C.Rec [(h, C.Prim p)]) (C.App _ (C.Var h'))
          | h == h', C.primName p == C.primName workPrim -> pure ()
        _ -> assertFailure ("unexpected result: " <> show res)
      -- Allocated bindings are fresh in the context
      assertBool "fresh" (all (`notElem` map C.varUniq [x, y]) (map C.varUniq (letBinders res)))

  , testCase "without a change, the allocated bindings are dropped" $ do
      let e = C.App (C.Var allocG) (C.Prim workPrim)
          keep = ScopedTransform $ \ctx t -> S.whnfRW False ctx t (\_ _ -> pure (sink t))
      (res, _, ch) <- runViaFoil invariantsEnv{_evaluator = allocEvaluator}
        (def{_globalHeap = (IntMap.empty, 0)} :: RewriteState NormalizeState)
        (TransformContext (inScopeOf []) []) keep e
      ch @?= False
      show res @?= show e
  ]
 where
  x = intId User "x" 1
  y = intId User "y" 2
  z = intId User "z" 3
  s = intId User "s" 4
  w = intId User "w" 5
  a = tyVar User "a" 6
  v = localId User "v" 7 (CT.VarTy a)

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.Scoped.ViaFoil"
  [ testGroup "viaFoil" viaFoilTests
  , testGroup "whnfRW" whnfTests
  ]
