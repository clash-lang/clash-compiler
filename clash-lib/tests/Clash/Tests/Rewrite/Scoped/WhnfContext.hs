{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  The cases of \"whnfRW uses the context's let-bindings (#2571)\":
  'Clash.Rewrite.Scoped.whnfRW' evaluates a variable with the
  let-bindings of its context, which a test builds by going under the binders
  of a term to the variable.

  In Clash Core, a binder that shadows a let-binder, or a free variable of its
  right-hand side, makes the binding unusable, and the variable stays as it is.
  In scoped Core, the shadowing binder has another name, so the binding is
  used where it is valid: the variable evaluates to the free variable of the
  right-hand side, not to the binder in between that has its name in the
  source.
  The evaluator marks a variable it looked up with a name modifier. A
  recursive binding is not usable in the right-hand sides of its group.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Rewrite.Scoped.WhnfContext (tests) where

import Data.Default (def)
import qualified Data.IntMap.Strict as IntMap
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Scoped (ScopedRewrite, ScopedTransform (..), changed)
import qualified Clash.Rewrite.Scoped as S
import Clash.Rewrite.Types (RewriteEnv (..), RewriteState (..))

import Test.Clash.Rewrite (TestTerm, heapLookupEvaluator)
import Test.Clash.Scoped
  ( ScopeDecl, Step (..), assertLintClean, assertTermIs, runScopedAt, termIn, tyVarD, varD
  , withBinderClashes, withTestScope )

-- | Replace a term by its WHNF
whnfS :: ScopedRewrite NormalizeState
whnfS = ScopedTransform $ \ctx t -> S.whnfRW False ctx t (\_ v -> changed v)

-- | Assert that evaluating the variable at a path of a term gives the
-- expected term, also in a scope that the binders of the term clash with
whnfAt :: [Step] -> [ScopeDecl] -> TestTerm -> TestTerm -> Assertion
whnfAt path decls input expected = withTestScope decls $ \ts0 -> do
  let e0 = termIn ts0 input
  check ts0 e0
  withBinderClashes ts0 e0 check
 where
  check ts e = do
    (e', _, _) <- runScopedAt env st ts path whnfS e
    assertLintClean ts e'
    assertTermIs ts expected e'
  env = def{_evaluator = heapLookupEvaluator}
  st :: RewriteState NormalizeState
  st = def{_globalHeap = (IntMap.empty, 0)}

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.Scoped.WhnfContext"
  [ testCase "let x = 3 in \\y -> x" $
      whnfAt [LetBodyStep, LamStep] []
        "let { (x :: Int) = 3 } in \\(y :: Int) -> x"
        "let { (x :: Int) = 3 } in \\(y :: Int) -> 3"
  , testCase "let x = 3 in \\x -> x" $
      whnfAt [LetBodyStep, LamStep] []
        "let { (x :: Int) = 3 } in \\(x :: Int) -> x"
        "let { (x :: Int) = 3 } in \\(x :: Int) -> x"
  , testCase "let x = y in \\y -> x" $
      whnfAt [LetBodyStep, LamStep] [varD "y" "Int"]
        "let { (x :: Int) = y } in \\(y :: Int) -> x"
        "let { (x :: Int) = y } in \\(y2 :: Int) -> Core.prefixName @\"x_\" y"
  , testCase "let x = y in case s of MkPair y z -> x" $
      whnfAt [LetBodyStep, AltStep 0] [varD "y" "Int", varD "s" "Pair"]
        "let { (x :: Int) = y } in (case s of { MkPair (y :: Int) (z :: Int) -> x }) :: Int"
        "let { (x :: Int) = y } in (case s of { MkPair (y2 :: Int) (z :: Int) -> Core.prefixName @\"x_\" y }) :: Int"
  , testCase "let x = 3 in letrec { x = 4; w = x } in .." $
      whnfAt [LetBodyStep, LetRhsStep 1] []
        "let { (x :: Int) = 3 } in let { (x :: Int) = 4; (w :: Int) = x } in w"
        "let { (x :: Int) = 3 } in let { (x :: Int) = 4; (w :: Int) = x } in w"
  , testCase "let x = y in let y = 5 in x" $
      whnfAt [LetBodyStep, LetBodyStep] [varD "y" "Int"]
        "let { (x :: Int) = y } in let { (y :: Int) = 5 } in x"
        "let { (x :: Int) = y } in let { (y2 :: Int) = 5 } in Core.prefixName @\"x_\" y"
  , testCase "let x = (v :: a) in /\\a -> x" $
      whnfAt [LetBodyStep, TyLamStep] [tyVarD "a", varD "v" "a"]
        "let { (x :: a) = v } in \\(a :: Type) -> x"
        "let { (x :: a) = v } in \\(b :: Type) -> Core.prefixName @\"x_\" v"
  ]
