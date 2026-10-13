{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.Scoped.Whnf": evaluating subterms with the
  let-bindings of their scoped context.

  The cases of \"whnfRW uses the context's let-bindings (#2571)\": a
  traversal goes under the binders itself, so the context is built by
  'Clash.Rewrite.Scoped.pushUnder', and evaluates every variable. A binder
  that shadows a let-binder, or a free variable of its right-hand side, in
  the source of a test has another name in scoped Core, so the binding is
  used where it is valid, and nowhere else.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Rewrite.Scoped.Whnf (tests) where

import Data.Default (def)
import qualified Data.IntMap.Strict as IntMap
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Types (RewriteState (..))

import Clash.Core.Scoped.Syntax
import Clash.Rewrite.Scoped (ScopedRewrite, ScopedTransform (..), changed)
import qualified Clash.Rewrite.Scoped as S

import Test.Clash.Scoped (TestTerm, assertTermIs, runScopedIn, withTerm)

-- | Replace every variable that evaluates to a literal by that literal
evalVars :: ScopedRewrite NormalizeState
evalVars = S.topdownR $ ScopedTransform $ \ctx -> \case
  e@(Var {}) -> S.whnfRW False ctx e $ \_ r -> case r of
    Literal _ -> changed r
    _ -> pure (sink e)
  e -> pure e

-- | Assert that 'evalVars' rewrites a term, in the scope of its free
-- variables, to the expected one
evalVarsGives :: TestTerm -> TestTerm -> Assertion
evalVarsGives tm expected = withTerm tm $ \ts e -> do
  (res, _, _) <- runScopedIn def st ts evalVars e
  assertTermIs ts expected res
 where
  st :: RewriteState NormalizeState
  st = def{_globalHeap = (IntMap.empty, 0)}

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.Scoped.Whnf"
  [ testCase "let x = 3 in \\y -> x" $
      evalVarsGives
        "let { !(x :: Int) = 3 } in \\(y :: Int) -> x"
        "let { !(x :: Int) = 3 } in \\(y :: Int) -> 3"

    -- The lambda binds another variable than the let
  , testCase "let x = 3 in \\x -> x" $
      evalVarsGives
        "let { !(x :: Int) = 3 } in \\(x :: Int) -> x"
        "let { !(x :: Int) = 3 } in \\(x :: Int) -> x"

    -- The y of the right-hand side is the free one, not the lambda's
  , testCase "let x = y in \\y -> x" $
      evalVarsGives
        "let { !(x :: Int) = (y :: Int) } in \\(y :: Int) -> x"
        "let { !(x :: Int) = y } in \\(y_9 :: Int) -> x"

  , testCase "let y = 5 in let x = y in \\y -> x" $
      evalVarsGives
        "let { !(y :: Int) = 5 } in let { !(x :: Int) = y } in \\(y :: Int) -> x"
        "let { !(y :: Int) = 5 } in let { !(x :: Int) = 5 } in \\(y :: Int) -> 5"

  , testCase "let y = 5 in let x = y in case s of MkPair y z -> x" $
      evalVarsGives
        "let { !(y :: Int) = 5 } in let { !(x :: Int) = y } in (case (s :: Pair) of { MkPair (y :: Int) (z :: Int) -> x }) :: Int"
        "let { !(y :: Int) = 5 } in let { !(x :: Int) = 5 } in (case s of { MkPair (y :: Int) (z :: Int) -> 5 }) :: Int"

    -- The right-hand side of w refers to the inner x, which is not on the
    -- heap while rewriting that right-hand side; the body sees both
  , testCase "let x = 3 in letrec { x = 4; w = x } in w" $
      evalVarsGives
        "let { !(x :: Int) = 3 } in let { (x :: Int) = 4; (w :: Int) = x } in w"
        "let { !(x :: Int) = 3 } in let { (x :: Int) = 4; (w :: Int) = x } in 4"

  , testCase "let x = y in let y = 5 in x" $
      evalVarsGives
        "let { !(x :: Int) = (y :: Int) } in let { !(y :: Int) = 5 } in x"
        "let { !(x :: Int) = y } in let { !(y_9 :: Int) = 5 } in x"

  , testCase "let x = (v :: a) in /\\a -> x" $
      evalVarsGives
        "let { !(x :: Int) = (v :: a) } in \\(a :: Type) -> x"
        "let { !(x :: Int) = v } in \\(a_9 :: Type) -> x"
  ]
