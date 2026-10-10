{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.Util"
-}

module Clash.Tests.Rewrite.Util (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User), noSrcSpan)
import Clash.Core.Term (Bind (..), Term (..), collectArgs, mkApps)
import Clash.Core.Var (Id, Var (varUniq))
import Clash.Core.VarEnv (emptyInScopeSet, lookupVarEnv)
import Clash.Driver.Types (Binding (..))
import Clash.Normalize.Types (NormRewrite, NormalizeState)
import Clash.Rewrite.Types (RewriteState (..))
import Clash.Rewrite.Util (changed, liftBinding, runRewrite)
import Clash.Util.Supply (newSupply)

import Test.Clash.Rewrite
  ( argUniques, assertAlphaEq, assertErrorContainsIO, assertNoFreeLocals
  , globalId, inScopeOf, intFunTy, intId, intLit, localId, runRewriteTest
  , runSingleTransformation, showPprU )

-- * Invariant checks of 'Clash.Rewrite.Util.apply'

-- | #490 made 'Clash.Rewrite.Util.apply' check that a transformation does not
-- introduce let-binders (or pattern binders) that shadow each other. This runs
-- a transformation that does exactly that, which should be reported.
--
-- https://github.com/clash-lang/clash-compiler/pull/490
-- https://github.com/clash-lang/clash-compiler/commit/f4db6b8bd189de434aa0f160b963ecda4bde7fc1
applyShadowCheck :: Assertion
applyShadowCheck =
  assertErrorContainsIO "accidentally creates shadowing" $
    runSingleTransformation def def emptyInScopeSet bad (intLit 0)
 where
  bad :: NormRewrite
  bad _ _ = changed (Let (Rec [(x, intLit 1), (x, intLit 2)]) (Var x))
  x = intId User "x" 1

-- | #1837: with @-fclash-debug-invariants@, 'Clash.Rewrite.Util.apply' should
-- error when a transformation changes a term without signalling so. It used to
-- compare the original term against itself (it discarded the result of
-- transformations that did not signal a change), so it never fired.
--
-- https://github.com/clash-lang/clash-compiler/pull/1837
applyUnsignalledChange :: Assertion
applyUnsignalledChange =
  assertErrorContainsIO "Expression changed without notice" $
    runSingleTransformation def st emptyInScopeSet sneaky (intLit 0)
 where
  sneaky :: NormRewrite
  sneaky _ _ = pure (intLit 1)
  -- 'applyDebug' skips its checks until the first transformation that signals
  -- a change, so pretend one already did
  st = def { _transformCounter = 1 }

-- | Run a rewrite that replaces a term by the given variable, under the given
-- transformation name, with the given variables in scope. The free variable
-- check of 'Clash.Rewrite.Util.applyDebug' then checks the result.
replaceBy :: String -> [Id] -> Term -> Id -> IO Term
replaceBy name inScope before new = do
  (t, _, _) <- runRewriteTest def def (runRewrite name (inScopeOf inScope) rw before)
  pure t
 where
  rw :: NormRewrite
  rw _ctx _e = changed (Var new)

-- * liftBinding

-- | 'liftBinding' lifts a (recursive) let-binding to a global function,
-- abstracting over the free variables of the binding. It must replace the
-- recursive occurrences by an application of the new function /before/
-- abstracting over the free variables. Doing it afterwards puts the applied
-- free variables under the new lambdas, which a capture-avoiding substitution
-- then renames, leaving them free.
--
-- https://github.com/clash-lang/clash-compiler/commit/e4cdac6ea66d1a12d7f63ef5e2e3722021680a4b
liftBindingSubstitutesBeforeAbstracting :: Assertion
liftBindingSubstitutesBeforeAbstracting = do
  supply <- newSupply
  let r = localId User "r" 40 (intFunTy 1)
      z = intId User "z" 41
      x = intId User "x" 31
      k = globalId User "k" 33 (intFunTy 2)
      cf = globalId User "top" 42 (intFunTy 1)
      -- r = \z -> k (r z) x
      e = Lam z (mkApps (Var k) [Left (App (Var r) (Var z)), Left (Var x)])
      st :: RewriteState NormalizeState
      st = def{_curFun = (cf, noSrcSpan), _uniqSupply = supply}
  ((_, newExpr), st1, _) <- runRewriteTest def st (liftBinding (r, e))
  case collectArgs newExpr of
    (Var rLifted, args) -> do
      assertEqual ("Arguments of the lifted function in:\n" <> showPprU newExpr)
        [Just (varUniq x)] (argUniques args)
      case lookupVarEnv rLifted (_bindings st1) of
        Nothing -> assertFailure "Lifted function not found in global bindings"
        Just b -> assertNoFreeLocals (bindingTerm b)
    _ -> assertFailure ("Expected an application of a global, but got:\n" <> showPprU newExpr)

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Rewrite.Util"
    [ testCase "apply reports transformations that introduce shadowing (#490)"
        applyShadowCheck
    , testCase "apply errors on unsignalled change (#1837)"
        applyUnsignalledChange

    -- Since #2571, the evaluator uses the let-bindings in the context of the
    -- term it rewrites. Transformations using 'whnfRW' can therefore introduce
    -- variables that are bound in that context, but are not free in the term
    -- they rewrite. #2624 relaxed the free variable check of 'applyDebug'
    -- accordingly for 'caseCon', and #3207 did so for 'reduceConst' and
    -- 'constantSpec'. The check is not relaxed for other transformations, nor
    -- for variables that are not bound in the context.
    -- https://github.com/clash-lang/clash-compiler/pull/3207
    --
    -- Note that the relaxed check misses variables captured by a binder in the
    -- context: https://github.com/clash-lang/clash-compiler/issues/3208
    , testGroup "applyDebug free variable check (#3207)" $
        [ testCase (name <> " may introduce variables bound in the context") $
            replaceBy name [x, y] (Var x) y >>= assertAlphaEq (Var y)
        | name <- ["reduceConst", "constantSpec"]
        ] <>
        [ testCase "reduceConst may not introduce variables not bound in the context" $
            assertErrorContainsIO "It introduces free variables" $
              replaceBy "reduceConst" [x] (Var x) y
        , testCase "other transformations may not introduce variables bound in the context" $
            assertErrorContainsIO "It introduces free variables" $
              replaceBy "caseCase" [x, y] (Var x) y
        ]
    , testCase "liftBinding substitutes before abstracting"
        liftBindingSubstitutesBeforeAbstracting
    ]
 where
  x = intId User "x" 1
  y = intId User "y" 2
