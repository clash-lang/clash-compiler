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

import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Bind (..), Term (..))
import Clash.Core.VarEnv (emptyInScopeSet)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.Util (changed)

import Test.Clash.Rewrite
  (assertErrorContainsIO, intId, intLit, runSingleTransformation)

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

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Rewrite.Util"
    [ testCase "apply reports transformations that introduce shadowing (#490)"
        applyShadowCheck
    ]
