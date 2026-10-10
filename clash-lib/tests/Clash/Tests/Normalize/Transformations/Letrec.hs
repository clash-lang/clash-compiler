{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.Letrec"
-}

{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.Letrec (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Bind (..), Term (..))
import Clash.Core.Var (Var (varUniq))
import Clash.Core.VarEnv (emptyInScopeSet)
import Clash.Driver.Types (ClashEnv (..), ClashOpts (..), debugNone)
import Clash.Normalize.Transformations.Letrec (topLet)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteEnv (..), RewriteState (..))

import Clash.Util.Supply (freshId, newSupply)

import Test.Clash.Rewrite (intId, intLit, runSingleTransformation, showPprU)

topLetR :: NormRewrite
topLetR = $(asRewriteQ topLet)

-- * topLet (#490)

-- | 'topLet' turns @letrec x = 1 in 2@ into @letrec x = 1; result = 2 in
-- result@. Its new binder must not clash with the binders of the let
-- expression. Before #490, 'topLet' made a new binder using an in-scope set
-- that did not contain them. To make sure the bug shows up, we give @x@ the
-- unique 'topLet' would try first: the first unique of the supply we run it
-- with.
--
-- The invariant checks of 'Clash.Rewrite.Util.apply' would catch the bug too,
-- as they report let-binders that shadow each other (see
-- "Clash.Tests.Rewrite.Util"). We disable them with 'debugNone', so that this
-- tests 'topLet' itself.
--
-- https://github.com/clash-lang/clash-compiler/pull/490
topLetFreshBinder :: Assertion
topLetFreshBinder = do
  supply <- newSupply
  let x = intId User "x" (fst (freshId supply))
  actual <-
    runSingleTransformation envNoChecks def{_uniqSupply = supply}
      emptyInScopeSet topLetR (Let (Rec [(x, intLit 1)]) (intLit 2))
  case actual of
    Let (Rec [(x1, l1), (r, l2)]) (Var res)
      | x1 == x, l1 == intLit 1, l2 == intLit 2, res == r ->
          assertBool
            ("topLet reused the unique of an existing let-binder:\n" <> showPprU actual)
            (varUniq r /= varUniq x)
    _ -> assertFailure ("Unexpected result of topLet:\n" <> showPprU actual)
 where
  env = _clashEnv def
  envNoChecks =
    def { _clashEnv = env { envOpts = (envOpts env) { opt_debug = debugNone } } }

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.Letrec"
    [ testCase "topLet does not reuse the unique of a let-binder (#490)"
        topLetFreshBinder
    ]
