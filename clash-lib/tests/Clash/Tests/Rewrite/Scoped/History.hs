{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests of the steps of the rewrite history ("Clash.Rewrite.Scoped.History").
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Rewrite.Scoped.History (tests) where

import Data.Binary (decode, encode)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Clash.Core.Name as C
import qualified Clash.Core.Term as C

import Clash.Core.Scoped.Alpha (aeqTm)
import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.FreeVars (freeTmVars, freeTyVars, hasFreeVars)
import Clash.Core.Scoped.NameMap (nameSetToList)
import Clash.Core.Scoped.Syntax
import Clash.Rewrite.Scoped.History (closeRewriteStep)
import Clash.Rewrite.Types (RewriteStep (..))

import Test.Clash.Rewrite (intId)
import Test.Clash.Scoped (withFreeVars, withFreeVarsOf, withTests)
import Test.Clash.Scoped.Gen (InScope (..), genInScope, shrinkInScope)

-- | The number of leading type lambdas and lambdas of a term
leadingBinders :: Term n -> Int
leadingBinders = \case
  TyLam _ e -> 1 + leadingBinders e
  Lam _ e -> 1 + leadingBinders e
  _ -> 0

tests :: TestTree
tests = testGroup "Clash.Tests.Rewrite.Scoped.History"
  [ -- Before @x@, after @y@: both are abstracted over both variables
    testCase "abstracts over the free variables of both terms" $ do
      let x = intId C.User "x" 1
          y = intId C.User "y" 2
      withFreeVarsOf [C.Var x, C.Var y] $ \env -> do
        let st = closeRewriteStep "t" "f" (fromClashTerm env (C.Var x)) (fromClashTerm env (C.Var y))
        t_free st @?= 2
        assertBool "before" (aeqTm (t_before st) (fromClashTermClosed (C.Lam x (C.Lam y (C.Var x)))))
        assertBool "after" (aeqTm (t_after st) (fromClashTermClosed (C.Lam x (C.Lam y (C.Var y)))))

  , withTests 300 $ testProperty "closes over the free variables of a term" $
      forAllShrink genInScope shrinkInScope $ \(InScope fvs tm) ->
        withFreeVars fvs $ \env ->
          let e = fromClashTerm env tm
              st = closeRewriteStep "t" "f" e e
              nFree = length (nameSetToList (freeTyVars e)) + length (nameSetToList (freeTmVars e))
          in counterexample "count" (t_free st === nFree)
               .&&. counterexample "leading binders" (property (leadingBinders (t_before st) >= nFree))
               .&&. counterexample "closed" (property (not (hasFreeVars (t_before st))))
               .&&. counterexample "before and after" (property (aeqTm (t_before st) (t_after st)))
               .&&. counterexample "Binary" (property (aeqTm (t_before st) (t_before (decode (encode st)))))
  ]
