{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests of the steps of the rewrite history ("Clash.Rewrite.History").
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Rewrite.Scoped.History (tests) where

import Data.Binary (decode, encode)
import Data.Binary.Put (runPut)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Clash.Core.Alpha (aeqTm)
import Clash.Core.FreeVars (freeTmVars, freeTyVars, hasFreeVars)
import Clash.Core.NameMap (nameSetToList)
import Clash.Core.Syntax
import Clash.Core.Binary (putHeader)
import Clash.Rewrite.History (closeRewriteStep, decodeRewriteHistory)
import Clash.Rewrite.Types (RewriteStep (..))

import Test.Clash.Rewrite (varD)
import Test.Clash.Scoped (closedTerm, termIn, withTestScope, withTests)
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
    testCase "abstracts over the free variables of both terms" $
      withTestScope [varD "x_1" "Int", varD "y_2" "Int"] $ \ts -> do
        let st = closeRewriteStep "t" "f" (termIn ts "x_1") (termIn ts "y_2")
        t_free st @?= 2
        assertBool "before" (aeqTm (t_before st) (closedTerm "\\(x :: Int) (y :: Int) -> x"))
        assertBool "after" (aeqTm (t_after st) (closedTerm "\\(x :: Int) (y :: Int) -> y"))

  , testCase "a history is the header followed by steps" $ do
      let idInt = closedTerm "\\(x :: Int) -> x"
          st1 = closeRewriteStep "t1" "f" idInt idInt
          st2 = st1 {t_name = "t2", t_free = 3}
          hist = decodeRewriteHistory (runPut putHeader <> encode st1 <> encode st2)
      map t_name hist @?= ["t1", "t2"]
      map t_free hist @?= [0, 3]

  , withTests 300 $ testProperty "closes over the free variables of a term" $
      forAllShrink genInScope shrinkInScope $ \(InScope _ e) ->
        let st = closeRewriteStep "t" "f" e e
            nFree = length (nameSetToList (freeTyVars e)) + length (nameSetToList (freeTmVars e))
        in counterexample "count" (t_free st === nFree)
             .&&. counterexample "leading binders" (property (leadingBinders (t_before st) >= nFree))
             .&&. counterexample "closed" (property (not (hasFreeVars (t_before st))))
             .&&. counterexample "before and after" (property (aeqTm (t_before st) (t_after st)))
             .&&. counterexample "Binary" (property (aeqTm (t_before st) (t_before (decode (encode st)))))
  ]
