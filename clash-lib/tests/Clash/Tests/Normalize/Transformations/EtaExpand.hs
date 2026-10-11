{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.EtaExpand"
-}

{-# LANGUAGE QuasiQuotes #-}

module Clash.Tests.Normalize.Transformations.EtaExpand (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Term (Term)
import Clash.Normalize.Transformations.EtaExpand (etaExpansionTL)
import Clash.Normalize.Types (NormalizeState)
import Clash.Rewrite.Types (RewriteState)

import Test.Clash.Rewrite
  (assertAlphaEq, parseToTermQQ, runSingleTransformation)

-- | Run 'etaExpansionTL' on a closed term
runEtaTL :: Term -> IO Term
runEtaTL = runSingleTransformation def st def etaExpansionTL
 where
  st :: RewriteState NormalizeState
  st = def

-- | 'etaExpansionTL' moves the lambdas in the body of a let-expression out of
-- that let-expression:
--
-- > let i = x in \y -> e  ==>  \y -> let i = x in e
--
-- It used to do so without renaming @y@. If @y@ has the same unique as @i@,
-- the let-binder captures the references to @y@ in @e@.
etaTLLetCapturesLambda :: Assertion
etaTLLetCapturesLambda = do
  res <- runEtaTL [parseToTermQQ|
    let
      i_1 :: Int
      i_1 = 3
    in
      \(i_1 :: Int) -> i_1
  |]
  assertAlphaEq
    [parseToTermQQ|
      \(y_2 :: Int) ->
        let
          i_1 :: Int
          i_1 = 3
        in
          y_2
    |]
    res

-- | Like 'etaTLLetCapturesLambda', but now the moved lambda captures a free
-- variable of the right-hand side of the let-binding: if @y@ is free in @x@,
-- then @\\y -> let i = x in e@ binds it.
etaTLLambdaCapturesLet :: Assertion
etaTLLambdaCapturesLet = do
  res <- runEtaTL [parseToTermQQ|
    \(y_1 :: Int) ->
      let
        i_2 :: Int
        i_2 = y_1
      in
        \(y_1 :: Int) -> i_2
  |]
  assertAlphaEq
    [parseToTermQQ|
      \(y_1 :: Int) (y_3 :: Int) ->
        let
          i_2 :: Int
          i_2 = y_1
        in
          i_2
    |]
    res

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.EtaExpand"
    [ testCase "etaExpansionTL: let-binder does not capture lambda"
        etaTLLetCapturesLambda
    , testCase "etaExpansionTL: lambda does not capture let-binding"
        etaTLLambdaCapturesLet
    ]
