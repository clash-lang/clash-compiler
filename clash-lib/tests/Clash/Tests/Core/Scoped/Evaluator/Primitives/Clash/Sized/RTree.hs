{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.Evaluator.Primitives.Clash.Sized.RTree", on
  fixed inputs. @clash-differential prims --open@ compares the reductions with
  those of Clash Core on generated calls.
-}

{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.RTree (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Scoped.Builder (TmB, dataB)
import Clash.Core.Scoped.Syntax (emptyScope)

import Test.Clash.Rewrite (ghcPairDc, ghcPairTcNm)
import Test.Clash.Scoped.Evaluator (assertSameValue)
import Test.Clash.Scoped.Evaluator.Primitives

-- | A tree of the given depth of @Integer@ literals
intTree :: Integer -> [Integer] -> TmB
intTree d = rtree d integerPrimTy . map integerLit

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.Evaluator.Primitives.Clash.Sized.RTree"
  [ testCase "tsplit gives the subtrees as a pair" $
      -- tsplit :: forall d a. RTree (d + 1) a -> (RTree d a, RTree d a)
      let primTy =
            forAll "d_9201" natKind $ \d -> forAll "a_9202" typeKind $ \a ->
              rtreeTy (addTy d (numTy 1)) a ~> tc ghcPairTcNm [rtreeTy d a, rtreeTy d a]
          subTy = rtreeTy (numTy 0) integerPrimTy
      in assertSameValue emptyScope
           (closed (apps (dataB ghcPairDc)
              [Right subTy, Right subTy, Left (intTree 0 [1]), Left (intTree 0 [2])]))
           (evalSubj (apps (prim "Clash.Sized.RTree.tsplit" primTy)
              [Right (numTy 0), Right integerPrimTy, Left (intTree 1 [1, 2])]))

  , testCase "treplicate builds a tree of copies of its argument" $
      -- treplicate :: forall d a. SNat d -> a -> RTree d a
      let primTy =
            forAll "d_9201" natKind $ \d -> forAll "a_9202" typeKind $ \a ->
              snatTy d ~> a ~> rtreeTy d a
      in assertSameValue emptyScope (closed (intTree 2 [7, 7, 7, 7]))
           (evalSubj (apps (prim "Clash.Sized.RTree.treplicate" primTy)
              -- The reduction doesn't look at the SNat, but it must be a value
              [ Right (numTy 2), Right integerPrimTy, Left (natLit 2)
              , Left (integerLit 7) ]))
  ]
