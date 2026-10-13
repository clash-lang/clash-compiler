{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Core.Scoped.TermInfo"
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Core.Scoped.TermInfo (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Scoped.Builder (TmB, castB, deDupTickB, tickB)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TermInfo

import Test.Clash.Rewrite (TestTerm (..), TestType (..))
import Test.Clash.Scoped
  ( TestScope (..), assertTermAlphaEq, termIn, testTypeEnv, typeIn, varD
  , withTerms, withTestScope)

-- | Terms of all shapes
examples :: [TestTerm]
examples =
  [ "\\(x :: Int) (y :: Int) -> x"
  , "\\(x :: Int) -> let (y :: Int) = x in y"
  , "\\(x :: Int) -> let { (y :: Int) = f y; (z :: Int) = y } in z"
  , "\\(a :: Type) (ax :: a) -> ax"
  , "(x :: Int)"
  , "(g_G53 :: Int -> Int)"
  , "(g_G53 :: Int -> Int) (x :: Int)"
  , wrapB (tickB deDupTickB) "(x :: Int)"
  , wrapB (\e -> castB e (testTypeB "Int") (testTypeB "Int")) "(x :: Int)"
  , "\\(ax :: a) -> (x :: Int)"
  ]

-- | A term around a parsed one, with its free variables
wrapB :: (TmB -> TmB) -> TestTerm -> TestTerm
wrapB f t = t {testTermB = f (testTermB t)}

-- | Literals, and data constructors applied to literals or not
literalExamples :: [TestTerm]
literalExamples =
  [ "3"
  , wrapB (tickB deDupTickB) "3"
  , "MkPair 1 2"
  , "MkPair 1 (x :: Int)"
  , "Just @Int 1"
  , "Just @Int (Just @Int 1)"
  , "(g_G53 :: Int -> Int) 1"
  ]

-- | Assert that a function on terms gives the expected results on terms, each
-- built in the scope of its free variables.
assertResults
  :: (Eq a, Show a)
  => [TestTerm] -> (forall n. Distinct n => Scope n -> Term n -> a) -> [a] -> Assertion
assertResults tms f expected =
  [ withTerms [tm] $ \ts -> \case
      [e] -> f (testScope ts) e
      _ -> error "impossible"
  | tm <- tms ] @?= expected

tests :: TestTree
tests = testGroup "Clash.Tests.Core.Scoped.TermInfo"
  [ testCase "termSize" $ assertResults examples (const termSize) [3, 3, 5, 2, 1, 1, 2, 1, 1, 2]
  , testCase "termSizeSmallerThan" $
      sequence_
        [ assertResults examples (const (termSizeSmallerThan k)) (map (< k) [3, 3, 5, 2, 1, 1, 2, 1, 1, 2])
        | k <- [0..6] ]
  , testCase "isFun" $
      assertResults examples (isFun testTypeEnv) [True, True, True, False, False, True, False, False, False, True]
  , testCase "isPolyFun" $
      assertResults examples (isPolyFun testTypeEnv) [True, True, True, True, False, True, False, False, False, True]
  , testCase "isLet" $ assertResults examples (const isLet) (replicate 10 False)
  , testCase "isVar" $
      assertResults examples (const isVar) [False, False, False, False, True, True, False, False, False, False]
  , testCase "isLocalVar" $
      assertResults examples (const isLocalVar) [False, False, False, False, True, False, False, False, False, False]
  , testCase "isGlobalVar" $
      assertResults examples (const isGlobalVar) [False, False, False, False, False, True, False, False, False, False]
  , testCase "isCon, isPrim, isTick, isCast" $ do
      assertResults examples (const isCon) (replicate 10 False)
      assertResults examples (const isPrim) (replicate 10 False)
      assertResults examples (const isTick) [False, False, False, False, False, False, False, True, False, False]
      assertResults examples (const isCast) [False, False, False, False, False, False, False, False, True, False]
  , testCase "isLiteral" $
      assertResults (examples <> literalExamples) (const isLiteral)
        (replicate 10 False <> [True, True, True, False, True, True, False])
  , testCase "isClosed" $
      assertResults examples (const isClosed) [True, True, False, True, False, True, False, False, False, False]
  , testCase "splitMultiPrimArgs" $ do
      -- Only the number of result types matters
      let mpi = MultiPrimInfo (error "unused") (error "unused") 2
      withTestScope [varD "r1" "Int", varD "r2" "Int", varD "i" "Int"] $ \ts -> do
        let args = [Right (typeIn ts "Int"), Left (termIn ts "i"), Left (termIn ts "r1"), Left (termIn ts "r2")]
            (args1, res) = splitMultiPrimArgs mpi args
        length args1 @?= 2
        length res @?= 2
        sequence_ (zipWith assertTermAlphaEq [termIn ts "r1", termIn ts "r2"] res)
  ]
