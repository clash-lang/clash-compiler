{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}

module Clash.Tests.Counter where

import qualified Prelude as P

import Clash.Class.Counter
import Clash.Class.Counter.Internal
import Clash.Prelude
import Data.Proxy
import Data.Typeable

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Test.Tasty
import qualified Test.Tasty.Hedgehog as H
import           Test.Tasty.HUnit

genUnsigned :: SNat n -> H.Gen (Unsigned n)
genUnsigned SNat = Gen.frequency
  [ (80, Gen.integral (Range.linear minBound maxBound))
  , (20, pure maxBound) ]

packTest2 ::
  forall a b.
  (KnownNat a, KnownNat b, KnownNat (a + b)) =>
  (Unsigned (a + b) -> Unsigned (a + b)) ->
  ((Unsigned a, Unsigned b) -> (Unsigned a, Unsigned b)) ->
  H.Property
packTest2 f1 f2 = H.property $ do
  a <- H.forAll (genUnsigned (SNat @a))
  b <- H.forAll (genUnsigned (SNat @b))
  let ab = unpack @(Unsigned (a + b)) (pack (a, b))
  f1 ab H.=== unpack (pack (f2 (a, b)))

packSuccTest2 ::
  forall a b.
  (KnownNat a, KnownNat b, KnownNat (a + b)) =>
  Proxy a ->
  Proxy b ->
  H.Property
packSuccTest2 _ _ = packTest2 (satSucc SatWrap) (countSucc @(Unsigned a, Unsigned b))

packPredTest2 ::
  forall a b.
  (KnownNat a, KnownNat b, KnownNat (a + b)) =>
  Proxy a ->
  Proxy b ->
  H.Property
packPredTest2 _ _ = packTest2 (satPred SatWrap) (countPred @(Unsigned a, Unsigned b))

-- | Counting /down/ from 'countMin' should yield 'countMin' at some point
predShouldWrapAround :: forall a. (Eq a, Counter a, Show a) => Proxy a -> Assertion
predShouldWrapAround Proxy =
  let counter = P.take 100_0000 (P.tail (P.iterate countPred countMin)) in
  assertBool "Pred should wrap-around" (countMin @a `P.elem` counter)

-- | Counting /up/ from 'countMin' should yield 'countMin' at some point
succShouldWrapAround :: forall a. (Eq a, Counter a, Show a) => Proxy a -> Assertion
succShouldWrapAround Proxy =
  let counter = P.take 100_000 (P.tail (P.iterate countSucc countMin)) in
  assertBool "Succ should wrap-around" (countMin @a `P.elem` counter)

-- | Counting /down/ from 'countMax' should yield 'countMin' at some point
predShouldSeeCountMin :: forall a. (Eq a, Counter a, Show a) => Proxy a -> Assertion
predShouldSeeCountMin Proxy =
  let counter = P.take 100_0000 (P.tail (P.iterate countPred countMax)) in
  assertBool "Pred should see countMin" (countMin @a `P.elem` counter)

-- | Counting /up/ from 'countMin' should yield 'countMax' at some point
succShouldSeeCountMax :: forall a. (Eq a, Counter a, Show a) => Proxy a -> Assertion
succShouldSeeCountMax Proxy =
  let counter = P.take 100_000 (P.tail (P.iterate countSucc countMin)) in
  assertBool "Succ should see countMax" (countMax @a `P.elem` counter)

quadTest :: forall a. (Eq a, Counter a, Typeable a, Show a) => Proxy a -> TestTree
quadTest proxy = testGroup (show (typeRep proxy))
  [ testCase "succShouldWrapAround" (succShouldWrapAround proxy)
  , testCase "predShouldWrapAround" (predShouldWrapAround proxy)
  , testCase "succShouldSeeCountMax" (succShouldSeeCountMax proxy)
  , testCase "predShouldSeeCountMin" (predShouldSeeCountMin proxy)
  ]

tests :: TestTree
tests = testGroup "All"
  [ H.testProperty "packSuccTest @2 @2"    (packSuccTest2 @2   @2 Proxy Proxy)
  , H.testProperty "packSuccTest @3 @2"    (packSuccTest2 @3   @2 Proxy Proxy)
  , H.testProperty "packSuccTest2 @129 @5" (packSuccTest2 @129 @5 Proxy Proxy)
  , H.testProperty "packPredTest @2 @2"    (packPredTest2 @2   @2 Proxy Proxy)
  , H.testProperty "packPredTest @3 @2"    (packPredTest2 @3   @2 Proxy Proxy)
  , H.testProperty "packPredTest2 @129 @5" (packPredTest2 @129 @5 Proxy Proxy)

  , quadTest (Proxy @(Signed 5))
  , quadTest (Proxy @(Signed 5, Signed 5))
  , quadTest (Proxy @(Signed 2, Signed 2, Unsigned 7))
  , quadTest (Proxy @(Either (Signed 5) (Index 5)))
  ]


-- Run with:
--
--    ./repld p:tests -T Clash.Tests.Counter.main
--
-- Add -W if you want to run tests in spite of warnings
--
main :: IO ()
main = defaultMain tests
