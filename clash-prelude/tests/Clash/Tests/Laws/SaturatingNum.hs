{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Tests.Laws.SaturatingNum (tests) where

import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit

import Clash.Class.Num
import Clash.Sized.Index (Index)
import Clash.Sized.Signed (Signed)
import Clash.Sized.Fixed (SFixed, UFixed)
import Clash.Sized.Unsigned (Unsigned)

import Control.DeepSeq (NFData)
import GHC.TypeLits (KnownNat)

import Hedgehog
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen

type TestWrap = Bool

type SaturationLaw a =
  (Ord a, Show a, Eq a, SaturatingNum a) =>
  Gen a ->
  Assertion

isTotal ::
  forall a.
  (NFData a, Show a, Eq a) =>
  (SaturationMode -> a -> a -> a) ->
  Gen a ->
  Property
isTotal f genA = property $ do
  satMode <- forAll Gen.enumBounded
  a <- forAll genA
  b <- forAll genA
  _ <- evalNF (f satMode a b)
  pure ()

satWrapOverflowLaw :: forall a. SaturationLaw a
satWrapOverflowLaw _ = satSucc @a SatWrap maxBound @?= minBound

satWrapUnderflowLaw :: forall a. SaturationLaw a
satWrapUnderflowLaw _ = satPred @a SatWrap minBound @?= maxBound

satBoundOverflowLaw :: forall a. SaturationLaw a
satBoundOverflowLaw _ = satSucc @a SatBound maxBound @?= maxBound

satBoundUnderflowLaw :: forall a. SaturationLaw a
satBoundUnderflowLaw _ = satPred @a SatBound minBound @?= minBound

satZeroOverflowLaw :: forall a. SaturationLaw a
satZeroOverflowLaw _ = satSucc @a SatZero maxBound @?= 0

satZeroUnderflowLaw :: forall a. SaturationLaw a
satZeroUnderflowLaw _ = satPred @a SatZero minBound @?= 0

satSymmetricOverflow :: forall a. SaturationLaw a
satSymmetricOverflow _ = satSucc @a SatSymmetric maxBound @?= maxBound

satSymmetricUnderflow :: forall a. SaturationLaw a
satSymmetricUnderflow _ =
  if minBound @a < 0 then
    -- Signed number
    satPred @a SatSymmetric minBound @?= satSucc SatWrap minBound
  else
    -- Unsigned number (or zero-width)
    satPred @a SatSymmetric minBound @?= minBound

saturatingNumLaws ::
  (NFData a, Ord a, Show a, Eq a, SaturatingNum a) =>
  TestWrap ->
  Gen a ->
  [TestTree]
saturatingNumLaws testWrap genA =
  (if testWrap then
    [ testCase "SatWrap: Wrap around on overflow" (satWrapOverflowLaw genA)
    , testCase "SatWrap: Wrap around on underflow" (satWrapUnderflowLaw genA) ]
  else
    []) <>
  [ testCase "SatBound: Become maxBound on overflow" (satBoundOverflowLaw genA)
  , testCase "SatBound: Become minBound on underflow" (satBoundUnderflowLaw genA)

  , testCase "SatZero: Become 0 on overflow" (satZeroOverflowLaw genA)
  , testCase "SatZero: Become 0 on underflow" (satZeroUnderflowLaw genA)

  , testCase "SatSymmetric: Become maxBound on overflow" (satSymmetricOverflow genA)
  , testCase "SatSymmetric: Become minBound or minBound+1 on underflow" (satSymmetricUnderflow genA)

  , testProperty "satAddTotal" (isTotal satAdd genA)
  , testProperty "satSubTotal" (isTotal satSub genA)
  , testProperty "satMulTotal" (isTotal satMul genA)
  ]

testSaturationLaws ::
  (NFData a, Ord a, Show a, Eq a, SaturatingNum a) =>
  TestWrap ->
  String ->
  Gen a ->
  TestTree
testSaturationLaws testWrap typeName genA =
  testGroup typeName (saturatingNumLaws testWrap genA)

-- | Generates a bounded integral with a bias towards extreme values:
--
--      5%: minBound
--      5%: maxBound
--      5%: 0
--     85%: uniform [minBound, maxBound]
--
genBoundedIntegral :: (Integral a, Bounded a) => Gen a
genBoundedIntegral = Gen.frequency
  [ (5,  pure minBound)
  , (5,  pure maxBound)
  , (5,  pure 0)
  , (85, Gen.integral (Range.linear minBound maxBound)) ]

genIndex :: forall n. KnownNat n => Gen (Index n)
genIndex = genBoundedIntegral

genUnsigned :: forall n. KnownNat n => Gen (Unsigned n)
genUnsigned = genBoundedIntegral

genSigned :: forall n. KnownNat n => Gen (Signed n)
genSigned = genBoundedIntegral

-- | Generates a bounded fractional with a bias towards extreme values:
--
--      5%: minBound
--      5%: maxBound
--      5%: 0.0
--     85%: uniform [minBound, maxBound]
--
genBoundedFractional :: forall a. (Real a, Fractional a, Bounded a) => Gen a
genBoundedFractional = Gen.frequency
  [ (5,  pure minBound)
  , (5,  pure maxBound)
  , (5,  pure 0.0)
  , (85,   fmap (fromRational . toRational)
         $ Gen.double
         $ fmap fromRational
         $ Range.linearFrac
             (toRational (minBound @a))
             (toRational (maxBound @a))) ]

genSFixed :: forall a b. (KnownNat a, KnownNat b) => Gen (SFixed a b)
genSFixed = genBoundedFractional

genUFixed :: forall a b. (KnownNat a, KnownNat b) => Gen (UFixed a b)
genUFixed = genBoundedFractional

tests :: TestTree
tests = testGroup "SaturatingNum"
  [ testSaturationLaws True "Index 1" (genIndex @1)
  , testSaturationLaws True "Index 2" (genIndex @2)
  , testSaturationLaws True "Index 128" (genIndex @128)

  , testSaturationLaws True "Unsigned 0" (genUnsigned @0)
  , testSaturationLaws True "Unsigned 1" (genUnsigned @1)
  , testSaturationLaws True "Unsigned 32" (genUnsigned @32)
  , testSaturationLaws True "Unsigned 127" (genUnsigned @127)
  , testSaturationLaws True "Unsigned 128" (genUnsigned @128)

  , testSaturationLaws True "Signed 0" (genSigned @0)
  , testSaturationLaws True "Signed 1" (genSigned @1)
  , testSaturationLaws True "Signed 32" (genSigned @32)
  , testSaturationLaws True "Signed 127" (genSigned @127)
  , testSaturationLaws True "Signed 128" (genSigned @128)

  -- , testSaturationLaws False "SFixed 0 0" (genSFixed @0 @0)
  , testSaturationLaws False "SFixed 0 1" (genSFixed @0 @1)
  , testSaturationLaws False "SFixed 1 0" (genSFixed @1 @0)
  , testSaturationLaws False "SFixed 1 1" (genSFixed @1 @1)
  , testSaturationLaws False "SFixed 1 2" (genSFixed @1 @2)
  , testSaturationLaws False "SFixed 2 1" (genSFixed @2 @1)
  , testSaturationLaws False "SFixed 2 2" (genSFixed @2 @2)
  , testSaturationLaws False "SFixed 128 128" (genSFixed @128 @128)

  , testSaturationLaws False "UFixed 0 0" (genUFixed @0 @0)
  , testSaturationLaws False "UFixed 0 1" (genUFixed @0 @1)
  , testSaturationLaws False "UFixed 1 0" (genUFixed @1 @0)
  , testSaturationLaws False "UFixed 1 1" (genUFixed @1 @1)
  , testSaturationLaws False "UFixed 1 2" (genUFixed @1 @2)
  , testSaturationLaws False "UFixed 2 1" (genUFixed @2 @1)
  , testSaturationLaws False "UFixed 2 2" (genUFixed @2 @2)
  , testSaturationLaws False "UFixed 128 128" (genUFixed @128 @128)
  ]
