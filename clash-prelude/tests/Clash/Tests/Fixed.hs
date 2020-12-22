{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Tests.Fixed (tests) where

import Test.Tasty
import Test.Tasty.Hedgehog

import Clash.Class.Num
import Clash.Sized.Fixed (Fixed(..), NumFixedC, SFixed, UFixed)

import GHC.TypeLits (KnownNat)

import Hedgehog
import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen

-- Saturate a number to be within certain bounds according to SaturationMode.
saturate
  :: RealFrac a
  => a  -- minBound
  -> a  -- maxBound
  -> a  -- repInt: the repetition interval of wrapping behaviour.
  -> SaturationMode
  -> a -> a
saturate minB _ repInt SatWrap x =
  let offs = x - minB
      offsW = minB + (offs - repInt * fromIntegral (floor @_ @Integer
                (offs / repInt)))
  in offsW
saturate minB maxB _ SatBound x
  | x < minB  = minB
  | x > maxB  = maxB
  | otherwise = x
saturate minB maxB _ SatZero x
  | x < minB  = 0
  | x > maxB  = 0
  | otherwise = x
saturate minB maxB _ SatSymmetric x
  | x < minB  = if minB < 0 then (-maxB) else minB
  | x > maxB  = maxB
  | otherwise = x

-- Saturate to bounds of type b.
--
-- It assumes the following values for the repetition interval:
--   - 2*(-minBound) when minBound < 0
--   - floor maxBound + 1 otherwise
-- This is correct for at least:
-- - Unsigned: maxBound @(Unsigned 4) == 15, behaves as modulo 16
-- - Signed: minBound @(Signed 4) == -8, behaves as shifted modulo 16
-- - UFixed: maxBound @(UFixed 4 2) == 15.75, behaves as modulo 16
-- - SFixed: minBound @(SFixed 4 m) == -8.0, behaves as shifted modulo 16
saturateToBounded
  :: forall b
   . (Bounded b, Real b)
  => SaturationMode
  -> Rational
  -> Rational
saturateToBounded satMode x =
  let repInt = if (minBound @b) < 0
               then 2 * negate (toRational (minBound @b))
               else 1 + toRational (floor @_ @Integer $ toRational $
                                      maxBound @b)
  in saturate (toRational (minBound @b)) (toRational (maxBound @b)) repInt
       satMode x


satSuccProperty
  :: forall a
   . (SaturatingNum a, Real a, Show a)
  => Gen a
  -> Property
satSuccProperty genA = property $ do
  satMode <- forAll Gen.enumBounded
  a <- forAll genA
  toRational (satSucc satMode a) === (saturateToBounded @a satMode)
                                        (toRational a + 1)

satPredProperty
  :: forall a
   . (SaturatingNum a, Real a, Show a)
  => Gen a
  -> Property
satPredProperty genA = property $ do
  satMode <- forAll Gen.enumBounded
  a <- forAll genA
  toRational (satPred satMode a) === (saturateToBounded @a satMode)
                                        (toRational a - 1)

saturatingNumLaws
  :: (SaturatingNum a, Real a, Show a)
  => Gen a
  -> [TestTree]
saturatingNumLaws genA =
  [ testProperty "satSucc" $ satSuccProperty genA
  , testProperty "satPred" $ satPredProperty genA
  ]

testSaturationLaws
  :: (SaturatingNum a, Real a, Show a)
  => String
  -> Gen a
  -> TestTree
testSaturationLaws typeName genA = testGroup typeName (saturatingNumLaws genA)

-- Generates a random Fixed number in the given [inclusive,inclusive] range.
--
-- When the generator tries to shrink, it will shrink towards the origin of the
-- specified Range.
genFixed
  :: ( MonadGen m
     , NumFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => Range f
  -> m f
genFixed range = fmap Fixed $ Gen.integral $ fmap unFixed range

-- Note that the ranges defined in Hedgehog.Range for Fractional do not interact
-- correctly with a datatype that is also Bounded, hence these variants.
rangeLinearFixed
  :: ( NumFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => f
  -> f
  -> Range f
rangeLinearFixed x y = fmap Fixed $ Range.linear (unFixed x) (unFixed y)

rangeLinearFixedBounded
  :: NumFixedC rep int frac
  => Range (Fixed rep int frac)
rangeLinearFixedBounded = fmap Fixed Range.linearBounded

-- | Generates a Fixed number with a bias towards extreme values:
--
--     10%: uniform [minBound, minBound + 1]
--     10%: uniform [maxBound - 1, maxBound]
--      5%: 0
--     75%: uniform [minBound, maxBound]
genBoundBiased
  :: forall f rep int frac
   . ( NumFixedC rep int frac
     , f ~ Fixed rep int frac
     )
   => Gen f
genBoundBiased = Gen.frequency
  [ (10, genFixed $ rangeLinearFixed (minBound + 1) minBound)
  , (10, genFixed $ rangeLinearFixed (maxBound - 1) (maxBound))
  , ( 5, pure 0)
  , (75, genFixed $ rangeLinearFixedBounded)]

genBoundBiasedS :: forall a b. (KnownNat a, KnownNat b) => Gen (SFixed a b)
genBoundBiasedS = genBoundBiased

genBoundBiasedU :: forall a b. (KnownNat a, KnownNat b) => Gen (UFixed a b)
genBoundBiasedU = genBoundBiased

tests :: TestTree
tests = testGroup "SaturatingNum"
  [ testSaturationLaws "SFixed 0 0" (genBoundBiasedS @0 @0)
  , testSaturationLaws "SFixed 0 1" (genBoundBiasedS @0 @1)
  , testSaturationLaws "SFixed 1 0" (genBoundBiasedS @1 @0)
  , testSaturationLaws "SFixed 1 1" (genBoundBiasedS @1 @1)
  , testSaturationLaws "SFixed 1 2" (genBoundBiasedS @1 @2)
  , testSaturationLaws "SFixed 2 1" (genBoundBiasedS @2 @1)
  , testSaturationLaws "SFixed 2 2" (genBoundBiasedS @2 @2)
  , testSaturationLaws "SFixed 128 128" (genBoundBiasedS @128 @128)

  , testSaturationLaws "UFixed 0 0" (genBoundBiasedU @0 @0)
  , testSaturationLaws "UFixed 0 1" (genBoundBiasedU @0 @1)
  , testSaturationLaws "UFixed 1 0" (genBoundBiasedU @1 @0)
  , testSaturationLaws "UFixed 1 1" (genBoundBiasedU @1 @1)
  , testSaturationLaws "UFixed 1 2" (genBoundBiasedU @1 @2)
  , testSaturationLaws "UFixed 2 1" (genBoundBiasedU @2 @1)
  , testSaturationLaws "UFixed 2 2" (genBoundBiasedU @2 @2)
  , testSaturationLaws "UFixed 128 128" (genBoundBiasedU @128 @128)
  ]
