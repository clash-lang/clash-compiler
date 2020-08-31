{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Tests.Fixed (tests) where

import Test.Tasty
import Test.Tasty.Hedgehog

import Clash.Class.Num
import Clash.Sized.Fixed (SFixed, UFixed)

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

-- | Generates a bounded fractional with a bias towards extreme values:
--
--     10%: uniform [minBound, minBound + 1]
--     10%: uniform [maxBound - 1, maxBound]
--      5%: 0.0
--     75%: uniform [minBound, maxBound]
--
genBoundedFractional :: forall a. (Real a, Fractional a, Bounded a) => Gen a
genBoundedFractional = Gen.frequency
  [ (10,   fmap (fromRational . toRational)
         $ Gen.double
         $ fmap fromRational
         $ Range.linearFrac
             (toRational (minBound @a))
             (toRational (minBound @a) + 1))
  , (10,   fmap (fromRational . toRational)
         $ Gen.double
         $ fmap fromRational
         $ Range.linearFrac
             (toRational (maxBound @a) - 1)
             (toRational (maxBound @a)))
  , (5,  pure 0.0)
  , (75,   fmap (fromRational . toRational)
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
  [ testSaturationLaws "SFixed 0 0" (genSFixed @0 @0)
  , testSaturationLaws "SFixed 0 1" (genSFixed @0 @1)
  , testSaturationLaws "SFixed 1 0" (genSFixed @1 @0)
  , testSaturationLaws "SFixed 1 1" (genSFixed @1 @1)
  , testSaturationLaws "SFixed 1 2" (genSFixed @1 @2)
  , testSaturationLaws "SFixed 2 1" (genSFixed @2 @1)
  , testSaturationLaws "SFixed 2 2" (genSFixed @2 @2)
  , testSaturationLaws "SFixed 128 128" (genSFixed @128 @128)

  , testSaturationLaws "UFixed 0 0" (genUFixed @0 @0)
  , testSaturationLaws "UFixed 0 1" (genUFixed @0 @1)
  , testSaturationLaws "UFixed 1 0" (genUFixed @1 @0)
  , testSaturationLaws "UFixed 1 1" (genUFixed @1 @1)
  , testSaturationLaws "UFixed 1 2" (genUFixed @1 @2)
  , testSaturationLaws "UFixed 2 1" (genUFixed @2 @1)
  , testSaturationLaws "UFixed 2 2" (genUFixed @2 @2)
  , testSaturationLaws "UFixed 128 128" (genUFixed @128 @128)
  ]
