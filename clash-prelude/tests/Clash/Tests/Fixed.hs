{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Tests.Fixed (tests) where

import Data.Bits (isSigned)
import Data.Proxy (Proxy(..))

import Test.Tasty
import Test.Tasty.Hedgehog

import Clash.Class.Num
import Clash.Sized.Fixed (Fixed(..), FracFixedC, NumFixedC, SFixed, UFixed)

import GHC.TypeLits (KnownNat)

import Hedgehog
import Hedgehog.Extra (throwsException)
import Hedgehog.Internal.Source (HasCallStack, withFrozenCallStack)
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
  => Proxy b
  -> SaturationMode
  -> Rational
  -> Rational
saturateToBounded Proxy satMode x =
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
  toRational (satSucc satMode a) === (saturateToBounded (Proxy @a) satMode)
                                        (toRational a + 1)

satPredProperty
  :: forall a
   . (SaturatingNum a, Real a, Show a)
  => Gen a
  -> Property
satPredProperty genA = property $ do
  satMode <- forAll Gen.enumBounded
  a <- forAll genA
  toRational (satPred satMode a) === (saturateToBounded (Proxy @a) satMode)
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

saturationTests :: TestTree
saturationTests = testGroup "SaturatingNum"
  [ testSaturationLaws "SFixed 0 0" (genBoundBiasedS @0 @0)
  , testSaturationLaws "SFixed 0 1" (genBoundBiasedS @0 @1)
  , testSaturationLaws "SFixed 1 0" (genBoundBiasedS @1 @0)
  , testSaturationLaws "SFixed 0 2" (genBoundBiasedS @0 @2)
  , testSaturationLaws "SFixed 1 1" (genBoundBiasedS @1 @1)
  , testSaturationLaws "SFixed 2 0" (genBoundBiasedS @2 @0)
  , testSaturationLaws "SFixed 1 2" (genBoundBiasedS @1 @2)
  , testSaturationLaws "SFixed 2 1" (genBoundBiasedS @2 @1)
  , testSaturationLaws "SFixed 2 2" (genBoundBiasedS @2 @2)
  , testSaturationLaws "SFixed 7 7" (genBoundBiasedS @7 @7)
  , testSaturationLaws "SFixed 121 121" (genBoundBiasedS @121 @121)
  , testSaturationLaws "SFixed 128 128" (genBoundBiasedS @128 @128)

  , testSaturationLaws "UFixed 0 0" (genBoundBiasedU @0 @0)
  , testSaturationLaws "UFixed 0 1" (genBoundBiasedU @0 @1)
  , testSaturationLaws "UFixed 1 0" (genBoundBiasedU @1 @0)
  , testSaturationLaws "UFixed 0 2" (genBoundBiasedU @0 @2)
  , testSaturationLaws "UFixed 1 1" (genBoundBiasedU @1 @1)
  , testSaturationLaws "UFixed 2 0" (genBoundBiasedU @2 @0)
  , testSaturationLaws "UFixed 1 2" (genBoundBiasedU @1 @2)
  , testSaturationLaws "UFixed 2 1" (genBoundBiasedU @2 @1)
  , testSaturationLaws "UFixed 2 2" (genBoundBiasedU @2 @2)
  , testSaturationLaws "UFixed 7 7" (genBoundBiasedU @7 @7)
  , testSaturationLaws "UFixed 121 121" (genBoundBiasedU @121 @121)
  , testSaturationLaws "UFixed 128 128" (genBoundBiasedU @128 @128)
  ]

-- | Test pred for Fixed
--
-- Edges where behavior changes are picked explicitly:
--  2 % minBound
--  2 % highest value that causes an exception
--  2 % minBound + 1
--  2 % maxBound
-- 41 % uniform [minBound, minBound + 1)
-- 41 % uniform [minBound + 1, maxBound]
--
--  Note that for types with many integral bits, the two uniform ranges have
--  vastly different sizes. The range that causes exceptions is included
--  separately so numbers in that range will be part of a test run.
predProperty
  :: forall f rep int frac
   . ( NumFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => Proxy f
  -> Property
predProperty Proxy = property $ do
  let excHi = Fixed $ satPred SatBound $ unFixed valLo
      valLo = satSucc SatBound minBound
  x :: f <- forAll $ Gen.frequency
                     [ (2, pure minBound)
                     , (2, pure excHi)
                     , (2, pure valLo)
                     , (2, pure maxBound)
                     , (41, genFixed $ rangeLinearFixed excHi minBound)
                     , (41, genFixed $ rangeLinearFixed valLo maxBound)]
  if toRational x - 1 < toRational (minBound @f) then do
    throwsException (pred x)
  else do
    toRational (pred x) === toRational x - 1

-- | Test succ for Fixed
--
-- Edges where behaviour changes are picked explicitly:
--  2 % minBound
--  2 % maxBound - 1
--  2 % lowest value that causes an exception
--  2 % maxBound
-- 41 % uniform (maxBound - 1, maxBound]
-- 41 % uniform [minBound, maxBound - 1]
--
--  Note that for types with many integral bits, the two uniform ranges have
--  vastly different sizes. The range that causes exceptions is included
--  separately so numbers in that range will be part of a test run.
succProperty
  :: forall f rep int frac
   . ( NumFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => Proxy f
  -> Property
succProperty Proxy = property $ do
  let valHi = satPred SatBound maxBound
      excLo = Fixed $ satSucc SatBound $ unFixed valHi
  x :: f <- forAll $ Gen.frequency
                     [ (2, pure minBound)
                     , (2, pure valHi)
                     , (2, pure excLo)
                     , (2, pure maxBound)
                     , (41, genFixed $ rangeLinearFixed excLo maxBound)
                     , (41, genFixed $ rangeLinearFixed valHi minBound)]
  if toRational x + 1 > toRational (maxBound @f) then do
    throwsException (succ x)
  else do
    toRational (succ x) === toRational x + 1

-- The maximum length of lists we generate as test cases.
--
-- Property tests might overshoot this by a small amount, but no more than that.
maxLength :: Num n => n
maxLength = 1000

-- Verify generated list is as expected
--
-- Filters those rs that cannot and therefore should not occur in fs.
--
-- Also asserts the length is within reasonable bounds. Property tests might
-- overshoot `maxLength` by a small amount, but not more than that. But if some
-- change broke our candidate number generation in the property tests, we might
-- end up generating really long lists, which might take really long or possibly
-- a (practically) infinite amount of time to compute. While the bug or change
-- is probably not in the verified property itself, it still is an indication
-- something broke or needs to be adjusted, hence unittest failure is
-- reasonable.
listsEqual
  :: forall f rep int frac m
   . ( NumFixedC rep int frac
     , f ~ Fixed rep int frac
     , MonadTest m
     , HasCallStack
     )
  => [f]
  -> [Rational]
  -> m ()
listsEqual fs rs0 = withFrozenCallStack $ do
  let limit = 2 * maxLength
      minVal = toRational $ minBound @f
      maxVal = toRational $ maxBound @f
      rs = take limit $ takeWhile (\r -> r >= minVal && r <= maxVal) rs0
  assert (length rs < limit)
  take limit (map toRational fs) === rs

-- Round fromRational towards specific value
--
-- fromRational for Fixed rounds towards negative infinity.
-- fromRationalTowards to x rounds x towards to.
fromRationalTowards
  :: forall rep int frac
   . FracFixedC rep int frac
  => Fixed rep int frac
  -> Rational
  -> Fixed rep int frac
fromRationalTowards to x
  | toRational to < x = fromRational x
  | isSigned to       = negate $ fromRational $ negate x
  | otherwise         = let mb = maxBound :: Fixed rep int frac
                        in mb - fromRational (toRational mb - x)

enumFromProperty
  :: forall f rep int frac
   . ( FracFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => Proxy f
  -> Property
enumFromProperty Proxy = property $ do
  let minVal = if toRational (maxBound @f) < maxLength then
                 minBound
               else
                 maxBound - maxLength
  x1 :: f <- forAll $ genFixed $ rangeLinearFixed minVal maxBound
  footnote $ "x1 = Fixed " ++ show (toInteger $ unFixed x1)
  listsEqual (enumFrom x1) (enumFrom (toRational x1))

enumFromThenProperty
  :: forall f rep int frac
   . ( FracFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => Proxy f
  -> Property
enumFromThenProperty Proxy = property $ do
  x1 :: f <- forAll $ genFixed rangeLinearFixedBounded
  footnote $ "x1 = Fixed " ++ show (toInteger $ unFixed x1)
  approxLen <- forAll $ Gen.int $ Range.linear 0 maxLength
  footnote $ "approxLen = " ++ show approxLen
  let x2Range y = ( if approxLen <= 1 then
                      y
                    else
                      fromRational
                        $ toRational x1 +   (toRational y - toRational x1)
                                          / (toRational approxLen - 1)
                  , fromRational
                      $ toRational x1 +   (toRational y - toRational x1)
                                        / (toRational approxLen + 1))
  x2 :: f <- forAll $ Gen.choice
                      [ genFixed $ uncurry Range.constant $ x2Range minBound
                      , genFixed $ uncurry Range.constant $ x2Range maxBound
                      ]
  footnote $ "x2 = Fixed " ++ show (toInteger $ unFixed x2)
  let fs = enumFromThen x1 x2
      rs = enumFromThen (toRational x1) (toRational x2)
  if (x1 == x2) then do
    take 10 (map toRational fs) === take 10 rs
  else do
    listsEqual fs rs

enumFromToProperty
  :: forall f rep int frac
   . ( FracFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => Proxy f
  -> Property
enumFromToProperty Proxy = property $ do
  x1 :: f <- forAll $ genFixed rangeLinearFixedBounded
  footnote $ "x1 = Fixed " ++ show (toInteger $ unFixed x1)
  let (minVal, maxVal) = if toRational (maxBound @f) < maxLength then
                           (minBound, maxBound)
                         else
                           (x1 - maxLength, x1 + maxLength)
  y :: f <- forAll $ genFixed $ rangeLinearFixed minVal maxVal
  footnote $ "y = Fixed " ++ show (toInteger $ unFixed y)
  listsEqual (enumFromTo x1 y) (enumFromTo (toRational x1) (toRational y))

enumFromThenToProperty
  :: forall f rep int frac
   . ( FracFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => Proxy f
  -> Property
enumFromThenToProperty Proxy = property $ do
  x1 :: f <- forAll $ genFixed rangeLinearFixedBounded
  footnote $ "x1 = Fixed " ++ show (toInteger $ unFixed x1)
  let closeToMin =
        minBound @f +
        fromRational ((toRational x1 - toRational (minBound @f)) / 4)
      closeToMax =
        maxBound @f -
        fromRational ((toRational (maxBound @f) - toRational x1) / 4)
  y :: f <- forAll $ Gen.frequency
                       [ (10, genFixed $ rangeLinearFixed closeToMin minBound)
                       , (10, genFixed $ rangeLinearFixed closeToMax maxBound)
                       , (80, genFixed $ rangeLinearFixedBounded)]
  footnote $ "y = Fixed " ++ show (toInteger $ unFixed y)
  approxLen <- forAll $ Gen.int $ Range.linear 0 maxLength
  footnote $ "approxLen = " ++ show approxLen
  let (revBound, fwdBound) = if y < x1 then
                               (maxBound @f, minBound @f)
                             else
                               (minBound, maxBound)
      minX2 = if approxLen <= 1 then
                y
              else
                fromRationalTowards y
                  (toRational x1 +   (toRational y - toRational x1)
                                   / (toRational approxLen - 1))
      maxX2 = fromRationalTowards y
                  (toRational x1 +  (toRational y - toRational x1)
                                  / (toRational approxLen + 1))
  x2 :: f <- forAll $ Gen.frequency
                      [ (2, pure x1)
                      , (2, genFixed $ rangeLinearFixed x1 revBound)
                      , (2, genFixed $ rangeLinearFixed y fwdBound)
                      , (94, genFixed $ Range.constant minX2 maxX2)]
  footnote $ "x2 = Fixed " ++ show (toInteger $ unFixed x2)
  let fs = enumFromThenTo x1 x2 y
      rs = enumFromThenTo (toRational x1) (toRational x2) (toRational y)
  if (x1 == x2) then do
    take 10 (map toRational fs) === take 10 rs
  else do
    listsEqual fs rs

enumProperties
  :: forall f rep int frac
   . ( FracFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => Proxy f
  -> [TestTree]
enumProperties pf =
  [ testProperty "pred" $ predProperty pf
  , testProperty "succ" $ succProperty pf
  , testProperty "enumFrom" $ enumFromProperty pf
  , testProperty "enumFromThen" $ enumFromThenProperty pf
  , testProperty "enumFromTo" $ enumFromToProperty pf
  , testProperty "enumFromThenTo" $ enumFromThenToProperty pf
  ]

testEnumProperties
  :: forall f rep int frac
   . ( FracFixedC rep int frac
     , f ~ Fixed rep int frac
     )
  => String
  -> Proxy f
  -> TestTree
testEnumProperties typeName pf = testGroup typeName (enumProperties pf)

-- Small types are tested exhaustively in Clash.Tests.FixedExhaustive
enumTests :: TestTree
enumTests =
  testGroup "Enum"
    [ testEnumProperties "SFixed 7 7" (Proxy @(SFixed 7 7))
    , testEnumProperties "SFixed 121 121" (Proxy @(SFixed 121 121))
    , testEnumProperties "SFixed 128 128" (Proxy @(SFixed 128 128))
    , testEnumProperties "UFixed 7 7" (Proxy @(UFixed 7 7))
    , testEnumProperties "UFixed 121 121" (Proxy @(UFixed 121 121))
    , testEnumProperties "UFixed 128 128" (Proxy @(UFixed 128 128))
    ]

tests :: TestTree
tests =
  testGroup "Fixed"
    [ saturationTests
    , enumTests
    ]
