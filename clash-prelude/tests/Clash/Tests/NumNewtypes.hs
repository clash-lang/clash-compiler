{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Clash.Tests.NumNewtypes (tests) where

import Control.DeepSeq (NFData, force)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownNat)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog.Extra
import Hedgehog.Internal.Exception (tryEvaluate)
import Test.Tasty
import Test.Tasty.Hedgehog

import Clash.Class.Num
import Clash.Num.Erroring
import Clash.Num.Overflowing
import Clash.Num.Saturating
import Clash.Num.Wrapping
import Clash.Num.Zeroing
import Clash.Sized.Index (Index)
import Clash.Sized.Signed (Signed)
import Clash.Sized.Unsigned (Unsigned)

tests :: TestTree
tests = testGroup "Numeric Newtypes"
  [ testGroup "Erroring"
      [ testIntegral "Index 4" Error (genErroring (genIndex @4))
      , testIntegral "Signed 4" Error (genErroring (genSigned @4))
      , testIntegral "Unsigned 4" Error (genErroring (genUnsigned @4))
      ]
  , testGroup "Overflowing"
      [ testIntegral "Index 4" Over (genOverflowing (genIndex @4))
      , testIntegral "Signed 4" Over (genOverflowing (genSigned @4))
      , testIntegral "Unsigned 4" Over (genOverflowing (genUnsigned @4))
      ]
  , testGroup "Saturating"
      [ testIntegral "Index 4" Sat (genSaturating (genIndex @4))
      , testIntegral "Signed 4" Sat (genSaturating (genSigned @4))
      , testIntegral "Unsigned 4" Sat (genSaturating (genUnsigned @4))
      ]
  , testGroup "Wrapping"
      [ testIntegral "Index 4" Wrap (genWrapping (genIndex @4))
      , testIntegral "Signed 4" Wrap (genWrapping (genSigned @4))
      , testIntegral "Unsigned 4" Wrap (genWrapping (genUnsigned @4))
      ]
  , testGroup "Zeroing"
      [ testIntegral "Index 4" Zero (genZeroing (genIndex @4))
      , testIntegral "Signed 4" Zero (genZeroing (genSigned @4))
      , testIntegral "Unsigned 4" Zero (genZeroing (genUnsigned @4))
      ]
  ]

testIntegral
  :: (Bounded (f a), Integral (f a), Show (f a), NFData (f a))
  => TestName
  -> Mode f
  -> Gen (f a)
  -> TestTree
testIntegral name mode gen =
  testGroup name
    [ testProperty "Addition" $ checkIntegral2 mode gen (+)
    , testProperty "Subtraction" $ checkIntegral2 mode gen (-)
    , testProperty "Multiplication" $ checkIntegral2 mode gen (*)
    , testProperty "Negation" $ checkIntegral mode gen negate
    , testProperty "Absolute" $ checkIntegral mode gen abs
    , testProperty "Successor" $ checkIntegral mode gen succ
    , testProperty "Predecessor" $ checkIntegral mode gen pred
    , testProperty "Division" $ checkIntegral2 mode gen div
    , testProperty "Modulo" $ checkIntegral2 mode gen mod
    , testProperty "Quotient" $ checkIntegral2 mode gen quot
    , testProperty "Remainder" $ checkIntegral2 mode gen rem
    ]

data Mode :: (Type -> Type) -> Type where
  Error :: Mode Erroring
  Over  :: Mode Overflowing
  Sat   :: Mode Saturating
  Wrap  :: Mode Wrapping
  Zero  :: Mode Zeroing

data BoundsCheck
  = Overflow | Underflow
  deriving (Show)

boundsIntegral
  :: forall a
   . (Bounded a, Integral a)
  => Proxy a
  -> Maybe Integer
  -> Maybe BoundsCheck
boundsIntegral Proxy (Just x)
  | toInteger (maxBound @a) < x = Just Overflow
  | x < toInteger (minBound @a) = Just Underflow
  | otherwise = Nothing

boundsIntegral Proxy Nothing = Just Overflow

tryArithmetic :: (Show a, NFData a) => a -> PropertyT IO (Maybe a)
tryArithmetic x =
  case tryEvaluate (force x) of
    Left err -> do
      footnoteShow err
      pure Nothing

    Right res ->
      pure (Just res)

-- fromInteger wraps for most types, but not Index. So we need this to get the
-- wrapping behaviour we expect.
wrapIntegral
  :: forall a
   . (Bounded a, Integral a)
  => Integer
  -> a
wrapIntegral x =
  let minB = toInteger (minBound @a)
      maxB = toInteger (maxBound @a) + 1
   in fromInteger $! minB + (x - minB) `mod` (maxB - minB)

checkIntegral
  :: forall f a
   . (Bounded (f a), Integral (f a), Show (f a), NFData (f a))
  => Mode f
  -> Gen (f a)
  -> (forall b. Integral b => b -> b)
  -> Property
checkIntegral mode gen op =
  property $ do
    x <- forAll gen
    result <- tryArithmetic (op (toInteger x))

    case boundsIntegral (Proxy @(f a)) result of
      Nothing -> do
        label "InBounds"
        goInBounds result x

      Just info -> do
        collect info
        goOutBounds info result x
 where
  goInBounds mInteger x
    | Over <- mode
    , Just i <- mInteger
    = do let result = op x
         assert (not (hasOverflowed result))
         fromInteger i === result

    | Just i <- mInteger
    = fromInteger i === op x

    | otherwise
    -- If we reach here, the operation which should be in bounds and valid
    -- resulted in an exception being thrown.
    = error "checkIntegral.goInBounds: mInteger should not be Nothing"

  goOutBounds info mInteger x
    | Nothing <- mInteger
    = throwsDeepException (op x)

    | Error <- mode
    = throwsDeepException (op x)

    | Over <- mode
    , Just i <- mInteger
    = do let result = op x
         assert (hasOverflowed result)
         wrapIntegral i === result

    | Sat <- mode
    , Overflow <- info
    = maxBound === op x

    | Sat <- mode
    , Underflow <- info
    = minBound === op x

    | Wrap <- mode
    , Just i <- mInteger
    = wrapIntegral i === op x

    | Zero <- mode
    = 0 === op x

checkIntegral2
  :: forall f a
   . (Bounded (f a), Integral (f a), Show (f a), NFData (f a))
  => Mode f
  -> Gen (f a)
  -> (forall b. Integral b => b -> b -> b)
  -> Property
checkIntegral2 mode gen op =
  property $ do
    x <- forAll gen
    y <- forAll gen
    result <- tryArithmetic (op (toInteger x) (toInteger y))
    footnote ("result: " <> show result)

    case boundsIntegral (Proxy @(f a)) result of
      Nothing -> do
        label "InBounds"
        footnote "InBounds"
        goInBounds result x y

      Just info -> do
        collect info
        footnoteShow info
        goOutBounds info result x y
 where
  goInBounds mInteger x y
    | Over <- mode
    , Just i <- mInteger
    = do let result = op x y
         assert (not (hasOverflowed result))
         fromInteger i === result

    | Just i <- mInteger
    = fromInteger i === op x y

    | otherwise
    = error "checkIntegral2.goInBounds: mInteger should not be Nothing"

  goOutBounds info mInteger x y
    | Nothing <- mInteger
    = throwsDeepException (op x y)

    | Error <- mode
    = throwsDeepException (op x y)

    | Over <- mode
    , Just i <- mInteger
    = do let result = op x y
         assert (hasOverflowed result)
         wrapIntegral i === result

    | Sat <- mode
    , Overflow <- info
    = maxBound === op x y

    | Sat <- mode
    , Underflow <- info
    = minBound === op x y

    | Wrap <- mode
    , Just i <- mInteger
    = wrapIntegral i === op x y

    | Zero <- mode
    = 0 === op x y

genErroring :: forall a. (SaturatingNum a) => Gen a -> Gen (Erroring a)
genErroring = fmap toErroring

genOverflowing :: forall a. (SaturatingNum a) => Gen a -> Gen (Overflowing a)
genOverflowing = fmap toOverflowing

genSaturating :: forall a. (SaturatingNum a) => Gen a -> Gen (Saturating a)
genSaturating = fmap toSaturating

genWrapping :: forall a. (SaturatingNum a) => Gen a -> Gen (Wrapping a)
genWrapping = fmap toWrapping

genZeroing :: forall a. (SaturatingNum a) => Gen a -> Gen (Zeroing a)
genZeroing = fmap toZeroing

genBoundedIntegral :: forall a. (Bounded a, Integral a) => Gen a
genBoundedIntegral = Gen.frequency
  [ (10, pure minBound)
  , (10, pure 0)
  , (40, Gen.integral (Range.linear minBound maxBound))
  , (40, pure maxBound)
  ]

genIndex :: forall n. (KnownNat n) => Gen (Index n)
genIndex = genBoundedIntegral

genSigned :: forall n. (KnownNat n) => Gen (Signed n)
genSigned = genBoundedIntegral

genUnsigned :: forall n. (KnownNat n) => Gen (Unsigned n)
genUnsigned = genBoundedIntegral
