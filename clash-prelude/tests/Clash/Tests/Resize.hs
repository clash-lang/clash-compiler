{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Tests.Resize (tests, main) where

import Clash.Prelude (Unsigned, Signed, BitVector, Index)
import Clash.Promoted.Nat (SNat(..))
import Clash.XException (XException)
import Control.DeepSeq (NFData)
import Control.Exception (SomeException, try, evaluate)
import Data.Either (isLeft)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeNats (KnownNat, SomeNat(..), type (<=), type (+), Nat, someNatVal)
import Hedgehog ((===))
import Numeric.Natural (Natural)
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.Hedgehog (testPropertyNamed)
import Test.Tasty.QuickCheck (testProperty, discard, ioProperty, counterexample)

import qualified Clash.Class.Resize as Resize
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty.QuickCheck as Q

withSomeSNat :: Natural -> (forall (n :: Nat). SNat n -> r) -> r
withSomeSNat n f = case someNatVal n of
  SomeNat (_ :: Proxy n) -> f (SNat @n)

-- | Anything that's in bounds should not cause an error
indexProp ::
  forall a b.
  ((a <= b), KnownNat a, KnownNat b) =>
  Proxy b -> Index a -> Bool
indexProp Proxy v =
  Resize.resize v == Resize.checkedResize @a @b v

-- | Anything that's out of bounds should cause an error
indexFailProp ::
  forall a b.
  ((b <= a), KnownNat a, KnownNat b) =>
  Proxy b -> Index a -> Q.Property
indexFailProp Proxy v =
  let checked = Resize.checkedResize @a @b v in
  if toInteger v > toInteger (maxBound @(Index b)) then
    expectExceptionNoX checked
  else
    discard

maybeResizePropT ::
  forall f a b .
  ( Integral (f a)
  , Bounded (f a)
  , Show (f a)
  , Integral (f b)
  , Bounded (f b)
  , Show (f b)
  , Resize.Resize f
  , KnownNat a
  , KnownNat b
  ) =>
  Proxy (f a) ->
  Proxy (f b) ->
  H.PropertyT IO ()
maybeResizePropT _ _ = do
  let
    minFa = fromIntegral (minBound @(f a)) :: Integer
    maxFa = fromIntegral (maxBound @(f a)) :: Integer
    minFb = fromIntegral (minBound @(f b)) :: Integer
    maxFb = fromIntegral (maxBound @(f b)) :: Integer

  H.footnote $ "minFa: " <> show minFa
  H.footnote $ "maxFa: " <> show maxFa
  H.footnote $ "minFb: " <> show minFb
  H.footnote $ "maxFb: " <> show maxFb

  input <- H.forAll $ Gen.integral (Range.constant minFa maxFa)
  let output = Resize.maybeResize @a @b @f (fromIntegral input)
  if minFb <= input && input <= maxFb
  then output === Just (fromIntegral input)
  else output === Nothing

maybeResizeUnsignedProp :: H.Property
maybeResizeUnsignedProp = H.property $ do
  a <- H.forAll $ Gen.integral (Range.linear 0 100)
  b <- H.forAll $ Gen.integral (Range.linear 0 100)
  withSomeSNat a $ \(SNat :: SNat a) -> do
    withSomeSNat b $ \(SNat :: SNat b) -> do
      maybeResizePropT @Unsigned @a @b Proxy Proxy

maybeResizeSignedProp :: H.Property
maybeResizeSignedProp = H.property $ do
  a <- H.forAll $ Gen.integral (Range.linear 0 100)
  b <- H.forAll $ Gen.integral (Range.linear 0 100)
  withSomeSNat a $ \(SNat :: SNat a) -> do
    withSomeSNat b $ \(SNat :: SNat b) -> do
      maybeResizePropT @Signed @a @b Proxy Proxy

maybeResizeIndexProp :: H.Property
maybeResizeIndexProp = H.property $ do
  a <- H.forAll $ Gen.integral (Range.linear 1 (2^(128::Natural)))
  b <- H.forAll $ Gen.integral (Range.linear 1 (2^(128::Natural)))
  withSomeSNat a $ \(SNat :: SNat a) -> do
    withSomeSNat b $ \(SNat :: SNat b) -> do
      maybeResizePropT @Index @a @b Proxy Proxy

maybeResizeBitVectorProp :: H.Property
maybeResizeBitVectorProp = H.property $ do
  a <- H.forAll $ Gen.integral (Range.linear 0 100)
  b <- H.forAll $ Gen.integral (Range.linear 0 100)
  withSomeSNat a $ \(SNat :: SNat a) -> do
    withSomeSNat b $ \(SNat :: SNat b) -> do
      maybeResizePropT @BitVector @a @b Proxy Proxy

maybeTruncatePropT ::
  forall f a b .
  ( Integral (f a)
  , Bounded (f a)
  , Show (f a)
  , Integral (f (a + b))
  , Bounded (f (a + b))
  , Show (f (a + b))
  , Resize.Resize f
  , KnownNat a
  , KnownNat b
  ) =>
  Proxy (f a) ->
  Proxy (f (a + b)) ->
  H.PropertyT IO ()
maybeTruncatePropT _ _ = do
  let
    minFa = fromIntegral (minBound @(f a)) :: Integer
    maxFa = fromIntegral (maxBound @(f a)) :: Integer
    minFab = fromIntegral (minBound @(f (a + b))) :: Integer
    maxFab = fromIntegral (maxBound @(f (a + b))) :: Integer

  H.footnote $ "minFa: " <> show minFa
  H.footnote $ "maxFa: " <> show maxFa
  H.footnote $ "minFab: " <> show minFab
  H.footnote $ "maxFab: " <> show maxFab

  input <- H.forAll $ Gen.integral (Range.constant minFa maxFa)
  let output = Resize.maybeTruncateB @a @b @f (fromIntegral input)
  if minFab <= input && input <= maxFab
  then output === Just (fromIntegral input)
  else output === Nothing

maybeTruncateUnsignedProp :: H.Property
maybeTruncateUnsignedProp = H.property $ do
  a <- H.forAll $ Gen.integral (Range.linear 0 100)
  b <- H.forAll $ Gen.integral (Range.linear 0 100)
  withSomeSNat a $ \(SNat :: SNat a) -> do
    withSomeSNat b $ \(SNat :: SNat b) -> do
      maybeTruncatePropT @Unsigned @a @b Proxy Proxy

maybeTruncateSignedProp :: H.Property
maybeTruncateSignedProp = H.property $ do
  a <- H.forAll $ Gen.integral (Range.linear 0 100)
  b <- H.forAll $ Gen.integral (Range.linear 0 100)
  withSomeSNat a $ \(SNat :: SNat a) -> do
    withSomeSNat b $ \(SNat :: SNat b) -> do
      maybeTruncatePropT @Signed @a @b Proxy Proxy

maybeTruncateIndexProp :: H.Property
maybeTruncateIndexProp = H.property $ do
  a <- H.forAll $ Gen.integral (Range.linear 1 (2^(128::Natural)))
  b <- H.forAll $ Gen.integral (Range.linear 1 (2^(128::Natural)))
  withSomeSNat a $ \(SNat :: SNat a) -> do
    withSomeSNat b $ \(SNat :: SNat b) -> do
      maybeTruncatePropT @Index @a @b Proxy Proxy

maybeTruncateBitVectorProp :: H.Property
maybeTruncateBitVectorProp = H.property $ do
  a <- H.forAll $ Gen.integral (Range.linear 0 100)
  b <- H.forAll $ Gen.integral (Range.linear 0 100)
  withSomeSNat a $ \(SNat :: SNat a) -> do
    withSomeSNat b $ \(SNat :: SNat b) -> do
      maybeTruncatePropT @BitVector @a @b Proxy Proxy

-- | Succeed if evaluating leads to a non-XException Exception
expectExceptionNoX :: (Show a, NFData a) => a -> Q.Property
expectExceptionNoX a0 = ioProperty $ do
  a1 <- try @SomeException (try @XException (evaluate a0))
  pure $
    counterexample
      ("Expected non-XException Exception, got: " <> show a1)
      (isLeft a1)

tests :: TestTree
tests = testGroup "Resize"
  [ testGroup "checkedResize"
    [ testProperty "indexProp @17 @19" (indexProp @17 @19 Proxy)
    , testProperty "indexProp @19 @19" (indexProp @19 @19 Proxy)
    , testProperty "indexFailProp @37 @7" (indexFailProp @37 @7 Proxy)
    , testPropertyNamed "maybeResizeUnsignedProp" "maybeResizeUnsignedProp" maybeResizeUnsignedProp
    , testPropertyNamed "maybeResizeSignedProp" "maybeResizeSignedProp" maybeResizeSignedProp
    , testPropertyNamed "maybeResizeIndexProp" "maybeResizeIndexProp" maybeResizeIndexProp
    , testPropertyNamed "maybeResizeBitVectorProp" "maybeResizeBitVectorProp" maybeResizeBitVectorProp
    , testPropertyNamed "maybeTruncateUnsignedProp" "maybeTruncateUnsignedProp" maybeTruncateUnsignedProp
    , testPropertyNamed "maybeTruncateSignedProp" "maybeTruncateSignedProp" maybeTruncateSignedProp
    , testPropertyNamed "maybeTruncateIndexProp" "maybeTruncateIndexProp" maybeTruncateIndexProp
    , testPropertyNamed "maybeTruncateBitVectorProp" "maybeTruncateBitVectorProp" maybeTruncateBitVectorProp
    ]
  ]

main :: IO ()
main = defaultMain
  -- $ adjustOption (\_ -> HedgehogTestLimit (Just 1_000_000))
  $ tests
