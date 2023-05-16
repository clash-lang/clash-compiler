{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}

module Clash.Tests.BitVector (tests, main) where

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Data.Proxy
import GHC.TypeNats (KnownNat, SomeNat (..), natVal, someNatVal)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Test.Tasty.Hedgehog.Extra as H
import qualified Test.Tasty.QuickCheck as Q

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Clash.Prelude
  (Bit, high, low, bitPattern, type (<=), type (-), natToInteger, msb, bLit)
import Clash.Sized.Internal.BitVector (BitVector (..))

import Clash.Tests.SizedNum

-- | Generates a BitVector either with the MSB set or not, and checks whether
-- 'msb' agrees with it.
msbTest :: forall n. (1 <= n, KnownNat n) => H.Property
msbTest = H.property $ do
  (bv, b) <- H.forAll msbGen
  b H.=== msb bv
 where
  msbGen :: H.Gen (BitVector n, Bit)
  msbGen = Gen.choice
    [ liftA2 (,) msbSetGen (Gen.constant high)
    , liftA2 (,) msbUnsetGen (Gen.constant low) ]

  msbSetGen :: H.Gen (BitVector n)
  msbSetGen = Gen.integral (Range.linear (2^natToInteger @(n-1)) maxBound)

  msbUnsetGen :: H.Gen (BitVector n)
  msbUnsetGen = Gen.integral (Range.linear 0 (pred (2^natToInteger @(n-1))))

test1 :: BitVector 8 -> Int
test1 =
  \case
    $(bitPattern "0..._....") -> 0
    $(bitPattern "01.._....") -> 1
    $(bitPattern "11.._..01") -> 2
    $(bitPattern "1111_1110") -> 3
    $(bitPattern "110a_babb") -> 4 + fromIntegral aa + fromIntegral bbb
    $(bitPattern "...._....") -> 4
    _                         -> 5  -- To keep exhaustiveness checker happy

tests :: TestTree
tests = localOption (Q.QuickCheckMaxRatio 2) $ testGroup "All"
  [ testGroup
    "bitPattern"
    [ testCase "case0-0" $ test1 0b00000000 @?= 0
    , testCase "case0-1" $ test1 0b00011001 @?= 0
    , testCase "case0-2" $ test1 0b01111111 @?= 0
    , testCase "case0-3" $ test1 0b01100000 @?= 0
    , testCase "case2-0" $ test1 0b11111101 @?= 2
    , testCase "case2-1" $ test1 0b11100001 @?= 2
    , testCase "case3-0" $ test1 0b11111110 @?= 3
    , testCase "case3-1" $ test1 0b11111111 @?= 4
    , testCase "case3-2" $ test1 0b11010110 @?= 9
    ]
  , testGroup "BitVector 1" $
      Q.testProperty "fromInteger"
        (fromIntegerProp (Proxy :: Proxy 1)) :
      map lawsToTest (laws1 (Proxy :: Proxy (BitVector 1)))
  , testGroup "BitVector 21" $
      Q.testProperty "fromInteger"
        (fromIntegerProp (Proxy :: Proxy 21)) :
      map lawsToTest (laws (Proxy :: Proxy (BitVector 21)))
  , testGroup "BitVector 83" $
      Q.testProperty "fromInteger"
        (fromIntegerProp (Proxy :: Proxy 83)) :
      map lawsToTest (laws (Proxy :: Proxy (BitVector 83)))
  , testGroup "Random BitVector"
    [ Q.testProperty "fromInteger" fromIntegerRandomProp ]
  , testGroup "Enum"
    [ testCase "[4,3..]" $ [4,3..] @?= [4,3,2,1,0 :: BitVector 8]
    , testCase "[4,2..]" $ [4,2..] @?= [4,2,0 :: BitVector 8]
    , testCase "take 5 [4,4..]" $ take 5 [4,4..] @?= [4,4,4,4,4 :: BitVector 8]
    , testCase "[2,4..]" $ [2,4..] @?= [2,4,6 :: BitVector 3]
    , testCase "[3,4..]" $ [3,4..] @?= [3,4,5,6,7 :: BitVector 3]
    ]
  , testGroup "Bounds"
    [ testCase "maxBound :: BitVector 0" $ maxBound @(BitVector 0) @?= 0
    , testCase "minBound :: BitVector 0" $ minBound @(BitVector 0) @?= 0
    ]
  , testGroup "MSB"
    [ H.testPropertyXXX "msb @(BitVector 1)" (msbTest @1)
    , H.testPropertyXXX "msb @(BitVector 2)" (msbTest @2)
    , H.testPropertyXXX "msb @(BitVector 3)" (msbTest @3)
    , H.testPropertyXXX "msb @(BitVector 37)" (msbTest @37)
    , H.testPropertyXXX "msb @(BitVector 64)" (msbTest @64)
    , H.testPropertyXXX "msb @(BitVector 128)" (msbTest @128)
    , H.testPropertyXXX "msb @(BitVector 129)" (msbTest @129)
    ]
  , testGroup "show"
    [ testCase "show0"  $ show @(BitVector 0) 0b0 @?= "0"
    , testCase "show1"  $ show @(BitVector 1) 0b1 @?= "0b1"
    , testCase "show2"  $ show @(BitVector 1) 0b0 @?= "0b0"
    , testCase "show3"  $ show @(BitVector 2) 0b00 @?= "0b00"
    , testCase "show4"  $ show @(BitVector 2) 0b01 @?= "0b01"
    , testCase "show5"  $ show @(BitVector 2) 0b10 @?= "0b10"
    , testCase "show6"  $ show @(BitVector 2) 0b11 @?= "0b11"
    , testCase "show7"  $ show @(BitVector 3) 0b111 @?= "0b111"
    , testCase "show8"  $ show @(BitVector 4) $(bLit "0000") @?= "0b0000"
    , testCase "show9"  $ show @(BitVector 4) $(bLit "000.") @?= "0b000."
    , testCase "show10" $ show @(BitVector 4) $(bLit "010.") @?= "0b010."
    , testCase "show11" $ show @(BitVector 5) $(bLit "1010.") @?= "0b1_010."
    , testCase "show12" $ show @(BitVector 8) $(bLit "0001010.") @?= "0b0001_010."
    , testCase "show13" $ show @(BitVector 9) $(bLit "10001010.") @?= "0b1_0001_010."
    ]
  ]

fromIntegerProp :: forall m. KnownNat m => Proxy m -> Integer -> Q.Property
fromIntegerProp p n = unsafeToNatural m Q.=== fromInteger (n `mod` (2 ^ toInteger (natVal p)))
  where
    m :: BitVector m
    m = fromInteger n

fromIntegerRandomProp :: Q.Positive Integer -> Integer -> Q.Property
fromIntegerRandomProp (Q.Positive m) n = m > 1 Q.==> case someNatVal (fromInteger m) of
  SomeNat p -> fromIntegerProp p n

-- Run with:
--
--    ./repld p:tests -T Clash.Tests.BitVector.main
--
-- Add -W if you want to run tests in spite of warnings
--
main :: IO ()
main = defaultMain tests
