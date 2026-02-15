module Clash.Tests.Signed (tests) where

import Data.Proxy
import GHC.TypeNats (KnownNat, SomeNat (..), natVal, someNatVal)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Bits (Bits(shiftR))

import Clash.Sized.Internal.Signed
import Clash.Tests.SizedNum

import Test.Tasty.HUnit.Extra (expectException)

tests :: TestTree
tests = localOption (QuickCheckMaxRatio 2) $ testGroup "All"
  [ testGroup "Signed 1" $
      testProperty "fromInteger"
        (fromIntegerProp (Proxy :: Proxy 1)) :
      map lawsToTest (laws1 (Proxy :: Proxy (Signed 1)))
  , testGroup "Signed 21" $
      testProperty "fromInteger"
        (fromIntegerProp (Proxy :: Proxy 21)) :
      map lawsToTest (laws (Proxy :: Proxy (Signed 21)))
  , testGroup "Signed 83" $
      testProperty "fromInteger"
        (fromIntegerProp (Proxy :: Proxy 83)) :
      map lawsToTest (laws (Proxy :: Proxy (Signed 83)))
  , testGroup "Random Signed"
    [ testProperty "fromInteger" fromIntegerRandomProp  ]
  , testGroup "Enum"
    [ testCase "[3,2..]" $ [3,2..] @?= [3,2,1,0,-1,-2,-3,-4 :: Signed 3]
    , testCase "[3,1..]" $ [3,1..] @?= [3,1,-1,-3,-5,-7 :: Signed 4]
    , testCase "take 5 [4,4..]" $ take 5 [4,4..] @?= [4,4,4,4,4 :: Signed 4]
    , testCase "[2,4..]" $ [2,4..] @?= [2,4,6 :: Signed 4]
    , testCase "[3,4..]" $ [3,4..] @?= [3,4,5,6,7 :: Signed 4]
    ]
  , testGroup "Bounds"
    [ testCase "maxBound :: Signed 0" $ maxBound @(Signed 0) @?= 0
    , testCase "minBound :: Signed 0" $ minBound @(Signed 0) @?= 0
    ]
  , testGroup "shiftR"
    [ testCase "shiftR 5 0 == 5" $
        shiftR (5 :: Signed 8) 0 @?= 5
    , testCase "returns 0 when non-negative and n == bitSize" $
        shiftR (0x7F :: Signed 8) 8 @?= 0
    , testCase "returns 0 when non-negative and n > bitSize" $
        shiftR (0x7F :: Signed 8) (8 + 1) @?= 0
    , testCase "returns 0 when non-negative and n >> bitSize" $
        shiftR (0x7F :: Signed 8) (8 + 1000) @?= 0
    , testCase "returns -1 when negative and n == bitSize" $
        shiftR (-1 :: Signed 8) 8 @?= (-1)
    , testCase "returns -1 when negative and n > bitSize" $
        shiftR (-50 :: Signed 8) (8 + 1) @?= (-1)
    , testCase "returns -1 when negative and n >> bitSize" $
        shiftR (-1 :: Signed 8) (8 + 1000) @?= (-1)
    , testCase "undefined when n < 0" $
        expectException (shiftR (1 :: Signed 8) (-1))
    ]
  ]


fromIntegerProp :: forall m. KnownNat m => Proxy m -> Integer -> Property
fromIntegerProp p n = unsafeToInteger m === fromInteger i
  where
    m :: Signed m
    m = fromInteger n

    mb = 2 ^ (toInteger (natVal p) - 1)
    i = case divMod n mb of
          (d,r) | even d    -> r
                | otherwise -> r - mb

fromIntegerRandomProp :: Positive Integer -> Integer -> Property
fromIntegerRandomProp (Positive m) n = m > 1 ==> case someNatVal (fromInteger m) of
  SomeNat p -> fromIntegerProp p n
