module Clash.Tests.Unsigned (tests) where

import Data.Proxy
import GHC.TypeNats (KnownNat, SomeNat (..), natVal, someNatVal)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Bits (Bits(shiftR))

import Clash.Sized.Internal.Unsigned
import Clash.Tests.SizedNum

import Test.Tasty.HUnit.Extra (expectException)

tests :: TestTree
tests = localOption (QuickCheckMaxRatio 2) $ testGroup "All"
  [ testGroup "Unsigned 1" $
      testProperty "fromInteger"
        (fromIntegerProp (Proxy :: Proxy 1)) :
      map lawsToTest (laws1 (Proxy :: Proxy (Unsigned 1)))
  , testGroup "Unsigned 21" $
      testProperty "fromInteger"
        (fromIntegerProp (Proxy :: Proxy 21)) :
      map lawsToTest (laws (Proxy :: Proxy (Unsigned 21)))
  , testGroup "Unsigned 83" $
      testProperty "fromInteger"
        (fromIntegerProp (Proxy :: Proxy 83)) :
      map lawsToTest (laws (Proxy :: Proxy (Unsigned 83)))
  , testGroup "Random Unsigned"
    [ testProperty "fromInteger" fromIntegerRandomProp ]
  , testGroup "Enum"
    [ testCase "[4,3..]" $ [4,3..] @?= [4,3,2,1,0 :: Unsigned 8]
    , testCase "[4,2..]" $ [4,2..] @?= [4,2,0 :: Unsigned 8]
    , testCase "take 5 [4,4..]" $ take 5 [4,4..] @?= [4,4,4,4,4 :: Unsigned 8]
    , testCase "[2,4..]" $ [2,4..] @?= [2,4,6 :: Unsigned 3]
    , testCase "[3,4..]" $ [3,4..] @?= [3,4,5,6,7 :: Unsigned 3]
    ]
  , testGroup "Bounds"
    [ testCase "maxBound :: Unsigned 0" $ maxBound @(Unsigned 0) @?= 0
    , testCase "minBound :: Unsigned 0" $ minBound @(Unsigned 0) @?= 0
    ]
  , testGroup "shiftR"
    [ testCase "shiftR 5 0 == 5" $
        shiftR (5 :: Unsigned 8) 0 @?= 5
    , testCase "returns 0 when n == bitSize" $
        shiftR (0xFF :: Unsigned 8) 8 @?= 0
    , testCase "returns 0 when n > bitSize" $
        shiftR (0xFF :: Unsigned 8) (8 + 1) @?= 0
    , testCase "returns 0 when n >> bitSize" $
        shiftR (0xFF :: Unsigned 8) (8 + 1000) @?= 0
    , testCase "undefined when n < 0" $
        expectException (shiftR (1 :: Unsigned 8) (-1))
    ]
  ]

fromIntegerProp :: forall m. KnownNat m => Proxy m -> Integer -> Property
fromIntegerProp p n = unsafeToNatural m === fromInteger (n `mod` (2 ^ toInteger (natVal p)))
  where
    m :: Unsigned m
    m = fromInteger n

fromIntegerRandomProp :: Positive Integer -> Integer -> Property
fromIntegerRandomProp (Positive m) n = m > 1 ==> case someNatVal (fromInteger m) of
  SomeNat p -> fromIntegerProp p n
