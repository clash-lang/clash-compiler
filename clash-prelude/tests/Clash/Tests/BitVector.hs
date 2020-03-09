{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.BitVector where

import Data.Proxy
import GHC.TypeNats (KnownNat, SomeNat (..), natVal, someNatVal)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Clash.Prelude (bitPattern)
import Clash.Sized.Internal.BitVector (BitVector (..))

import Clash.Tests.SizedNum

test1 :: BitVector 8 -> Int
test1 =
  \case
    $(bitPattern "0.......") -> 0
    $(bitPattern "01......") -> 1
    $(bitPattern "11....01") -> 2
    $(bitPattern "11111110") -> 3
    $(bitPattern "........") -> 4
    _                        -> 5  -- To keep exhaustiveness checker happy

tests :: TestTree
tests = localOption (QuickCheckMaxRatio 2) $ testGroup "All"
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
    , testCase "case3-2" $ test1 0b11010110 @?= 4
    ]
  , testGroup "BitVector 1" $
      testProperty "fromInteger"
        (fromIntegerProp (Proxy :: Proxy 1)) :
      map lawsToTest (laws1 (Proxy :: Proxy (BitVector 1)))
  , testGroup "BitVector 21" $
      testProperty "fromInteger"
        (fromIntegerProp (Proxy :: Proxy 21)) :
      map lawsToTest (laws (Proxy :: Proxy (BitVector 21)))
  , testGroup "BitVector 83" $
      testProperty "fromInteger"
        (fromIntegerProp (Proxy :: Proxy 83)) :
      map lawsToTest (laws (Proxy :: Proxy (BitVector 83)))
  , testGroup "Random BitVector"
    [ testProperty "fromInteger" fromIntegerRandomProp ]
  , testGroup "Enum"
    [ testCase "[4,3..]" $ [4,3..] @?= [4,3,2,1,0 :: BitVector 8]
    , testCase "[4,2..]" $ [4,2..] @?= [4,2,0 :: BitVector 8]
    , testCase "take 5 [4,4..]" $ take 5 [4,4..] @?= [4,4,4,4,4 :: BitVector 8]
    , testCase "[2,4..]" $ [2,4..] @?= [2,4,6 :: BitVector 3]
    , testCase "[3,4..]" $ [3,4..] @?= [3,4,5,6,7 :: BitVector 3]
    ]
  ]

fromIntegerProp :: forall m. KnownNat m => Proxy m -> Integer -> Property
fromIntegerProp p n = unsafeToNatural m === fromInteger (n `mod` (2 ^ toInteger (natVal p)))
  where
    m :: BitVector m
    m = fromInteger n

fromIntegerRandomProp :: Positive Integer -> Integer -> Property
fromIntegerRandomProp (Positive m) n = m > 1 ==> case someNatVal (fromInteger m) of
  SomeNat p -> fromIntegerProp p n
