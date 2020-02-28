module Clash.Tests.Unsigned (tests) where

import Data.Proxy
import GHC.TypeNats (KnownNat, SomeNat (..), natVal, someNatVal)
import Test.Tasty
import Test.Tasty.QuickCheck

import Clash.Sized.Internal.Unsigned
import Clash.Tests.SizedNum

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
  ]

fromIntegerProp :: forall m. KnownNat m => Proxy m -> Integer -> Property
fromIntegerProp p n = unsafeToInteger m === fromInteger (n `mod` (2 ^ toInteger (natVal p)))
  where
    m :: Unsigned m
    m = fromInteger n

fromIntegerRandomProp :: Positive Integer -> Integer -> Property
fromIntegerRandomProp (Positive m) n = m > 1 ==> case someNatVal (fromInteger m) of
  SomeNat p -> fromIntegerProp p n
