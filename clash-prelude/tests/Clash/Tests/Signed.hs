module Clash.Tests.Signed (tests) where

import Data.Proxy
import GHC.TypeNats (KnownNat, SomeNat (..), natVal, someNatVal)
import Test.Tasty
import Test.Tasty.QuickCheck

import Clash.Sized.Internal.Signed
import Clash.Tests.SizedNum

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
