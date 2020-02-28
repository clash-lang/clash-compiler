module Clash.Tests.SizedNum where

import Data.Bits
import Data.Proxy
import Test.QuickCheck.Classes.Base
import Test.Tasty
import Test.Tasty.QuickCheck

laws1 :: (Eq a, Ord a, Show a, Num a, Arbitrary a, Integral a, FiniteBits a) => Proxy a -> [Laws]
laws1 p =
    [ eqLaws          p
    , ordLaws         p
    , numLaws         p
    , showLaws        p
    , integralLaws    p
    , bitsLaws        p
    ]

laws :: (Eq a, Ord a, Show a, Num a, Enum a, Bounded a, Arbitrary a, Integral a, FiniteBits a) => Proxy a -> [Laws]
laws p = boundedEnumLaws p : laws1 p

lawsToTest :: Laws -> TestTree
lawsToTest (Laws name props) =
  testGroup name $ map (uncurry testProperty) props
