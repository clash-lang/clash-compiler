{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase      #-}

module Clash.Tests.BitVector where

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Prelude (BitVector, bitPattern)

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
tests =
  testGroup
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

