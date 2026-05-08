module Clash.Tests.Index (tests) where

import Data.Bits (Bits(shiftR))
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Sized.Internal.Index (Index, minus#)

import Test.Tasty.HUnit.Extra (expectXException)

tests :: TestTree
tests = testGroup "Index"
  [ testGroup "shiftR"
    [ testCase "shiftR 5 0 == 5" $
        shiftR (5 :: Index 128) 0 @?= 5
    , testCase "returns 0 when n == bitSize" $
        shiftR (127 :: Index 128) 7 @?= 0
    , testCase "returns 0 when n > bitSize" $
        shiftR (127 :: Index 128) (7 + 1) @?= 0
    , testCase "returns 0 when n >> bitSize" $
        shiftR (127 :: Index 128) (7 + 1000) @?= 0
    , testCase "undefined when n < 0" $
        expectXException (shiftR (1 :: Index 128) (-1))
    ]
  , testGroup "XException on illegal input"
    [ testCase "minus# underflow" $
        expectXException (minus# (0 :: Index 8) (1 :: Index 8) :: Index 15)
    ]
  ]
