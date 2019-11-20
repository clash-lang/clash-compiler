{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.Tests.Signal.Trace where

import           Prelude
import           Clash.Prelude          hiding (take,drop)
import           Clash.Signal.Trace     (dumpToList)
import qualified Data.Text              as Text
import           Data.Text              (Text)

import           Test.Tasty
import           Test.Tasty.HUnit

dump :: Signal dom a -> Int -> Text -> [Int]
dump s n nm =
  case take n <$> dumpToList s nm of
    Left err -> error (Text.unpack err)
    Right ints -> ints

dumpS :: Text -> [Int]
dumpS = dump simpleTrace 5

dumpR :: Text -> [Int]
dumpR = dump recursiveTrace 10

-- | Simple (non-recursive) adder, where the inputs are delayed by a register
simpleTrace :: Signal System Int
simpleTrace = withClockResetEnable clockGen resetGen enableGen $
  delayedAdd
    (fromListWithReset 2 [3, 4, 5])
    (fromListWithReset 20 [30, 40, 50])
 where
  delayedAdd a b =
    let
      a' = traceSignal "a'" (register 1 a)
      b' = traceSignal "b'" (register 10 b)
    in
      traceSignal "c" (a' + b')

-- | Recursively defined  counting circuit
recursiveTrace :: Signal System Int
recursiveTrace = withClockResetEnable clockGen resetGen enableGen c
 where
  c :: HiddenClockResetEnable dom => Signal dom Int
  c =
    let
      a = traceSignal "a" $ mealy (\(!s) i -> (s+i, s)) 0 b
      b = traceSignal "b" $ mealy (\(!s) i -> (s+i, s)) 8 a
    in
      a

tests :: TestTree
tests = testGroup "Signal.Trace"
  [ testGroup "Simple"
    [ -- Simple, non-recursive test case
      testCase "a'" $ dumpS "a'" @?= [1,1,3,4,5]
    , testCase "b'" $ dumpS "b'" @?= [10,10,30,40,50]
    , testCase "c"  $ dumpS "c" @?= [11,11,33,44,55]
    ]
  , testGroup "Rec"
    [ -- Recursive test case, to test for <<loop>>
      testCase "a" $ dumpR "a" @?= [0,0,8,16,32,64,128,256,512,1024]
    , testCase "b" $ dumpR "b" @?= [8,8,8,16,32,64,128,256,512,1024]
    ]
  ]

