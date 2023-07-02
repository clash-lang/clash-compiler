{-# LANGUAGE CPP #-}

module LotOfStates where

import Clash.Prelude
import Clash.Explicit.Testbench

data States = S_0
            | S_1
            | S_2
            | S_3
            | S_4
            | S_5
            | S_6
            | S_7
            | S_8
            | S_9
            | S_10
            deriving (Enum, Eq, Generic, NFDataX)

fsm :: States
    -> Unsigned 8
    -> (States, Unsigned 8)
fsm s i
  | s == S_0  && i ==  1 = (S_1,   1)
  | s == S_1  && i ==  2 = (S_2,   2)
  | s == S_2  && i ==  3 = (S_3,   3)
  | s == S_3  && i ==  4 = (S_4,   4)
  | s == S_4  && i ==  5 = (S_5,   5)
  | s == S_5  && i ==  6 = (S_6,   6)
  | s == S_6  && i ==  7 = (S_7,   7)
  | s == S_7  && i ==  8 = (S_8,   8)
  | s == S_8  && i ==  9 = (S_9,   9)
  | s == S_9  && i == 10 = (S_10, 10)
  | s == S_10 && i == 11 = (S_0,  11)
  | otherwise            = (s,     0)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Unsigned 8)
  -> Signal System (Unsigned 8)
topEntity = exposeClockResetEnable (mealy fsm S_0)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput = stimuliGenerator clk rst $(listToVecTH
        [0 :: (Unsigned 8), 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 1, 1])
    expectedOutput = outputVerifier' clk rst $(listToVecTH
        [0 :: (Unsigned 8), 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 0, 6, 0, 0, 7, 0, 0, 8, 0, 9, 0, 10,  0, 11,  0, 1, 0])
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
