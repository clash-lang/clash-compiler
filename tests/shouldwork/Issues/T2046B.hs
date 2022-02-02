{-# LANGUAGE ViewPatterns #-}

module T2046B where

import Clash.Prelude
import Clash.Explicit.Testbench

import T2046BType (T2046B)

topEntity :: T2046B -> T2046B
topEntity ((a, b), (c, d), (e, f), (g, h), i) =
  ( (toEnum (fromEnum a), toEnum (fromEnum b))
  , (toEnum (fromEnum c), toEnum (fromEnum d))
  , (toEnum (fromEnum e), toEnum (fromEnum f))
  , (toEnum (fromEnum g), toEnum (fromEnum h))
  , toEnum (fromEnum i)
  )
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput = stimuliGenerator clk rst
    $(listToVecTH [( (0, 0), (0, 0), (0, 0), (0, 0), 0)
                  , ((0, 1), (0, 1), (0, 1), (0, 1), 1) :: T2046B])

  expectedOutput = outputVerifier' clk rst
    $(listToVecTH [( (0, 0), (0, 0), (0, 0), (0, 0), 0)
                   , ((0, 1), (0, 1), (0, 1), (0, 1), 1) :: T2046B])

  done = expectedOutput (topEntity <$> testInput)
  clk  = tbSystemClockGen (not <$> done)
  rst  = systemResetGen
