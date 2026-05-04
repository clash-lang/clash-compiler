{-# LANGUAGE CPP #-}

module T2729 where

import Clash.Prelude
import Clash.Explicit.Testbench

fromEnumTest :: Signed 8 -> Int
fromEnumTest = fromEnum

fromIntegralTest :: Signed 4 -> Signed 6
fromIntegralTest = numConvert

toEnumTest :: Int -> Signed 6
toEnumTest = toEnum

topEntity :: Signed 4 -> (Int, Signed 6, Signed 6)
topEntity x = (fromEnumTest (numConvert x), fromIntegralTest x, toEnumTest (fromEnum x))
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInputs = -1 :> -2 :> -8 :> 7 :> Nil
  expectedOutputs =
    (-1, -1, -1) :>
    (-2, -2, -2) :>
    (-8, -8, -8) :>
    (7, 7, 7) :>
    Nil

  done = outputVerifier' clk rst expectedOutputs (topEntity <$> stimuliGenerator clk rst testInputs)
  clk  = tbSystemClockGen (not <$> done)
  rst  = systemResetGen
{-# OPAQUE testBench #-}
