{-# LANGUAGE CPP #-}

module T2220_toEnumOOB where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: BitVector 2 -> Maybe A_Index
topEntity x | x == 3    = Nothing
            | otherwise = Just $ toEnum $ fromIntegral x
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

{-
Because of the concurrent nature of the VHDL generate by clash,
in VHDL the toEnum will be called with on 3 too.
This toEnum is implemented in VHDL by fromSLV,
which used to throw an exception on out-of-bound values.
-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst ( 0 :> 1 :> 2 :> 3 :> Nil)
    expectedOutput = outputVerifier' clk rst (Just A_0 :> Just A_1 :> Just A_2 :> Nothing :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen


data A_Index = A_0 | A_1 | A_2
      deriving (Show,Bounded,Enum,Generic,BitPack,Eq,ShowX)
