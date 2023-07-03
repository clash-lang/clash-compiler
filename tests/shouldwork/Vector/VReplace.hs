{-# LANGUAGE CPP #-}

module VReplace where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: (Integer,Unsigned 4,Vec 8 (Unsigned 4)) -> Vec 8 (Vec 8 (Unsigned 4))
topEntity (i,j,as) = zipWith (\i u -> replace i u as) (iterateI (+1) i) ((iterateI (subtract 1) j))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst
                      $(listToVecTH ([(0,8,replicate d8 0)]::[(Integer,Unsigned 4,Vec 8 (Unsigned 4))]))
    expectedOutput = outputVerifier' clk rst
                                    (((8:>0:>0:>0:>0:>0:>0:>0:>Nil):>
                                      (0:>7:>0:>0:>0:>0:>0:>0:>Nil):>
                                      (0:>0:>6:>0:>0:>0:>0:>0:>Nil):>
                                      (0:>0:>0:>5:>0:>0:>0:>0:>Nil):>
                                      (0:>0:>0:>0:>4:>0:>0:>0:>Nil):>
                                      (0:>0:>0:>0:>0:>3:>0:>0:>Nil):>
                                      (0:>0:>0:>0:>0:>0:>2:>0:>Nil):>
                                      (0:>0:>0:>0:>0:>0:>0:>1:>Nil):>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
