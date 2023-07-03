{-# LANGUAGE CPP #-}

module TB where

import Clash.Prelude
import Clash.Explicit.Testbench

type Inp   = (Signed 4,Outp)
type Outp  = (Maybe (Signed 8,Bool),Bit)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Inp -> Signal System Outp
topEntity = exposeClockResetEnable (transfer `mealy` initS)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

transfer s i = (i,o)
  where
    o = snd s

initS = (0,(Nothing,0))

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst
                      $(listToVecTH ([ (1,(Just (4,True), 0))
                                     , (3,(Nothing, 1))
                                     ]::[(Signed 4,(Maybe (Signed 8,Bool),Bit))]))
    expectedOutput = outputVerifier' clk rst
                      $(listToVecTH ([(Nothing,0)
                                     ,(Just (4,True), 0)
                                     ]::[(Maybe (Signed 8,Bool),Bit)]))
    done           = expectedOutput (topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
