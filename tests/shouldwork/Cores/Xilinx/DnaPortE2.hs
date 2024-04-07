{-# LANGUAGE CPP #-}

module DnaPortE2 where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Cores.Xilinx.Unisim.DnaPortE2

topEntity ::
  Clock XilinxSystem ->
  Reset XilinxSystem ->
  Signal XilinxSystem (Maybe (BitVector 96))
topEntity clk rst = readDnaPortE2 clk rst enableGen simDna2
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal XilinxSystem Bool
testBench = done
 where
  expected =
    ($(listToVecTH (sampleN 200 $
      readDnaPortE2 (clockGen @XilinxSystem) noReset enableGen simDna2)))
  done = outputVerifier' clk rst expected (topEntity clk rst)
  clk = tbClockGen (not <$> done)
  rst = noReset
{-# CLASH_OPAQUE testBench #-}
