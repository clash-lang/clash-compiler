module TRepeat where

import Clash.Explicit.Testbench
import Clash.Prelude

topEntity :: Signal System (RTree 2 Bool)
topEntity = pure (trepeat True)

-- Simulation test
testBench :: Signal System Bool
testBench = done
 where
  expectedOutput = outputVerifier' clk rst ($ ([|v2t (replicate d4 True)|]) :> Nil)
  done = expectedOutput topEntity
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
