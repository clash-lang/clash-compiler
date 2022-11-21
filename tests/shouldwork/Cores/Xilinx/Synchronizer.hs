module Synchronizer where

import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import qualified Clash.Cores.Xilinx.Synchronizer as X

topEntity ::
  Clock XilinxSystem ->
  Clock XilinxSystem ->
  Signal XilinxSystem Bit ->
  Signal XilinxSystem Bit
topEntity = X.dualFlipFlopSynchronizer

testBench :: Signal XilinxSystem Bool
testBench = done
 where
  testInput = stimuliGenerator clk rst (1 :> 0 :> 1 :> 1 :> Nil)
  actual    = ignoreFor clk rst ena d2 0 $ topEntity clk clk testInput
  done      = outputVerifier' clk rst (0 :> 0 :> 1 :> 0 :> 1 :> 1 :> Nil) actual
  clk       = tbClockGen (not <$> done)
  rst       = resetGen
  ena       = enableGen
