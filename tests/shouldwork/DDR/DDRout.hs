module DDRout where

import Clash.Explicit.Prelude
import Clash.Explicit.DDR
import Clash.Explicit.Testbench (ignoreFor)
import Clash.Intel.DDR
import Clash.Xilinx.DDR

createDomain vSystem{vTag="AsyncReal", vPeriod=2000, vReset=Asynchronous}  -- real clock domain
createDomain vSystem{vTag="AsyncDDR", vPeriod=1000, vReset=Asynchronous}  -- fake ddr domain
createDomain vSystem{vTag="SyncReal", vPeriod=2000, vReset=Synchronous}  -- real clock domain
createDomain vSystem{vTag="SyncDDR", vPeriod=1000, vReset=Synchronous}  -- fake ddr domain

{-
The four variants defined here are all the combinations of
  clock: Enabled  or Regular
  reset: Asynch or Sync
-}

topEntityGeneric
  :: ( KnownDomain fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownDomain slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity) )
  => Clock slow
  -> Reset slow
  -> Enable slow
  -> Signal slow (BitVector 8,BitVector 8)
  -> Signal fast (BitVector 8)
topEntityGeneric clk rst en = ddrOut clk rst en 0
-- topEntityGeneric = oddr
-- topEntityGeneric = altddioOut (SSymbol @"Cyclone IV GX")


topEntityUA
  :: Clock AsyncReal
  -> Reset AsyncReal
  -> Signal AsyncReal (BitVector 8, BitVector 8)
  -> Signal AsyncDDR (BitVector 8)
topEntityUA clk rst = topEntityGeneric clk rst enableGen

topEntityUS
  :: Clock SyncReal
  -> Reset SyncReal
  -> Signal SyncReal (BitVector 8, BitVector 8)
  -> Signal SyncDDR (BitVector 8)
topEntityUS clk rst = topEntityGeneric clk rst enableGen

topEntityGA
  :: Clock AsyncReal
  -> Reset AsyncReal
  -> Signal AsyncReal (BitVector 8, BitVector 8)
  -> Signal AsyncDDR (BitVector 8)
topEntityGA clk rst = topEntityGeneric clk rst tbEnableGen

topEntityGS
  :: Clock SyncReal
  -> Reset SyncReal
  -> Signal SyncReal (BitVector 8, BitVector 8)
  -> Signal SyncDDR (BitVector 8)
topEntityGS clk rst = topEntityGeneric clk rst tbEnableGen

input = ((0,1):>(2,3):>(4,5):>(6,7):>((8,9)::(BitVector 8,BitVector 8)) :> Nil)
expected = dummy:>dummy:> 0:>1:>2:>3:>4:>5:>6:>7:>8:>(9 :: BitVector 8) :> Nil

dummy = 0

testBenchUS :: Signal SyncDDR Bool
testBenchUS = done
  where
    testInput      = stimuliGenerator clkReal rstReal input
    actualOutput   = ignoreFor clkDDR rstDDR enableGen d1 dummy (topEntityUS clkReal rstReal testInput)
    expectedOutput = outputVerifier clkDDR rstDDR expected
    done           = expectedOutput actualOutput
    done'          = not <$> done
    clkDDR         = tbClockGen @SyncDDR done'
    clkReal        = tbClockGen @SyncReal (unsafeSynchronizer clkDDR clkReal done')
    rstDDR         = resetGen @SyncDDR
    rstReal        = resetGen @SyncReal

testBenchUA :: Signal AsyncDDR Bool
testBenchUA = done
  where
    testInput      = stimuliGenerator clkReal rstReal input
    actualOutput   = ignoreFor clkDDR rstDDR enableGen d1 dummy (topEntityUA clkReal rstReal testInput)
    expectedOutput = outputVerifier clkDDR rstDDR expected
    done           = expectedOutput actualOutput
    done'          = not <$> done
    clkDDR         = tbClockGen @AsyncDDR done'
    clkReal        = tbClockGen @AsyncReal (unsafeSynchronizer clkDDR clkReal done')
    rstDDR         = resetGen @AsyncDDR
    rstReal        = resetGen @AsyncReal

testBenchGA :: Signal AsyncDDR Bool
testBenchGA = done
  where
    testInput      = stimuliGenerator clkReal rstReal input
    actualOutput   = ignoreFor clkDDR rstDDR enableGen d1 dummy (topEntityGA clkReal rstReal testInput)
    expectedOutput = outputVerifier clkDDR rstDDR expected
    done           = expectedOutput actualOutput
    done'          = not <$> done
    clkDDR         = tbClockGen @AsyncDDR done'
    clkReal        = tbClockGen @AsyncReal (unsafeSynchronizer clkDDR clkReal done')
    rstDDR         = resetGen @AsyncDDR
    rstReal        = resetGen @AsyncReal

testBenchGS :: Signal SyncDDR Bool
testBenchGS = done
  where
    testInput      = stimuliGenerator clkReal rstReal input
    actualOutput   = ignoreFor clkDDR rstDDR enableGen d1 dummy (topEntityGS clkReal rstReal testInput)
    expectedOutput = outputVerifier clkDDR rstDDR expected
    done           = expectedOutput actualOutput
    done'          = not <$> done
    clkDDR         = tbClockGen @SyncDDR done'
    clkReal        = tbClockGen @SyncReal (unsafeSynchronizer clkDDR clkReal done')
    rstDDR         = resetGen @SyncDDR
    rstReal        = resetGen @SyncReal
