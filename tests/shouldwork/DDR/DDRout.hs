module DDRout where

import Clash.Explicit.Prelude
import Clash.Explicit.DDR
import Clash.Explicit.Testbench
import Clash.Intel.DDR
import Clash.Xilinx.DDR

createDomain vSystem{vName="AsyncReal", vPeriod=2000, vResetKind=Asynchronous}  -- real clock domain
--createDomain vSystem{vName="AsyncDDR", vPeriod=1000, vResetKind=Asynchronous}  -- fake ddr domain (same as TB)
createDomain vSystem{vName="SyncReal", vPeriod=2000, vResetKind=Synchronous}  -- real clock domain
createDomain vSystem{vName="SyncDDR", vPeriod=1000, vResetKind=Synchronous}  -- fake ddr domain

-- | Domain used for testbench itself
createDomain vSystem{vName="TB", vPeriod=1000, vResetKind=Asynchronous}
type AsyncDDR = TB

{-
The four variants defined here are all the combinations of
  clock: Enabled  or Regular
  reset: Asynch or Sync
-}

topEntityGeneric
  :: ( KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity) )
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

testBenchUS :: Signal TB Bool
testBenchUS = done
  where
    testInput      = stimuliGenerator clkReal rstReal input
    actualOutput   = ignoreFor clkDDR rstDDR enableGen d1 dummy (topEntityUS clkReal rstReal testInput)
    expectedOutput = outputVerifier clkTest rstTest expected
    done           = expectedOutput actualOutput
    notDone        = not <$> done

    clkTest        = tbClockGen @TB notDone
    clkDDR         = tbClockGen @SyncDDR (unsafeSynchronizer clkTest clkDDR notDone)
    clkReal        = tbClockGen @SyncReal (unsafeSynchronizer clkTest clkReal notDone)

    rstTest        = resetGen @TB
    rstDDR         = resetGen @SyncDDR
    rstReal        = resetGen @SyncReal

testBenchUA :: Signal TB Bool
testBenchUA = done
  where
    testInput      = stimuliGenerator clkReal rstReal input
    actualOutput   = ignoreFor clkDDR rstDDR enableGen d1 dummy (topEntityUA clkReal rstReal testInput)
    expectedOutput = outputVerifier' clkDDR rstDDR expected
    done           = expectedOutput actualOutput
    notDone        = not <$> done
    clkDDR         = tbClockGen @AsyncDDR notDone
    clkReal        = tbClockGen @AsyncReal (unsafeSynchronizer clkDDR clkReal notDone)
    rstDDR         = resetGen @AsyncDDR
    rstReal        = resetGen @AsyncReal

testBenchGA :: Signal TB Bool
testBenchGA = done
  where
    testInput      = stimuliGenerator clkReal rstReal input
    actualOutput   = ignoreFor clkDDR rstDDR enableGen d1 dummy (topEntityGA clkReal rstReal testInput)
    expectedOutput = outputVerifier' clkDDR rstDDR expected
    done           = expectedOutput actualOutput
    notDone        = not <$> done
    clkDDR         = tbClockGen @AsyncDDR notDone
    clkReal        = tbClockGen @AsyncReal (unsafeSynchronizer clkDDR clkReal notDone)
    rstDDR         = resetGen @AsyncDDR
    rstReal        = resetGen @AsyncReal

testBenchGS :: Signal TB Bool
testBenchGS = done
  where
    testInput      = stimuliGenerator clkReal rstReal input
    actualOutput   = ignoreFor clkDDR rstDDR enableGen d1 dummy (topEntityGS clkReal rstReal testInput)
    expectedOutput = outputVerifier clkTest rstTest expected
    done           = expectedOutput actualOutput
    notDone        = not <$> done

    clkTest        = tbClockGen @TB notDone
    clkDDR         = tbClockGen @SyncDDR (unsafeSynchronizer clkTest clkDDR notDone)
    clkReal        = tbClockGen @SyncReal (unsafeSynchronizer clkTest clkReal notDone)

    rstTest        = resetGen @TB
    rstDDR         = resetGen @SyncDDR
    rstReal        = resetGen @SyncReal
