module DDRin where

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
  -> Signal fast (BitVector 8)
  -> Signal slow (BitVector 8, BitVector 8)
topEntityGeneric clk rst en = ddrIn clk rst en (dummy,dummy,dummy)
-- topEntityGeneric clk rst en = iddr clk rst en
-- topEntityGeneric clk rst en = altddioIn (SSymbol @"Cyclone IV GX") clk rst en


topEntityUA
  :: Clock AsyncReal
  -> Reset AsyncReal
  -> Signal AsyncDDR (BitVector 8)
  -> Signal AsyncReal (BitVector 8, BitVector 8)
topEntityUA clk rst = topEntityGeneric clk rst enableGen

topEntityUS
  :: Clock SyncReal
  -> Reset SyncReal
  -> Signal SyncDDR (BitVector 8)
  -> Signal SyncReal (BitVector 8, BitVector 8)
topEntityUS clk rst = topEntityGeneric clk rst enableGen

topEntityGA
  :: Clock AsyncReal
  -> Reset AsyncReal
  -> Signal AsyncDDR (BitVector 8)
  -> Signal AsyncReal (BitVector 8, BitVector 8)
topEntityGA clk rst = topEntityGeneric clk rst tbEnableGen

topEntityGS
  :: Clock SyncReal
  -> Reset SyncReal
  -> Signal SyncDDR (BitVector 8)
  -> Signal SyncReal (BitVector 8, BitVector 8)
topEntityGS clk rst = topEntityGeneric clk rst tbEnableGen


testinput = $(listToVecTH [1..17::BitVector 8])

dummy = 0

testoutputAsync = (dummy, dummy):> (1,2) :> (3,4):>(5,6):>(7,8):>(9,10):>(11,12):>(13,14):>((15,16)::(BitVector 8,BitVector 8)) :> Nil
testoutputSync =  (dummy, dummy):>(dummy,2) :> (3,4):>(5,6):>(7,8):>(9,10):>(11,12):>(13,14):>((15,16)::(BitVector 8,BitVector 8)) :> Nil



testBenchUS :: Signal SyncReal Bool
testBenchUS = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinput
    expectedOutput = outputVerifier   clkReal rstReal testoutputSync
    actualOutput   = ignoreFor clkReal rstReal enableGen d1 (dummy, dummy) (topEntityUS clkReal rstReal testInput)
    done           = expectedOutput actualOutput
    done'          = not <$> done

    clkDDR         = tbClockGen @SyncDDR (unsafeSynchronizer clkReal clkDDR done')
    clkReal        = tbClockGen @SyncReal done'
    rstDDR         = resetGen @SyncDDR
    rstReal        = resetGen @SyncReal

testBenchUA :: Signal AsyncReal Bool
testBenchUA = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinput
    expectedOutput = outputVerifier   clkReal rstReal testoutputAsync
    actualOutput   = ignoreFor clkReal rstReal enableGen d1 (dummy, dummy) (topEntityUA clkReal rstReal testInput)
    done           = expectedOutput actualOutput
    done'          = not <$> done

    clkDDR         = tbClockGen @AsyncDDR (unsafeSynchronizer clkReal clkDDR done')
    clkReal        = tbClockGen @AsyncReal done'
    rstDDR         = resetGen @AsyncDDR
    rstReal        = resetGen @AsyncReal

testBenchGS :: Signal SyncReal Bool
testBenchGS = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinput
    expectedOutput = outputVerifier   clkReal rstReal testoutputSync
    actualOutput   = ignoreFor clkReal rstReal enableGen d1 (dummy, dummy) (topEntityGS clkReal rstReal testInput)
    done           = expectedOutput actualOutput
    done'          = not <$> done

    clkDDR         = tbClockGen @SyncDDR (unsafeSynchronizer clkReal clkDDR done')
    clkReal        = tbClockGen @SyncReal done'
    rstDDR         = resetGen @SyncDDR
    rstReal        = resetGen @SyncReal

testBenchGA :: Signal AsyncReal Bool
testBenchGA = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinput
    expectedOutput = outputVerifier   clkReal rstReal testoutputAsync
    actualOutput   = ignoreFor clkReal rstReal enableGen d1 (dummy, dummy) (topEntityGA clkReal rstReal testInput)
    done           = expectedOutput actualOutput
    done'          = not <$> done

    clkDDR         = tbClockGen @AsyncDDR (unsafeSynchronizer clkReal clkDDR done')
    clkReal        = tbClockGen @AsyncReal done'
    rstDDR         = resetGen @AsyncDDR
    rstReal        = resetGen @AsyncReal
