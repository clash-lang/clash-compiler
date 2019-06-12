module DDRin where

import Clash.Explicit.Prelude
import Clash.Explicit.DDR
import Clash.Explicit.Testbench (ignoreFor)
import Clash.Intel.DDR
import Clash.Xilinx.DDR

type DomReal = Dom "A" 2000 -- real clock domain
type DomDDR  = Dom "A" 1000 -- fake doublespeed domain, used to model a ddr signal

{-
The four variants defined here are all the combinations of
  clock: Gated  or Ungated
  reset: Asynch or Sync
-}

topEntityGeneric :: Clock DomReal gated
          -> Reset DomReal synchronous
          -> Signal DomDDR (BitVector 8)
          -> Signal DomReal (BitVector 8, BitVector 8)
topEntityGeneric clk rst = ddrIn clk rst (dummy,dummy,dummy)
-- topEntityGeneric clk rst = iddr clk rst
-- topEntityGeneric clk rst = altddioIn (SSymbol @"Cyclone IV GX") clk rst


topEntityUA :: Clock DomReal Source
          -> Reset DomReal Asynchronous
          -> Signal DomDDR (BitVector 8)
          -> Signal DomReal (BitVector 8, BitVector 8)
topEntityUA = topEntityGeneric

topEntityUS :: Clock DomReal Source
          -> Reset DomReal Synchronous
          -> Signal DomDDR (BitVector 8)
          -> Signal DomReal (BitVector 8, BitVector 8)
topEntityUS = topEntityGeneric

topEntityGA :: Clock DomReal Gated
          -> Reset DomReal Asynchronous
          -> Signal DomDDR (BitVector 8)
          -> Signal DomReal (BitVector 8, BitVector 8)
topEntityGA = topEntityGeneric

topEntityGS :: Clock DomReal Gated
          -> Reset DomReal Synchronous
          -> Signal DomDDR (BitVector 8)
          -> Signal DomReal (BitVector 8, BitVector 8)
topEntityGS = topEntityGeneric


testinput = $(listToVecTH [1..17::BitVector 8])

dummy = 0

testoutputAsync = (dummy, dummy):> (1,2) :> (3,4):>(5,6):>(7,8):>(9,10):>(11,12):>(13,14):>((15,16)::(BitVector 8,BitVector 8)) :> Nil
testoutputSync =  (dummy, dummy):>(dummy,2) :> (3,4):>(5,6):>(7,8):>(9,10):>(11,12):>(13,14):>((15,16)::(BitVector 8,BitVector 8)) :> Nil



testBenchUS :: Signal DomReal Bool
testBenchUS = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinput
    expectedOutput = outputVerifier   clkReal rstReal testoutputSync
    actualOutput   = ignoreFor clkReal rstReal d1 (dummy, dummy) (topEntityUS clkReal rstReal testInput)
    done           = expectedOutput actualOutput
    done'          = not <$> done

    clkDDR         = tbClockGen @DomDDR (unsafeSynchronizer clkReal clkDDR done')
    clkReal        = tbClockGen @DomReal done'
    rstDDR         = syncResetGen @DomDDR
    rstReal        = syncResetGen @DomReal

testBenchUA :: Signal DomReal Bool
testBenchUA = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinput
    expectedOutput = outputVerifier   clkReal rstReal testoutputAsync
    actualOutput   = ignoreFor clkReal rstReal d1 (dummy, dummy) (topEntityUA clkReal rstReal testInput)
    done           = expectedOutput actualOutput
    done'          = not <$> done

    clkDDR         = tbClockGen @DomDDR (unsafeSynchronizer clkReal clkDDR done')
    clkReal        = tbClockGen @DomReal done'
    rstDDR         = asyncResetGen @DomDDR
    rstReal        = asyncResetGen @DomReal

testBenchGS :: Signal DomReal Bool
testBenchGS = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinput
    expectedOutput = outputVerifier   clkReal rstReal testoutputSync
    actualOutput   = ignoreFor clkReal rstReal d1 (dummy, dummy) (topEntityGS clkReal rstReal testInput)
    done           = expectedOutput actualOutput
    done'          = not <$> done

    clkDDR         = let c = tbClockGen @DomDDR (unsafeSynchronizer clkReal clkDDR done') in clockGate c $ pure True
    clkReal        = let c = tbClockGen @DomReal done' in clockGate c $ pure True
    rstDDR         = syncResetGen @DomDDR
    rstReal        = syncResetGen @DomReal

testBenchGA :: Signal DomReal Bool
testBenchGA = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinput
    expectedOutput = outputVerifier   clkReal rstReal testoutputAsync
    actualOutput   = ignoreFor clkReal rstReal d1 (dummy, dummy) (topEntityGA clkReal rstReal testInput)
    done           = expectedOutput actualOutput
    done'          = not <$> done

    clkDDR         = let c = tbClockGen @DomDDR (unsafeSynchronizer clkReal clkDDR done') in clockGate c $ pure True
    clkReal        = let c = tbClockGen @DomReal done' in clockGate c $ pure True
    rstDDR         = asyncResetGen @DomDDR
    rstReal        = asyncResetGen @DomReal
