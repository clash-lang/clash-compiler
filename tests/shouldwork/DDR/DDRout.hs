module DDRout where

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
          -> Signal DomReal (BitVector 8,BitVector 8)
          -> Signal DomDDR (BitVector 8)
topEntityGeneric clk rst = ddrOut clk rst 0
-- topEntityGeneric = oddr
-- topEntityGeneric = altddioOut (SSymbol @"Cyclone IV GX")


topEntityUA :: Clock DomReal Source
          -> Reset DomReal Asynchronous
          -> Signal DomReal (BitVector 8,BitVector 8)
          -> Signal DomDDR (BitVector 8)
topEntityUA = topEntityGeneric

topEntityUS :: Clock DomReal Source
          -> Reset DomReal Synchronous
          -> Signal DomReal (BitVector 8,BitVector 8)
          -> Signal DomDDR (BitVector 8)
topEntityUS = topEntityGeneric

topEntityGA :: Clock DomReal Gated
          -> Reset DomReal Asynchronous
          -> Signal DomReal (BitVector 8,BitVector 8)
          -> Signal DomDDR (BitVector 8)
topEntityGA = topEntityGeneric

topEntityGS :: Clock DomReal Gated
          -> Reset DomReal Synchronous
          -> Signal DomReal (BitVector 8,BitVector 8)
          -> Signal DomDDR (BitVector 8)
topEntityGS = topEntityGeneric

input = ((0,1):>(2,3):>(4,5):>(6,7):>((8,9)::(BitVector 8,BitVector 8)) :> Nil)
expected = dummy:>dummy:> 0:>1:>2:>3:>4:>5:>6:>7:>8:>(9 :: BitVector 8) :> Nil

dummy = 0

testBenchUS :: Signal DomDDR Bool
testBenchUS = done
  where
    testInput      = stimuliGenerator clkReal rstReal input
    actualOutput   = ignoreFor clkDDR rstDDR d1 dummy (topEntityUS clkReal rstReal testInput)
    expectedOutput = outputVerifier clkDDR rstDDR expected
    done           = expectedOutput actualOutput
    done'          = not <$> done
    clkDDR         = tbClockGen @DomDDR done'
    clkReal        = tbClockGen @DomReal (unsafeSynchronizer clkDDR clkReal done')
    rstDDR         = syncResetGen @DomDDR
    rstReal        = syncResetGen @DomReal

testBenchUA :: Signal DomDDR Bool
testBenchUA = done
  where
    testInput      = stimuliGenerator clkReal rstReal input
    actualOutput   = ignoreFor clkDDR rstDDR d1 dummy (topEntityUA clkReal rstReal testInput)
    expectedOutput = outputVerifier clkDDR rstDDR expected
    done           = expectedOutput actualOutput
    done'          = not <$> done
    clkDDR         = tbClockGen @DomDDR done'
    clkReal        = tbClockGen @DomReal (unsafeSynchronizer clkDDR clkReal done')
    rstDDR         = asyncResetGen @DomDDR
    rstReal        = asyncResetGen @DomReal

testBenchGA :: Signal DomDDR Bool
testBenchGA = done
  where
    testInput      = stimuliGenerator clkReal rstReal input
    actualOutput   = ignoreFor clkDDR rstDDR d1 dummy (topEntityGA clkReal rstReal testInput)
    expectedOutput = outputVerifier clkDDR rstDDR expected
    done           = expectedOutput actualOutput
    done'          = not <$> done
    clkDDR         = let c = tbClockGen @DomDDR done' in clockGate c (pure True)
    clkReal        = let c = tbClockGen @DomReal (unsafeSynchronizer clkDDR clkReal done') in clockGate c (pure True)
    rstDDR         = asyncResetGen @DomDDR
    rstReal        = asyncResetGen @DomReal

testBenchGS :: Signal DomDDR Bool
testBenchGS = done
  where
    testInput      = stimuliGenerator clkReal rstReal input
    actualOutput   = ignoreFor clkDDR rstDDR d1 dummy (topEntityGS clkReal rstReal testInput)
    expectedOutput = outputVerifier clkDDR rstDDR expected
    done           = expectedOutput actualOutput
    done'          = not <$> done
    clkDDR         = let c = tbClockGen @DomDDR done' in clockGate c (pure True)
    clkReal        = let c = tbClockGen @DomReal (unsafeSynchronizer clkDDR clkReal done') in clockGate c (pure True)
    rstDDR         = syncResetGen @DomDDR
    rstReal        = syncResetGen @DomReal
