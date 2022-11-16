{-# OPTIONS_GHC -Wno-orphans #-}

module DDRin where

import Clash.Explicit.Prelude
import Clash.Explicit.DDR
import Clash.Explicit.Testbench

--createDomain vSystem{vName="AsyncReal", vPeriod=2000, vResetKind=Asynchronous}  -- real clock domain (same as TB)
createDomain vSystem{vName="AsyncDDR", vPeriod=1000, vResetKind=Asynchronous}  -- fake ddr domain
createDomain vSystem{vName="SyncReal", vPeriod=2000, vResetKind=Synchronous}  -- real clock domain
createDomain vSystem{vName="SyncDDR", vPeriod=1000, vResetKind=Synchronous}  -- fake ddr domain

-- | Domain used for testbench itself
createDomain vSystem{vName="TB", vPeriod=2000, vResetKind=Asynchronous}
type AsyncReal = TB

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
{-# NOINLINE topEntityUA #-}
{-# ANN topEntityUA (defSyn "topEntityUA") #-}

topEntityUS
  :: Clock SyncReal
  -> Reset SyncReal
  -> Signal SyncDDR (BitVector 8)
  -> Signal SyncReal (BitVector 8, BitVector 8)
topEntityUS clk rst = topEntityGeneric clk rst enableGen
{-# NOINLINE topEntityUS #-}
{-# ANN topEntityUS (defSyn "topEntityUS") #-}


topEntityGA
  :: Clock AsyncReal
  -> Reset AsyncReal
  -> Signal AsyncDDR (BitVector 8)
  -> Signal AsyncReal (BitVector 8, BitVector 8)
topEntityGA clk rst = topEntityGeneric clk rst tbEnableGen
{-# NOINLINE topEntityGA #-}
{-# ANN topEntityGA (defSyn "topEntityGA") #-}


topEntityGS
  :: Clock SyncReal
  -> Reset SyncReal
  -> Signal SyncDDR (BitVector 8)
  -> Signal SyncReal (BitVector 8, BitVector 8)
topEntityGS clk rst = topEntityGeneric clk rst tbEnableGen
{-# NOINLINE topEntityGS #-}
{-# ANN topEntityGS (defSyn "topEntityGS") #-}


testinput :: Vec 17 (BitVector 8)
testinput = $(listToVecTH [1..17 :: BitVector 8])

dummy :: BitVector 8
dummy = 0

testoutput :: Vec 9 (BitVector 8, BitVector 8)
testoutput = (dummy,dummy):>(dummy,2):>(3,4):>(5,6):>(7,8):>(9,10):>(11,12):>(13,14):>((15,16)::(BitVector 8,BitVector 8)) :> Nil



testBenchUS :: Signal TB Bool
testBenchUS = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinput
    expectedOutput = outputVerifier clkTest clkReal rstTest testoutput
    actualOutput   = ignoreFor clkReal rstReal enableGen d1 (dummy, dummy) (topEntityUS clkReal rstReal testInput)
    done           = expectedOutput actualOutput
    notDone        = not <$> done

    clkTest        = tbClockGen @TB notDone
    clkDDR         = tbClockGen @SyncDDR (unsafeSynchronizer clkTest clkDDR notDone)
    clkReal        = tbClockGen @SyncReal (unsafeSynchronizer clkTest clkReal notDone)

    rstTest        = resetGen @TB
    rstDDR         = resetGen @SyncDDR
    rstReal        = resetGen @SyncReal
{-# NOINLINE testBenchUS #-}
{-# ANN testBenchUS (TestBench 'topEntityUS) #-}

testBenchUA :: Signal TB Bool
testBenchUA = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinput
    expectedOutput = outputVerifier'   clkReal rstReal testoutput
    actualOutput   = ignoreFor clkReal rstReal enableGen d1 (dummy, dummy) (topEntityUA clkReal rstReal testInput)
    done           = expectedOutput actualOutput
    notDone        = not <$> done

    clkDDR         = tbClockGen @AsyncDDR (unsafeSynchronizer clkReal clkDDR notDone)
    clkReal        = tbClockGen @AsyncReal notDone
    rstDDR         = resetGen @AsyncDDR
    rstReal        = resetGen @AsyncReal
{-# NOINLINE testBenchUA #-}
{-# ANN testBenchUA (TestBench 'topEntityUA) #-}

testBenchGS :: Signal TB Bool
testBenchGS = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinput
    expectedOutput = outputVerifier clkTest clkReal rstTest testoutput
    actualOutput   = ignoreFor clkReal rstReal enableGen d1 (dummy, dummy) (topEntityGS clkReal rstReal testInput)
    done           = expectedOutput actualOutput
    notDone        = not <$> done

    clkTest        = tbClockGen @TB notDone
    clkDDR         = tbClockGen @SyncDDR (unsafeSynchronizer clkTest clkDDR notDone)
    clkReal        = tbClockGen @SyncReal (unsafeSynchronizer clkTest clkReal notDone)

    rstTest        = resetGen @TB
    rstDDR         = resetGen @SyncDDR
    rstReal        = resetGen @SyncReal
{-# NOINLINE testBenchGS #-}
{-# ANN testBenchGS (TestBench 'topEntityGS) #-}

testBenchGA :: Signal TB Bool
testBenchGA = done
  where
    testInput      = stimuliGenerator clkDDR rstDDR testinput
    expectedOutput = outputVerifier'   clkReal rstReal testoutput
    actualOutput   = ignoreFor clkReal rstReal enableGen d1 (dummy, dummy) (topEntityGA clkReal rstReal testInput)
    done           = expectedOutput actualOutput
    notDone        = not <$> done

    clkDDR         = tbClockGen @AsyncDDR (unsafeSynchronizer clkReal clkDDR notDone)
    clkReal        = tbClockGen @AsyncReal notDone
    rstDDR         = resetGen @AsyncDDR
    rstReal        = resetGen @AsyncReal
{-# NOINLINE testBenchGA #-}
{-# ANN testBenchGA (TestBench 'topEntityGA) #-}
