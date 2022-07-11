{-# LANGUAGE NamedFieldPuns #-}

module DcFifoBroken where

import Clash.Cores.Xilinx.DcFifo
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench

import DcFifo.Abstract

topEntity :: ConfiguredFifo Dom2 Dom17
topEntity = dcFifo defConfig
{-# NOINLINE topEntity #-}

-- | Drive FIFO, ignore @full@ signals.
driveClumsy ::
  forall n dom depth .
  ( KnownDomain dom
  , KnownNat n ) =>
  Clock dom -> Reset dom -> Enable dom ->
  -- | Stall circuit? For this test case, this signal comes from 'lfsrF'
  Signal dom Stall ->
  -- | Signals from FIFO
  Signal dom (ResetBusy, Full, DataCount depth) ->
  -- | Maybe write input to FIFO
  Signal dom (Maybe (BitVector n))
driveClumsy clk rst ena stalls inps =
  mealyB clk rst ena go 0 (stalls, inps)
 where
  go ::
    BitVector n ->
    (Stall, (ResetBusy, Full, DataCount depth)) ->
    (BitVector n, Maybe (BitVector n))
  go n0 (_,     (True, _,    _         )) = (n0, Nothing)
  go n0 (stall, (_,    _, _dataCount)) = (n1, maybeWrite)
   where
    maybeWrite = willWrite `orNothing` n0
    willWrite = not stall
    n1 = if willWrite then succ n0 else n0

-- | To verify that the FIFO works even when we ignore the @full@ signal, we
-- simply that the actual value read off is never less than the expected value.
fifoVerifyBad::
  KnownDomain dom =>
  Clock dom -> Reset dom -> Enable dom ->
  Signal dom (Maybe (BitVector 16)) ->
  Signal dom Bool
fifoVerifyBad clk rst ena = mealy clk rst ena go 0
 where
  go :: BitVector 16 -> Maybe (BitVector 16) -> (BitVector 16, Bool)
  go expected Nothing = (expected, False)
  go expected (Just actual) = (succ actual, expected < actual)
{-# NOINLINE fifoVerifyBad #-}

testBench :: Signal Dom2 Bool
testBench = done
 where
  (rClk, wClk) = biTbClockGen (not <$> done)
  rRst = resetGen
  wRst = resetGen
  rEna = enableGen
  wEna = enableGen

  -- Driver
  wLfsr = bitToBool <$> lfsrF wClk wRst wEna 0xDEAD
  writeData = driveClumsy wClk wRst wEna wLfsr (bundle (writeReset, isFull, writeCount))

  -- Sampler
  rLfsr = bitToBool <$> lfsrF rClk rRst rEna 0xBEEF
  (readEnable, maybeReadData) =
    unbundle $
      fifoSampler rClk rRst rEna rLfsr (bundle (readReset, isEmpty, readCount, fifoData))

  XilinxFifo{writeReset, isFull, writeCount, readReset, isEmpty, readCount, fifoData} =
    topEntity wClk rClk rRst writeData readEnable

  errorFound = fifoVerifyBad rClk rRst rEna maybeReadData
  done = outputVerifier' rClk rRst (repeat @100 False) errorFound
