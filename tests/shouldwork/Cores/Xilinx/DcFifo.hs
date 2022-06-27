{-# LANGUAGE NamedFieldPuns #-}

module DcFifo where

import Clash.Explicit.Prelude
import Clash.Cores.Xilinx.DcFifo
import Clash.Explicit.Testbench
import Data.Maybe (isJust)

type ReadLastCycle = Bool
type Stall = Bool
type ExpectedToRead n = BitVector n
type UnexpectedRead = Bool

type WriteDom = XilinxSystem
type ReadDom = XilinxSystem

-- | Produce a 'Just' when predicate is True, else Nothing
orNothing :: Bool -> a -> Maybe a
orNothing True a = Just a
orNothing False _ = Nothing

lfsrF ::
  KnownDomain dom =>
  Clock dom -> Reset dom -> Enable dom ->
  BitVector 16 ->
  Signal dom Bit
lfsrF clk rst ena seed = msb <$> r
 where
  r = register clk rst ena seed (lfsrF' <$> r)

  lfsrF' :: BitVector 16 -> BitVector 16
  lfsrF' s = pack lfsrFeedback ++# slice d15 d1 s
   where
    five, three, two, zero :: Unsigned 16
    (five, three, two, zero) = (5, 3, 2, 0)
    lfsrFeedback = s ! five `xor` s ! three `xor` s ! two `xor` s ! zero
{-# NOINLINE lfsrF #-}

fifoSampler ::
  KnownDomain dom =>
  Clock dom -> Reset dom -> Enable dom ->
  -- | Stall circuit? [note: probably coming from an LSFR, see `Clash.Examples`]
  Signal dom Stall ->
  -- | Signals from FIFO
  Signal dom (ResetBusy, Empty, DataCount depth, BitVector n) ->
  -- | Maybe output read from FIFO
  Signal dom (Bool, Maybe (BitVector n))
fifoSampler clk rst ena stalls inps =
  mealy clk rst ena go False (bundle (stalls, inps))
 where
  go ::
    ReadLastCycle ->
    (Stall, (ResetBusy, Empty, DataCount depth, BitVector n)) ->
    (ReadLastCycle, (Bool, Maybe (BitVector n)))
  go _             (_,     (True,      _,     _,         _       )) = (False, (False, Nothing))
  go readLastCycle (stall, (_resetBusy, fifoEmpty, _dataCount, readData)) = (readNow, (readNow, maybeData))
   where
    maybeData = readLastCycle `orNothing` readData
    readNow = not stall && not fifoEmpty
{-# NOINLINE fifoSampler #-}

-- | Drives Xilinx FIFO with an ascending sequence of 'BitVector's. Stalls
-- intermittently based on stall input.
fifoDriver ::
  forall n dom depth .
  ( KnownDomain dom
  , KnownNat n ) =>
  Clock dom -> Reset dom -> Enable dom ->
  -- | Stall circuit? [note: probably coming from an LSFR, see `Clash.Examples`]
  Signal dom Stall ->
  -- | Signals from FIFO
  Signal dom (ResetBusy, Full, DataCount depth) ->
  -- | Maybe write input to FIFO
  Signal dom (Maybe (BitVector n))
fifoDriver clk rst ena stalls inps =
  mealyB clk rst ena go 0 (stalls, inps)
 where
  go ::
    BitVector n ->
    (Stall, (ResetBusy, Full, DataCount depth)) ->
    (BitVector n, Maybe (BitVector n))
  go n0 (_,     (True, _,    _         )) = (n0, Nothing)
  go n0 (stall, (_,    full, _dataCount)) = (n1, maybeWrite)
   where
    maybeWrite = willWrite `orNothing` n0
    willWrite = not stall && not full
    n1 = if willWrite then succ n0 else n0

topEntity ::
  Clock WriteDom -> Clock ReadDom ->
  -- | Asynchronous reset
  Reset ReadDom ->

  -- | Write data
  Signal WriteDom (Maybe (BitVector 16)) ->
  -- | Read enable
  Signal ReadDom Bool ->
  XilinxFifo ReadDom WriteDom 4 16
topEntity = dcFifo defConfig
{-# NOINLINE topEntity #-}

testBench :: Signal ReadDom Bool
testBench = done
 where
  (rClk, wClk) = biTbClockGen (not <$> done)
  rRst = resetGen
  wRst = resetGen
  rEna = enableGen
  wEna = enableGen

  -- Driver
  wLfsr = bitToBool <$> lfsrF wClk wRst wEna 0xDEAD
  writeData = fifoDriver wClk wRst wEna wLfsr (bundle (writeReset, isFull, writeCount))

  -- Sampler
  rLfsr = bitToBool <$> lfsrF rClk rRst rEna 0xBEEF
  (readEnable, maybeReadData) =
    unbundle $
      fifoSampler rClk rRst rEna rLfsr (bundle (readReset, isEmpty, readCount, fifoData))

  XilinxFifo{writeReset, isFull, writeCount, readReset, isEmpty, readCount, fifoData} =
    topEntity rClk wClk rRst writeData readEnable

  errorFound = fifoVerifier rClk rRst rEna maybeReadData
  done = outputVerifier' rClk rRst (repeat @100 False) errorFound
{-# NOINLINE testBench #-}

fifoVerifier ::
  KnownDomain dom =>
  Clock dom -> Reset dom -> Enable dom ->
  Signal dom (Maybe (BitVector 16)) ->
  Signal dom Bool
fifoVerifier clk rst ena = mealy clk rst ena go 0
 where
  go :: BitVector 16 -> Maybe (BitVector 16) -> (BitVector 16, Bool)
  go expected Nothing = (expected, False)
  go expected (Just actual) = (succ actual, expected /= actual)
{-# NOINLINE fifoVerifier #-}
