{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lfsr where

import Clash.Explicit.Prelude
import Clash.Cores.Xilinx.DcFifo
import Clash.Explicit.Testbench
import Data.Maybe (isJust)

type ReadLastCycle = Bool
type Stall = Bool
type ExpectedToRead n = BitVector n
type UnexpectedRead = Bool

createDomain vXilinxSystem{vName="Dom2", vPeriod=hzToPeriod 2e7}
createDomain vXilinxSystem{vName="Dom3", vPeriod=hzToPeriod 3e7}
createDomain vXilinxSystem{vName="Dom17", vPeriod=hzToPeriod 17e7}

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
  -- | Stall circuit? For this test case, this signal comes from 'lfsrF'
  Signal dom Stall ->
  -- | Signals from FIFO
  Signal dom (Empty, DataCount depth, a) ->
  -- | Maybe output read from FIFO
  Signal dom (Bool, Maybe a)
fifoSampler clk rst ena stalls inps =
  mealy clk rst ena go False (bundle (stalls, inps))
 where
  go ::
    ReadLastCycle ->
    (Stall, (Empty, DataCount depth, a)) ->
    (ReadLastCycle, (Bool, Maybe a))
  go readLastCycle (stall, (fifoEmpty, _dataCount, readData)) = (readNow, (readNow, maybeData))
   where
    maybeData = readLastCycle `orNothing` readData
    readNow = not stall && not fifoEmpty
{-# NOINLINE fifoSampler #-}

-- | Drives Xilinx FIFO with an ascending sequence of 'BitVector's. Stalls
-- intermittently based on stall input.
fifoDriver ::
  forall a dom depth .
  ( KnownDomain dom
  , NFDataX a
  , Enum a
  , Num a
  ) =>
  Clock dom -> Reset dom -> Enable dom ->
  -- | Stall circuit? For this test case, this signal comes from 'lfsrF'
  Signal dom Stall ->
  -- | Signals from FIFO
  Signal dom (Full, DataCount depth) ->
  -- | Maybe write input to FIFO
  Signal dom (Maybe a)
fifoDriver clk rst ena stalls inps =
  mealyB clk rst ena go 0 (stalls, inps)
 where
  go ::
    a ->
    (Stall, (Full, DataCount depth)) ->
    (a, Maybe a)
  go n0 (stall, (full, _dataCount)) = (n1, maybeWrite)
   where
    maybeWrite = willWrite `orNothing` n0
    willWrite = not stall && not full
    n1 = if willWrite then succ n0 else n0

type ConfiguredFifo a read write =
  Clock write ->
  Reset write ->
  Clock read ->
  Reset read ->

  -- | Write data
  Signal write (Maybe a) ->
  -- | Read enable
  Signal read Bool ->
  FifoOut read write 4 a

mkTestBench ::
  forall a read write.
  ( Num a
  , Enum a
  , NFDataX a
  , Ord a
  , ShowX a
  , KnownDomain write
  , KnownDomain read
  ) =>
  ConfiguredFifo a read write ->
  Signal read Bool
mkTestBench cFifo = done
 where
  (rClk, wClk) = biTbClockGen (not <$> done)

  noRRst = unsafeFromHighPolarity $ pure False
  noWRst = unsafeFromHighPolarity $ pure False

  rEna = enableGen
  wEna = enableGen

  -- Driver
  wLfsr = bitToBool <$> lfsrF wClk noWRst wEna 0xDEAD
  writeData = fifoDriver wClk noWRst wEna wLfsr (bundle (isFull, writeCount))

  -- Sampler
  rLfsr = bitToBool <$> lfsrF rClk noRRst rEna 0xBEEF
  (readEnable, maybeReadData) =
    unbundle $
      fifoSampler rClk noRRst rEna rLfsr (bundle (isEmpty, readCount, fifoData))

  FifoOut{isFull, writeCount, isEmpty, readCount, fifoData} =
    cFifo wClk noWRst rClk noRRst writeData readEnable

  done = fifoVerifier rClk noRRst rEna maybeReadData
{-# INLINE mkTestBench #-}

fifoVerifier ::
  forall a dom .
  ( KnownDomain dom
  , Ord a
  , Num a
  , NFDataX a
  , ShowX a
  ) =>
  Clock dom -> Reset dom -> Enable dom ->
  Signal dom (Maybe a) ->
  Signal dom Bool
fifoVerifier clk rst ena actual = done0
 where
  expected = regEn clk rst ena 0 (isJust <$> actual) $ expected + 1
  samplesDone = expected .>. 100
  stuckCnt :: Signal dom (Index 25000)
  stuckCnt = regEn clk rst ena 0 (not <$> stuck) $ stuckCnt + 1
  stuck = stuckCnt .==. pure maxBound
  -- Delay one cycle so assertion definitely triggers before stopping simulation
  done = register clk rst ena False $ samplesDone .||. stuck
  expected0 = liftA2 (<$) expected actual
  done0 =
    assert clk rst "Doesn't time out" stuck (pure False) $
      assert clk rst "fifoVerifier" actual expected0 done
{-# NOINLINE fifoVerifier #-}

topEntity_17_2 :: ConfiguredFifo (BitVector 16) Dom17 Dom2
topEntity_17_2 = dcFifo defConfig
{-# NOINLINE topEntity_17_2 #-}
{-# ANN topEntity_17_2 (defSyn "topEntity_17_2") #-}

testBench_17_2 :: Signal Dom17 Bool
testBench_17_2 = mkTestBench topEntity_17_2
{-# NOINLINE testBench_17_2 #-}
{-# ANN testBench_17_2 (TestBench 'topEntity_17_2) #-}

topEntity_2_17 :: ConfiguredFifo (BitVector 16) Dom2 Dom17
topEntity_2_17 = dcFifo defConfig
{-# NOINLINE topEntity_2_17 #-}
{-# ANN topEntity_2_17 (defSyn "topEntity_2_17") #-}

testBench_2_17 :: Signal Dom2 Bool
testBench_2_17 = mkTestBench topEntity_2_17
{-# NOINLINE testBench_2_17 #-}
{-# ANN testBench_2_17 (TestBench 'topEntity_2_17) #-}

topEntity_2_2 :: ConfiguredFifo (Unsigned 16) Dom2 Dom2
topEntity_2_2 = dcFifo defConfig
{-# NOINLINE topEntity_2_2 #-}
{-# ANN topEntity_2_2 (defSyn "topEntity_2_2") #-}

testBench_2_2 :: Signal Dom2 Bool
testBench_2_2 = mkTestBench topEntity_2_2
{-# NOINLINE testBench_2_2 #-}
{-# ANN testBench_2_2 (TestBench 'topEntity_2_2) #-}
