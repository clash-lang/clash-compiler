{-# LANGUAGE OverloadedStrings #-}

module Test.Cores.Xilinx.DcFifo ( tests ) where

import qualified Prelude as P
import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad (replicateM)

import Clash.Explicit.Prelude
import Clash.Cores.Xilinx.DcFifo.Explicit

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog (MonadGen, (===), property, forAll, Property)
import Test.Tasty.Hedgehog (testPropertyNamed)
import Test.Tasty (TestTree)

tests :: TestTree
tests = testPropertyNamed
  "FIFO doesn't lose any data"
  "prop_fifo"
  prop_fifo

prop_fifo :: Property
prop_fifo = property $ do
  xs <- forAll (replicateM 10 genData)
  throughFifo xs === xs

-- see https://github.com/clash-lang/clash-protocols/blob/main/src/Protocols/Hedgehog.hs

genStall :: MonadGen m => m Int
genStall = Gen.integral (Range.linear 0 5)

genData :: (KnownNat n, MonadGen m) => m (BitVector n)
genData = Gen.enumBounded

takeState ::
  -- whether we asked for read last cycle
  Bool ->
  (ResetBusy, Empty, DataCount depth, BitVector n) ->
  (Bool, (Maybe (BitVector n), Bool))
takeState _ (1, _, _, _) = (False, (Nothing, False))
takeState readLastCycle (_, fifoEmpty, _, d) = (readThisCycle, (nextData, readThisCycle))
  where
    readThisCycle = fifoEmpty == low
    nextData = if readLastCycle then Just d else Nothing

feedState ::
  [BitVector 32] ->
  (ResetBusy, Full, DataCount 5) ->
  ([BitVector 32], (BitVector 32, Bool))
feedState xs (1, _, _) = (xs, (deepErrorX "Resetting", False))
feedState [] _ = ([], (deepErrorX "No more data", False))
feedState (x:xs) (_, full, _) =
  if full == high
    then (x:xs, (deepErrorX "FIFO full, waiting", False))
    else (xs, (x, True))

throughFifo
  :: [BitVector 32] -- ^ Write data
  -> [BitVector 32]
throughFifo wrDataList = rdDataList
  where
    clk = clockGen @System
    rst = resetGen @System
    ena = enableGen @System
    (wrData, wrEna) = mealyB clk rst ena feedState wrDataList (wrRstBusy, wrFull, wrCnt)
    (rdDataMaybe, rdEna) = mealyB clk rst ena takeState False (rdRstBusy, rdEmpty, rdCnt, rdData)

    rdDataList = catMaybes $ sampleN (P.length wrDataList*10) $ bundle rdDataMaybe

    (wrRstBusy, wrFull, wrCnt, rdRstBusy, rdEmpty, rdCnt, rdData) =
      dcFifo defConfig clk clk rst wrData wrEna rdEna
