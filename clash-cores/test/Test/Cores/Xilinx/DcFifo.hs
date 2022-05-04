{-# LANGUAGE OverloadedStrings #-}

module Test.Cores.Xilinx.DcFifo ( tests ) where

import qualified Prelude as P
import qualified Data.List as L
import Data.Maybe (catMaybes)
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

intersperseStalls ::
  -- | Data
  [a] ->
  -- | Stalls
  [a] ->
  [a]
intersperseStalls (d:ds) s = d:s P.++ intersperseStalls ds s
intersperseStalls [] _ = []

prop_fifo :: Property
prop_fifo = property $ do
  xs <- fmap Just <$> forAll (replicateM 10 genData)
  stallRead <- forAll (Gen.maybe (Gen.int (Range.linear 0 10)))
  stallWrite <- forAll (Gen.maybe (Gen.int (Range.linear 0 10)))
  let iWrites = case stallRead of
        Nothing -> []
        Just i -> P.replicate i Nothing
  let readStalls = case stallWrite of
        Nothing -> [False]
        Just i -> False : L.replicate i True
  throughFifo (intersperseStalls xs iWrites) (cycle readStalls) === catMaybes xs

genData :: (KnownNat n, MonadGen m) => m (BitVector n)
genData = Gen.enumBounded

takeState ::
  (Bool, [Bool]) ->
  (ResetBusy, Empty, DataCount depth, BitVector n) ->
  ((Bool, [Bool]), (Maybe (BitVector n), Bool))
takeState (_,[]) _ = error "Unreachable."
takeState (_, _:stalls) (1, _, _, _) = ((False, stalls), (Nothing, False))
takeState (readLastCycle, True:stalls) (_, _, _, d) =
    ((False, stalls), (nextData, False))
  where
    nextData = if readLastCycle then Just d else Nothing
takeState (readLastCycle, _:stalls) (_, fifoEmpty, _, d) =
    ((readThisCycle, stalls), (nextData, readThisCycle))
  where
    readThisCycle = fifoEmpty == low
    nextData = if readLastCycle then Just d else Nothing

feedState ::
  [Maybe (BitVector 32)] ->
  (ResetBusy, Full, DataCount 4) ->
  ([Maybe (BitVector 32)], (BitVector 32, Bool))
feedState xs (1, _, _) = (xs, (deepErrorX "Resetting", False))
feedState [] _ = ([], (deepErrorX "No more data", False))
feedState (Nothing:xs) (_, _, _) = (xs, (deepErrorX "Stall simulation", False))
feedState (Just x:xs) (_, full, _) =
  if full == high
    then (Just x:xs, (deepErrorX "FIFO full, waiting", False))
    else (xs, (x, True))

throughFifo
  :: [Maybe (BitVector 32)] -- ^ Write data ('Nothing' for stalls)
  -> [Bool] -- ^ Read stalls
  -> [BitVector 32]
throughFifo wrDataList rdStalls = rdDataList
  where
    clk = clockGen @System
    rst = resetGen @System
    ena = enableGen @System
    (wrData, wrEna) =
      mealyB clk rst ena feedState wrDataList (wrRstBusy, wrFull, wrCnt)
    (rdDataMaybe, rdEna) =
      mealyB clk rst ena takeState (False, rdStalls) (rdRstBusy, rdEmpty, rdCnt, rdData)

    rdDataList =
      catMaybes
        $ sampleN (P.length wrDataList*10)
        $ bundle rdDataMaybe

    (XilinxFifo wrRstBusy wrFull wrCnt rdRstBusy rdEmpty rdCnt rdData) =
      dcFifo defConfig clk clk rst wrData wrEna rdEna
