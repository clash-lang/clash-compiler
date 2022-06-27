{-# LANGUAGE OverloadedStrings #-}

module Test.Cores.Xilinx.DcFifo ( tests ) where

import qualified Prelude as P
import qualified Data.List as L
import Data.Maybe (catMaybes, fromMaybe)

import Clash.Explicit.Prelude
import Clash.Cores.Xilinx.DcFifo
import Clash.Netlist.Util (orNothing)


import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog (Gen, MonadGen, (===), property, forAll, Property)
import qualified Hedgehog as H
import Test.Tasty.Hedgehog (testPropertyNamed)
import Test.Tasty (testGroup, TestTree)

tests :: TestTree
tests = testGroup "FIFO tests"
  [ testPropertyNamed
      "FIFO doesn't lose any data with small stalls"
      "prop_noloss"
      prop_noloss
  , testPropertyNamed "FIFO preserves order" "prop_fifoOrder" prop_fifoOrder
  ]

intersperseStalls ::
  -- | Data
  [a] ->
  -- | Stalls
  [b] ->
  [Either b a]
intersperseStalls = go False
 where
  go True (d:ds) s = Right d : go False ds s
  go False ds (s:ss) = Left s : go True ds ss
  go _ [] s = Left <$> s
  go _ ds [] = Right <$> ds

genScenario ::
  (KnownNat n) =>
  -- | Number of elements to write and expect, @n@. For each write and read it
  -- may generate @n@ stall cycles. Test cases can therefore expect that all elements
  -- pass through the FIFO in @n * n * m@ cycles.
  Gen Int ->
  -- | Maximum stall size, @m@.
  Gen Int ->
  Gen ([BitVector n], [Either Int (BitVector n)], [Maybe Int])
genScenario genN genReadWrite = do
  n <- genN
  xs <- Gen.list (Range.singleton n) genData
  stallRead <- Gen.maybe (Gen.list (Range.singleton n) genReadWrite)
  stallWrite <- Gen.maybe (Gen.list (Range.singleton n) genReadWrite)
  let
    iWrites = fromMaybe [] stallWrite
    readStalls = maybe [] (fmap Just) stallRead
  pure (xs, intersperseStalls xs iWrites, L.intersperse Nothing readStalls)

-- | Assert that everything in @xs@ is in @ys@ and has the same relative rank.
orderFifo ::
  Eq a =>
  -- | @xs@
  [a] ->
  -- | @ys@
  [a] ->
  Bool
orderFifo [] [] = True
orderFifo (x:xs) (y:ys) | x == y = orderFifo xs ys
orderFifo xs (_:ys) = orderFifo xs ys
orderFifo _ _ = False

-- | Generated stalls are large enough that we expect loss, but order should be
-- preserved.
prop_fifoOrder :: Property
prop_fifoOrder = property $ do
  (xs, wrIn, rdStalls) <- forAll $ genScenario (Gen.int (Range.linear 30 40)) (Gen.int (Range.linear 10 15))
  H.assert (orderFifo (throughFifo wrIn rdStalls) xs)

-- | Generated stalls are small enough that we don't expect any data loss
prop_noloss :: Property
prop_noloss = property $ do
  (xs, wrIn, rdStalls) <- forAll $ genScenario (Gen.int (Range.linear 7 12)) (Gen.int (Range.linear 7 8))
  throughFifo wrIn rdStalls === xs

genData :: (KnownNat n, MonadGen m) => m (BitVector n)
genData = Gen.enumBounded

-- | Mealy machine which stalls reading from the FIFO based on ['Maybe'
-- 'Int']
takeState ::
  (Bool, [Maybe Int]) ->
  (ResetBusy, Empty, DataCount depth, BitVector n) ->
  ((Bool, [Maybe Int]), (Maybe (BitVector n), Bool))
takeState (_, stalls) (True, _, _, _) = ((False, stalls), (Nothing, False))
takeState (readLastCycle, Just 0:stalls) (_, _, _, d) =
    ((False, stalls), (nextData, False))
  where
    nextData = readLastCycle `orNothing` d
takeState (readLastCycle, Just n:stalls) (_, _, _, d) =
    ((False, Just (n-1):stalls), (nextData, False))
  where
    nextData = readLastCycle `orNothing` d
takeState (readLastCycle, stalls) (_, fifoEmpty, _, d) =
    ((readThisCycle, L.drop 1 stalls), (nextData, readThisCycle))
  where
    readThisCycle = not fifoEmpty
    nextData = readLastCycle `orNothing` d

feedState ::
  [Either Int (BitVector 32)] ->
  (ResetBusy, Full, DataCount 4) ->
  ([Either Int (BitVector 32)], Maybe (BitVector 32))
feedState xs (True, _, _) = (xs, Nothing)
feedState [] _ = ([], Nothing)
feedState (Left 0:xs) (_, _, _) = (xs, Nothing)
feedState (Left i:xs) (_, _, _) = (Left (i-1):xs, Nothing)
feedState (Right x:xs) (_, full, _) =
  if full
    then (Right x:xs, Nothing)
    else (xs, Just x)

throughFifo
  :: [Either Int (BitVector 32)] -- ^ Write data ('Left' 'Int' indicates a stall of duration @i@)
  -> [Maybe Int] -- ^ Read stalls
  -> [BitVector 32]
throughFifo wrDataList rdStalls = rdDataList
  where
    clk = clockGen @XilinxSystem
    rst = resetGen @XilinxSystem
    ena = enableGen @XilinxSystem
    wrData =
      -- The reset to the mealy machine must be the same reset fed to the FIFO
      mealyB clk rst ena feedState wrDataList (wrRstBusy, wrFull, wrCnt)
    (rdDataMaybe, rdEna) =
      mealyB clk rst ena takeState (False, rdStalls) (rdRstBusy, rdEmpty, rdCnt, rdData)

    rdDataList =
      catMaybes
        $ sampleN (P.length wrDataList*10)
        $ bundle rdDataMaybe

    (XilinxFifo wrRstBusy wrFull _ wrCnt rdRstBusy rdEmpty _ rdCnt rdData) =
      dcFifo defConfig clk clk rst wrData rdEna
