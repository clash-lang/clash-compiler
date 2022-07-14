{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-unused-top-binds #-}

module Test.Cores.Xilinx.DcFifo ( tests ) where

import qualified Prelude as P
import qualified Data.List as L
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))

import Clash.Explicit.Prelude
import Clash.Cores.Xilinx.DcFifo
import Clash.Netlist.Util (orNothing)

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Hedgehog (Gen, MonadGen, (===), property, forAll, Property)
import qualified Hedgehog as H
import Test.Tasty.Hedgehog (testPropertyNamed)
import Test.Tasty (testGroup, TestTree)

createDomain vSystem{vName="D3", vPeriod=3}
createDomain vSystem{vName="D5", vPeriod=5}
createDomain vSystem{vName="D11", vPeriod=11}

tests :: TestTree
tests = testGroup "FIFO tests"
  [ testPropertyNamed
      "FIFO doesn't lose any data with small stalls (wrT > rdT)"
      "prop_noloss"
      (prop_noloss (SNat @4) (Proxy @D3) (Proxy @D5))
  , testPropertyNamed
      "FIFO doesn't lose any data with small stalls (rdT > wrT)"
      "prop_noloss"
      (prop_noloss (SNat @4) (Proxy @D5) (Proxy @D3))
  , testPropertyNamed
      "FIFO doesn't lose any data with small stalls (rdT = wrT)"
      "prop_noloss"
      (prop_noloss (SNat @4) (Proxy @D3) (Proxy @D3))
  , testPropertyNamed
      "Large FIFO doesn't lose any data with (rdT > wrT)"
      "prop_noloss"
      (prop_noloss (SNat @17) (Proxy @D11) (Proxy @D3))
  , testPropertyNamed
      "FIFO preserves order (wrT = rdT)"
      "prop_fifoOrder"
      (prop_fifoOrder (SNat @4) (Proxy @D3) (Proxy @D3) feedState takeState)
  , testPropertyNamed
      "FIFO preserves order (wrT > rdT)"
      "prop_fifoOrder"
      (prop_fifoOrder (SNat @4) (Proxy @D3) (Proxy @D11) feedState takeState)
  , testPropertyNamed
      "FIFO preserves order (rdT > wrT)"
      "prop_fifoOrder"
      (prop_fifoOrder (SNat @4) (Proxy @D11) (Proxy @D3) feedState takeState)
  , testPropertyNamed
      "FIFO preserves order when writes don't respect overflow (rdT > wrT)"
      "prop_fifoOrder"
      (prop_fifoOrder (SNat @4) (Proxy @D11) (Proxy @D3) feedClumsy takeState)
  , testPropertyNamed
      "FIFO doesn't throw exceptions on underflow (wrT > rdT)"
      "prop_fifoOrder"
      (prop_noException (SNat @4) (Proxy @D3) (Proxy @D11) feedState takeClumsy)
  ]

-- | Must generate the same number of stalls and data
intersperseStalls ::
  -- | Data
  [a] ->
  -- | Stalls
  [b] ->
  [Either b a]
intersperseStalls d s = P.concat (P.zipWith (\x y -> [Left y, Right x]) d s)

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
  stallRead <- Gen.list (Range.singleton n) genReadWrite
  stallWrite <- Gen.list (Range.singleton n) genReadWrite
  let
    readStalls = fmap Just stallRead
  pure (xs, intersperseStalls xs stallWrite, L.intersperse Nothing readStalls)

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
prop_fifoOrder ::
  forall (read :: Symbol) (write :: Symbol) d.
  ( KnownDomain read
  , KnownDomain write
  , KnownNat d
  , 4 <= d
  , d <= 17
  ) =>
  SNat d ->
  Proxy read ->
  Proxy write ->
  Feed d ->
  Drain d 32 ->
  Property
prop_fifoOrder d pr pw feed drain = property $ do
  (xs, wrIn, rdStalls) <- forAll $ genScenario (Gen.int (Range.linear 30 40)) (Gen.int (Range.linear 10 15))
  let res = throughFifo d pr pw feed drain wrIn rdStalls
  H.annotate (show res)
  H.assert (orderFifo res xs)

-- | If we read after underflow, we still expect that the FIFO goes on.
prop_noException ::
  forall (read :: Symbol) (write :: Symbol) d.
  ( KnownDomain read
  , KnownDomain write
  , KnownNat d
  , 4 <= d
  , d <= 17
  ) =>
  SNat d ->
  Proxy read ->
  Proxy write ->
  Feed d ->
  Drain d 32 ->
  Property
prop_noException d pr pw feed drain = property $ do
  (_, wrIn, rdStalls) <- forAll $ genScenario (Gen.int (Range.linear 7 12)) (Gen.int (Range.linear 7 8))
  let res = throughFifo d pr pw feed drain wrIn rdStalls
  H.annotate (show res)
  H.assert (res `seq` True)

-- | Generated stalls are small enough that we don't expect any data loss
prop_noloss ::
  forall (read :: Symbol) (write :: Symbol) d.
  ( KnownDomain read
  , KnownDomain write
  , KnownNat d
  , 4 <= d
  , d <= 17
  ) =>
  SNat d -> Proxy read -> Proxy write -> Property
prop_noloss d pr pw = property $ do
  (xs, wrIn, rdStalls) <- forAll $ genScenario (Gen.int (Range.linear 7 12)) (Gen.int (Range.linear 7 8))
  throughFifo d pr pw feedState takeState wrIn rdStalls === xs

genData :: (KnownNat n, MonadGen m) => m (BitVector n)
genData = Gen.enumBounded

type Drain depth n =
  (Bool, [Maybe Int]) ->
  (ResetBusy, Empty, DataCount depth, BitVector n) ->
  ((Bool, [Maybe Int]), (Maybe (BitVector n), Bool))

-- | Mealy machine which stalls reading from the FIFO based on ['Maybe'
-- 'Int']
takeState ::
  (Bool, [Maybe Int]) ->
  (ResetBusy, Empty, DataCount depth, BitVector n) ->
  ((Bool, [Maybe Int]), (Maybe (BitVector n), Bool))
takeState (_, stalls) (True, _, _, _) = ((False, stalls), (Nothing, False))
takeState (readLastCycle, Just n:stalls) (_, _, _, d) | n > 0 =
    ((False, Just (n-1):stalls), (nextData, False))
  where
    nextData = readLastCycle `orNothing` d
takeState (readLastCycle, stalls) (_, fifoEmpty, _, d) =
    ((readThisCycle, L.drop 1 stalls), (nextData, readThisCycle))
  where
    readThisCycle = not fifoEmpty
    nextData = readLastCycle `orNothing` d

-- | Mealy machine which stalls reading from the FIFO based on ['Maybe'
-- 'Int']. This ignores @empty@ signals out of the FIFO.
takeClumsy ::
  (Bool, [Maybe Int]) ->
  (ResetBusy, Empty, DataCount depth, BitVector n) ->
  ((Bool, [Maybe Int]), (Maybe (BitVector n), Bool))
takeClumsy (_, stalls) (True, _, _, _) = ((False, stalls), (Nothing, False))
takeClumsy (readLastCycle, Just n:stalls) (_, _, _, d) | n > 0 =
    ((False, Just (n-1):stalls), (nextData, False))
  where
    nextData = readLastCycle `orNothing` d
takeClumsy (readLastCycle, stalls) (_, _, _, d) =
    ((True, L.drop 1 stalls), (nextData, True))
  where
    nextData = readLastCycle `orNothing` d

type Feed d =
  SNat d ->
  [Either Int (BitVector 32)] ->
  (ResetBusy, Full, DataCount d) ->
  ([Either Int (BitVector 32)], Maybe (BitVector 32))

-- | Driver for input to FIFO, takes stalls and data
feedState ::
  SNat d ->
  [Either Int (BitVector 32)] ->
  (ResetBusy, Full, DataCount d) ->
  ([Either Int (BitVector 32)], Maybe (BitVector 32))
feedState _ xs (True, _, _) = (xs, Nothing)
feedState _ [] _ = ([], Nothing)
feedState _ (Left 0:xs) (_, _, _) = (xs, Nothing)
feedState _ (Left i:xs) (_, _, _) = (Left (i-1):xs, Nothing)
feedState _ (Right x:xs) (_, full, _) =
  if full
    then (Right x:xs, Nothing)
    else (xs, Just x)

-- | Driver for input to FIFO, takes stalls and data and ignores full signals
-- out of the FIFO.
feedClumsy ::
  SNat d ->
  [Either Int (BitVector 32)] ->
  (ResetBusy, Full, DataCount d) ->
  ([Either Int (BitVector 32)], Maybe (BitVector 32))
feedClumsy _ xs (True, _, _) = (xs, Nothing)
feedClumsy _ [] _ = ([], Nothing)
feedClumsy _ (Left 0:xs) (_, _, _) = (xs, Nothing)
feedClumsy _ (Left i:xs) (_, _, _) = (Left (i-1):xs, Nothing)
feedClumsy _ (Right x:xs) (_, _, _) =
    (xs, Just x)

throughFifo
  :: forall (read :: Symbol) (write :: Symbol) d.
  ( KnownDomain read
  , KnownDomain write
  , KnownNat d
  , 4 <= d
  , d <= 17
  ) =>
  SNat d ->
  Proxy read ->
  Proxy write ->
  -- | Driver for write domain
  Feed d ->
  -- | Driver for read domain
  Drain d 32 ->
  -- | Write data ('Left' 'Int' indicates a stall of duration @i@)
  [Either Int (BitVector 32)] ->
  -- | Read stalls
  [Maybe Int] ->
  [BitVector 32]
throughFifo d _ _ feed drain wrDataList rdStalls = rdDataList
  where

    wrClk = clockGen @write
    wrRst = resetGen @write
    wrEna = enableGen @write

    rdClk = clockGen @read
    rdRst = resetGen @read
    rdEna = enableGen @read

    wrData =
      -- The reset to the mealy machine must be the same reset fed to the FIFO
      mealyB wrClk wrRst wrEna (feed d) wrDataList (wrRstBusy, wrFull, wrCnt)

    (rdDataMaybe, rdEnaB) =
      mealyB rdClk rdRst rdEna drain (False, rdStalls) (rdRstBusy, rdEmpty, rdCnt, rdData)

    rdDataList =
      catMaybes
        $ sampleN (P.length wrDataList*10)
        $ bundle rdDataMaybe

    (XilinxFifo wrRstBusy wrFull _ wrCnt rdRstBusy rdEmpty _ rdCnt rdData) =
      dcFifo defConfig wrClk rdClk rdRst wrData rdEnaB
