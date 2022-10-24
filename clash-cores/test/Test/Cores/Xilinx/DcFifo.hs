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
import Test.Tasty.HUnit (testCase, assertBool)
import Test.Tasty (testGroup, TestTree)

createDomain vXilinxSystem{vName="D3", vPeriod=3}
createDomain vXilinxSystem{vName="D5", vPeriod=5}
createDomain vXilinxSystem{vName="D11", vPeriod=11}

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
  , testOverflow
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

testOverflow :: TestTree
testOverflow = testCase "Overflows appropriately" $ do
  assertBool "Overflows if we never read" (or (sampleN 20 wrOver0))
  assertBool "Doesn't overflow if we write exactly 15 values"
    (not (or (sampleN 20 wrOver1)))
 where
  (FifoOut _ wrOver0 _ _ _ _ _) = fifo (pure (Just (1 :: Int))) (pure False)
  (FifoOut _ wrOver1 _ _ _ _ _) = fifo drive15 (pure False)

  fifo = dcFifo @4 defConfig{dcOverflow = True} clk noRst clk noRst
  clk = clockGen @D3
  noRst = unsafeFromHighPolarity $ pure False
  drive15 = mealy clk noRst enableGen go 15 (pure ())
  go 0 _ = (0 :: Int, Nothing)
  go n _ = (n-1, Just (1 :: Int))

genData :: (KnownNat n, MonadGen m) => m (BitVector n)
genData = Gen.enumBounded

type Drain depth n =
  (Bool, [Maybe Int]) ->
  (Empty, DataCount depth, BitVector n) ->
  ((Bool, [Maybe Int]), (Maybe (BitVector n), Bool))

-- | Mealy machine which stalls reading from the FIFO based on ['Maybe'
-- 'Int']
takeState ::
  (Bool, [Maybe Int]) ->
  (Empty, DataCount depth, BitVector n) ->
  ((Bool, [Maybe Int]), (Maybe (BitVector n), Bool))
takeState (readLastCycle, Just n:stalls) (_, _, d) | n > 0 =
    ((False, Just (n-1):stalls), (nextData, False))
  where
    nextData = readLastCycle `orNothing` d
takeState (readLastCycle, stalls) (fifoEmpty, _, d) =
    ((readThisCycle, L.drop 1 stalls), (nextData, readThisCycle))
  where
    readThisCycle = not fifoEmpty
    nextData = readLastCycle `orNothing` d

-- | Mealy machine which stalls reading from the FIFO based on ['Maybe'
-- 'Int']. This ignores @empty@ signals out of the FIFO.
takeClumsy ::
  (Bool, [Maybe Int]) ->
  (Empty, DataCount depth, BitVector n) ->
  ((Bool, [Maybe Int]), (Maybe (BitVector n), Bool))
takeClumsy (readLastCycle, Just n:stalls) (_, _, d) | n > 0 =
    ((False, Just (n-1):stalls), (nextData, False))
  where
    nextData = readLastCycle `orNothing` d
takeClumsy (readLastCycle, stalls) (_, _, d) =
    ((True, L.drop 1 stalls), (nextData, True))
  where
    nextData = readLastCycle `orNothing` d

type Feed d =
  SNat d ->
  [Either Int (BitVector 32)] ->
  (Full, DataCount d) ->
  ([Either Int (BitVector 32)], Maybe (BitVector 32))

-- | Driver for input to FIFO, takes stalls and data
feedState ::
  SNat d ->
  [Either Int (BitVector 32)] ->
  (Full, DataCount d) ->
  ([Either Int (BitVector 32)], Maybe (BitVector 32))
feedState _ [] _ = ([], Nothing)
feedState _ (Left 0:xs) (_, _) = (xs, Nothing)
feedState _ (Left i:xs) (_, _) = (Left (i-1):xs, Nothing)
feedState _ (Right x:xs) (full, _) =
  if full
    then (Right x:xs, Nothing)
    else (xs, Just x)

-- | Driver for input to FIFO, takes stalls and data and ignores full signals
-- out of the FIFO.
feedClumsy ::
  SNat d ->
  [Either Int (BitVector 32)] ->
  (Full, DataCount d) ->
  ([Either Int (BitVector 32)], Maybe (BitVector 32))
feedClumsy _ [] _ = ([], Nothing)
feedClumsy _ (Left 0:xs) (_, _) = (xs, Nothing)
feedClumsy _ (Left i:xs) (_, _) = (Left (i-1):xs, Nothing)
feedClumsy _ (Right x:xs) (_, _) =
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
    noWrRst = unsafeFromHighPolarity $ pure False
    wrEna = enableGen @write

    rdClk = clockGen @read
    noRdRst = unsafeFromHighPolarity $ pure False
    rdEna = enableGen @read

    wrData =
      mealyB wrClk noWrRst wrEna (feed d) wrDataList (wrFull, wrCnt)

    (rdDataMaybe, rdEnaB) =
      mealyB rdClk noRdRst rdEna drain (False, rdStalls) (rdEmpty, rdCnt, rdData)

    rdDataList =
      catMaybes
        $ sampleN (P.length wrDataList*10)
        $ bundle rdDataMaybe

    (FifoOut wrFull _ wrCnt rdEmpty _ rdCnt rdData) =
      dcFifo defConfig wrClk noWrRst rdClk noRdRst wrData rdEnaB
