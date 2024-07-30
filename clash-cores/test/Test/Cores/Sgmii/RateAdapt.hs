{-# LANGUAGE ViewPatterns #-}

module Test.Cores.Sgmii.RateAdapt where

import Clash.Cores.Sgmii.Common
import Clash.Cores.Sgmii.RateAdapt
import Clash.Hedgehog.Sized.BitVector
import qualified Clash.Prelude as C
import Data.List (uncons)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

-- | Version of 'rateAdaptRx' that takes an input tuple instead of separate
--   variables
rateAdaptRxSim ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom (LinkSpeed, Maybe (C.BitVector 8)) ->
  C.Signal dom (Maybe (C.BitVector 8))
rateAdaptRxSim (C.unbundle -> (linkSpeed, rxDw)) = rateAdaptRx linkSpeed rxDw

-- | Version of 'rateAdaptTx' that takes an input tuple instead of separate
--   variables
rateAdaptTxSim ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom (LinkSpeed, Maybe (C.BitVector 8)) ->
  C.Signal dom (Bool, Maybe (C.BitVector 8))
rateAdaptTxSim (C.unbundle -> (linkSpeed, txDw)) =
  C.bundle $ rateAdaptTx linkSpeed txDw

-- | Function to take the n'th elements of a list
everyNth :: (Num a) => Int -> [a] -> [a]
everyNth n (drop (n - 1) -> l)
  | null l = []
  | otherwise = head' 0 l : everyNth n (drop 1 l)

-- | Function to safely take the first element of a list and replace it with a
--   default value if the list is empty
head' :: a -> [a] -> a
head' a l = fst $ fromMaybe (a, []) $ uncons l

-- | Function that tests the rate adaptation function with a link speed of 1000
--   Mbps, which means that every input value should be propagated to the output
prop_rateAdaptRx1000 :: H.Property
prop_rateAdaptRx1000 = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 1000))

  inp <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let simOut =
        drop 1 $
          C.sampleN
            (simDuration + 1)
            ( rateAdaptRxSim @C.System
                (C.fromList ((Speed1000, Nothing) : map f inp))
            )
       where
        f a = (Speed1000, Just a)

      expected = inp

  catMaybes simOut H.=== expected

-- | Function that tests the rate adaptation function with a link speed of 100
--   Mbps, which means that every 10th input value (starting at 0) should be
--   propagated to the output
prop_rateAdaptRx100 :: H.Property
prop_rateAdaptRx100 = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 1000))

  inp <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let simOut =
        drop 1 $
          C.sampleN
            (simDuration + 1)
            ( rateAdaptRxSim @C.System
                (C.fromList ((Speed100, Nothing) : map f inp))
            )
       where
        f a = (Speed100, Just a)

      expected = head' 0 inp : everyNth 10 (drop 1 inp)

  catMaybes simOut H.=== expected

-- | Function that tests the rate adaptation function with a link speed of 10
--   Mbps, which means that every 100th input value (starting at 0) should be
--   propagated to the output
prop_rateAdaptRx10 :: H.Property
prop_rateAdaptRx10 = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 1000))

  inp <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let simOut =
        drop 1 $
          C.sampleN
            (simDuration + 1)
            ( rateAdaptRxSim @C.System
                (C.fromList ((Speed10, Nothing) : map f inp))
            )
       where
        f a = (Speed10, Just a)

      expected = head' 0 inp : everyNth 100 (drop 1 inp)

  catMaybes simOut H.=== expected

-- | Function that tests the rate adaptation function with a link speed of 1000
--   Mbps, which means that every input value should be propagated to the output
prop_rateAdaptTx1000 :: H.Property
prop_rateAdaptTx1000 = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let simOut =
        map g $
          drop 1 $
            C.sampleN
              (simDuration + 1)
              ( rateAdaptTxSim @C.System
                  (C.fromList ((Speed1000, Nothing) : map f inp))
              )
       where
        f a = (Speed1000, Just a)
        g (_, a) = fromJust a

      expected = inp

  simOut H.=== expected

-- | Function that tests the rate adaptation function with a link speed of 100
--   Mbps, which means that every 10th input value (starting at 0) should be
--   propagated to the output
prop_rateAdaptTx100 :: H.Property
prop_rateAdaptTx100 = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp1 <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let inp2 = concatMap (replicate 10) inp1

      simOut =
        map g $
          drop 1 $
            C.sampleN
              (length inp2 + 1)
              ( rateAdaptTxSim @C.System
                  (C.fromList ((Speed100, Nothing) : map f inp2))
              )
       where
        f a = (Speed100, Just a)
        g (_, a) = fromJust a

      expected = take (length simOut) inp2

  simOut H.=== expected

-- | Function that tests the rate adaptation function with a link speed of 10
--   Mbps, which means that every 100th input value (starting at 0) should be
--   propagated to the output
prop_rateAdaptTx10 :: H.Property
prop_rateAdaptTx10 = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp1 <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let inp2 = concatMap (replicate 100) inp1

      simOut =
        map g $
          drop 1 $
            C.sampleN
              (length inp2 + 1)
              ( rateAdaptTxSim @C.System
                  (C.fromList ((Speed10, Nothing) : map f inp2))
              )
       where
        f a = (Speed10, Just a)
        g (_, a) = fromJust a

      expected = take (length simOut) inp2

  simOut H.=== expected

tests :: TestTree
tests = $(testGroupGenerator)
