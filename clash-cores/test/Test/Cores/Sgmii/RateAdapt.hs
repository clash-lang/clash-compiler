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

-- | Convert a speed to a symbol duplication factor
duplicationFactor :: LinkSpeed -> Int
duplicationFactor linkSpeed = case linkSpeed of
  Speed10 -> 100
  Speed100 -> 10
  Speed1000 -> 1

-- | Function to take the n'th elements of a list
everyNth :: (Num a) => Int -> [a] -> [a]
everyNth n (drop (n - 1) -> l)
  | null l = []
  | otherwise = head' 0 l : everyNth n (drop 1 l)

-- | Function to safely take the first element of a list and replace it with a
--   default value if the list is empty
head' :: a -> [a] -> a
head' a l = fst $ fromMaybe (a, []) $ uncons l

-- | Test whether the receive rate adaptation works as intended
rateAdaptRxTest :: LinkSpeed -> H.Property
rateAdaptRxTest linkSpeed = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 1000))

  inp <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let simOut =
        drop 1 $
          C.sampleN
            (simDuration + 1)
            ( rateAdaptRxSim @C.System
                (C.fromList ((linkSpeed, Nothing) : map f inp))
            )
       where
        f a = (linkSpeed, Just a)

      expected =
        head' 0 inp : everyNth (duplicationFactor linkSpeed) (drop 1 inp)

  catMaybes simOut H.=== expected

prop_rateAdaptRx10 :: H.Property
prop_rateAdaptRx10 = rateAdaptRxTest Speed10

prop_rateAdaptRx100 :: H.Property
prop_rateAdaptRx100 = rateAdaptRxTest Speed100

prop_rateAdaptRx1000 :: H.Property
prop_rateAdaptRx1000 = rateAdaptRxTest Speed1000

-- | Test whether the transmit rate adaptation works as intended
rateAdaptTxTest :: LinkSpeed -> H.Property
rateAdaptTxTest linkSpeed = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp1 <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let inp2 = concatMap (replicate (duplicationFactor linkSpeed)) inp1

      simOut =
        map g $
          drop 1 $
            C.sampleN
              (length inp2 + 1)
              ( rateAdaptTxSim @C.System
                  (C.fromList ((linkSpeed, Nothing) : map f inp2))
              )
       where
        f a = (linkSpeed, Just a)
        g (_, a) = fromJust a

      expected = take (length simOut) inp2

  simOut H.=== expected

prop_rateAdaptTx10 :: H.Property
prop_rateAdaptTx10 = rateAdaptTxTest Speed10

prop_rateAdaptTx100 :: H.Property
prop_rateAdaptTx100 = rateAdaptTxTest Speed100

prop_rateAdaptTx1000 :: H.Property
prop_rateAdaptTx1000 = rateAdaptTxTest Speed1000

tests :: TestTree
tests = $(testGroupGenerator)
