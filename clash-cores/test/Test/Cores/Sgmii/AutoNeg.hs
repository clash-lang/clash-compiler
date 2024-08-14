{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Test.Cores.Sgmii.AutoNeg where

import Clash.Cores.Sgmii.AutoNeg
import Clash.Cores.Sgmii.Common
import Clash.Hedgehog.Sized.BitVector
import qualified Clash.Prelude as C
import Data.List (find)
import Data.Maybe (isJust, isNothing)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

-- | Generate a BitVector with its two most significant bits (15 and 14) set to
--   zero. Future improvement: also make bit 15 a random value.
genConfRegNoAck :: H.Gen ConfReg
genConfRegNoAck = (C.++#) (0b00 :: C.BitVector 2) <$> genDefinedBitVector

-- | Generate a BitVector with its most significant bit (15) set to zero and the
--   acknowledge bit set to one. Future improvement: also make bit 15 a random
--   value.
genConfRegAck :: H.Gen ConfReg
genConfRegAck = (C.++#) (0b01 :: C.BitVector 2) <$> genDefinedBitVector

-- | Generate a list of 'ConfReg's without bit 14 asserted
genConfRegsNoAck :: H.Range Int -> H.Gen [ConfReg]
genConfRegsNoAck range = do
  confReg <- Gen.filter (/= 0) genConfRegNoAck
  n <- Gen.int range
  pure $ take n $ concat $ replicate n (replicate 3 confReg)

-- | Generate a list of 'ConfReg's with bit 14 asserted where every value is
--   repeated 3 times in a row
genConfRegsAck :: H.Range Int -> H.Gen [ConfReg]
genConfRegsAck range = do
  confReg <- Gen.filter (/= 0) genConfRegAck
  n <- Gen.int range
  pure $ take n $ concat $ replicate n (replicate 3 confReg)

-- | Version of 'autoNeg' that does not return any actual values, but only the
--   entered state for debugging purposes.
autoNegSim ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom (Status, Maybe Rudi) ->
  C.Signal dom (AutoNegState dom)
autoNegSim (C.unbundle -> i) = s
 where
  (s, _, _) = C.mooreB autoNegT autoNegO (AnEnable Nothing 0 0) i

-- | Generate a list of values that do not contain the acknowledge bit, and
--   assert that the @ACKNOWLEDGE_DETECT@ state is entered but not the
--   @COMPLETE_ACKNOWLEDGE@ state
prop_autoNegNoAckComplete :: H.Property
prop_autoNegNoAckComplete = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 9 100))

  inp <- H.forAll (genConfRegsNoAck (Range.singleton simDuration))
  let simOut =
        C.sampleN simDuration (autoNegSim @C.System (C.fromList (map f inp)))
       where
        f a = (Ok, Just (RudiC a))

  H.assert $ isNothing (find g simOut)
  H.assert $ isJust (find h simOut)
 where
  g (CompleteAck{}) = True
  g _ = False

  h (AckDetect{}) = True
  h _ = False

-- | Generate a list of values that do contain the acknowledge bit, and assert
--   that the @COMPLETE_ACKNOWLEDGE@ state is entered
prop_autoNegAckComplete :: H.Property
prop_autoNegAckComplete = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 12 100))

  inp <- H.forAll (genConfRegsAck (Range.singleton simDuration))
  let simOut =
        C.sampleN simDuration (autoNegSim @C.System (C.fromList (map f inp)))
       where
        f a = (Ok, Just (RudiC a))

  H.assert $ isJust (find g simOut)
 where
  g (CompleteAck{}) = True
  g _ = False

-- | Assert that in a simulation, the number of times a given state that uses
--   the link timer as a transition predicate is entered is exactly equal to 3
prop_autoNegLinkTimer :: H.Property
prop_autoNegLinkTimer = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 10 100))

  inp <- H.forAll (genConfRegsNoAck (Range.singleton simDuration))
  let simOut =
        C.sampleN simDuration (autoNegSim @C.System (C.fromList (map f inp)))
       where
        f a = (Ok, Just (RudiC a))

  (length . filter g) simOut H.=== 3
 where
  g (AnRestart{}) = True
  g _ = False

-- | Assert that if 'Status' is set to 'Fail', 'autoNeg' will never leave the
--   'AnEnable' state (except at initialization, hence the first 10 outputs are
--   dropped from the comparision)
prop_autoNegFail :: H.Property
prop_autoNegFail = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 10 100))

  inp <- H.forAll (genConfRegsAck (Range.singleton simDuration))
  let simOut =
        C.sampleN simDuration (autoNegSim @C.System (C.fromList (map f inp)))
       where
        f a = (Fail, Just (RudiC a))

  (length . filter g) (drop 10 simOut) H.=== simDuration - 10
 where
  g (AnEnable{}) = True
  g _ = False

-- | Assert that if values with ack set and ack not set are inputted
--   interchangeably the system will never trigger 'acknowledgeMatch' and thus
--   not reach 'CompleteAck'.
prop_autoNegNoThreeInARow :: H.Property
prop_autoNegNoThreeInARow = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp1 <- H.forAll (genConfRegsAck (Range.singleton simDuration))
  inp2 <- H.forAll (genConfRegsNoAck (Range.singleton simDuration))
  let inp = take simDuration $ concat $ zipWith (\a b -> [a, b]) inp1 inp2
      simOut =
        C.sampleN simDuration (autoNegSim @C.System (C.fromList (map f inp)))
       where
        f a = (Ok, Just (RudiC a))

  H.assert $ isNothing (find g simOut)
 where
  g (CompleteAck{}) = True
  g _ = False

C.createDomain C.vSystem{C.vName = "TimeoutDom", C.vPeriod = C.hzToPeriod 125e6}

-- | Assert that for a domain frequency of 125 MHz the maximul value for timeout
--   is equal to the maxBound of an 'Index'.
prop_autoNegTimeoutLength :: H.Property
prop_autoNegTimeoutLength = H.withTests 1 $ H.property $ do
  maxBound @(Clash.Cores.Sgmii.AutoNeg.Timeout TimeoutDom)
    H.=== maxBound @(C.Index 200000)

tests :: TestTree
tests = $(testGroupGenerator)
