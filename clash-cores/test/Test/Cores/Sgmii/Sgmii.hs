{-# LANGUAGE ViewPatterns #-}

module Test.Cores.Sgmii.Sgmii where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.AutoNeg
import Clash.Cores.Sgmii.BitSlip
import Clash.Cores.Sgmii.Common
import Clash.Cores.Sgmii.PcsReceive
import Clash.Cores.Sgmii.PcsTransmit
import Clash.Cores.Sgmii.PcsTransmit.CodeGroup
import Clash.Cores.Sgmii.PcsTransmit.OrderedSet
import Clash.Cores.Sgmii.Sync
import Clash.Hedgehog.Sized.BitVector
import qualified Clash.Prelude as C
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.TH
import Prelude

-- | Version of 'pcsTransmit' that allows the initial state to be set via an
--   input variable
pcsTransmitSim ::
  (C.HiddenClockResetEnable dom) =>
  CodeGroupState ->
  OrderedSetState ->
  C.Signal dom Bool ->
  C.Signal dom Bool ->
  C.Signal dom (C.BitVector 8) ->
  C.Signal dom Xmit ->
  C.Signal dom ConfReg ->
  C.Signal dom (C.BitVector 10)
pcsTransmitSim s1 s2 txEn txEr dw xmit txConfReg = cg
 where
  (_, cg, txEven, cgSent) =
    C.mealyB codeGroupT s1 (txOSet, dw, txConfReg)

  (_, txOSet) =
    C.mealyB orderedSetT s2 (txEn, txEr, dw, xmit, txEven, cgSent)

-- | Version of 'sync' that allows the initial state to be set via an input
--   variable
syncSim ::
  (C.HiddenClockResetEnable dom) =>
  SyncState ->
  C.Signal dom (C.BitVector 10) ->
  C.Signal dom (C.BitVector 10, Bool, C.Vec 3 DataWord, Even, SyncStatus)
syncSim s cg1 = o
 where
  o =
    C.moore outputQueueT outputQueueO (C.repeat (0, False, Dw 0, Odd, Ok)) $
      C.bundle (cg2, rd, dw, rxEven, syncStatus)

  (_, cg2, rd, dw, rxEven, syncStatus) =
    C.mooreB syncT syncO s cg1

-- | Version of 'pcsReceive' that allows the initial state to be set via an
--   input variable
pcsReceiveSim ::
  (C.HiddenClockResetEnable dom) =>
  PcsReceiveState ->
  C.Signal dom (C.BitVector 10) ->
  C.Signal dom Bool ->
  C.Signal dom (C.Vec 3 DataWord) ->
  C.Signal dom Even ->
  C.Signal dom SyncStatus ->
  C.Signal dom Xmit ->
  C.Signal
    dom
    ( Maybe Bool
    , Maybe Bool
    , Maybe DataWord
    , Maybe Rudi
    , Maybe ConfReg
    )
pcsReceiveSim s cg rd dw1 rxEven syncStatus xmit =
  C.bundle (rxDv, rxEr, dw2, rudi, rxConfReg)
 where
  (_, rxDv, rxEr, dw2, rudi, rxConfReg) =
    C.mealyB pcsReceiveT s (cg, rd, dw1, rxEven, syncStatus, xmit)

-- | Placeholder integration function for all different parts of Sgmii. This is
--   used to check whether the combination of the blocks in this module actually
--   work together, but it is by no means a finished end product.
sgmii ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom Bool ->
  C.Signal dom Bool ->
  C.Signal dom (C.BitVector 8) ->
  C.Signal dom (C.BitVector 10) ->
  C.Signal dom (Bool, Bool, C.BitVector 8, C.BitVector 10)
sgmii txEn txEr dw cg1 =
  C.bundle
    ( C.regMaybe False rxDv
    , C.regMaybe False rxEr
    , C.regMaybe 0 ((fmap . fmap) fromDw dw2)
    , pcsTransmit txEn txEr dw xmit txConfReg
    )
 where
  (cg2, rd, dw1, rxEven, syncStatus) =
    C.unbundle $ sync $ fst <$> bitSlip cg1

  (rxDv, rxEr, dw2, rudi, rxConfReg) =
    C.unbundle $ pcsReceive cg2 rd dw1 rxEven syncStatus xmit

  (xmit, txConfReg) =
    C.unbundle $ autoNeg 0b0100000000000001 syncStatus rudi rxConfReg

-- | Implementation of the generic 'sgmii' function that has its own output
--   (of the 'pcsReceive' block) connected to its own input (of the
--   'pcsTransmit' block). The enable signal for 'pcsTransmit' is asserted as
--   soon as there is valid coming from the 'pcsReceive' block.
sgmiiLoopback ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom (C.BitVector 10) ->
  C.Signal dom (C.BitVector 10)
sgmiiLoopback cg1 = cg3
 where
  cg3 = pcsTransmit txEn txEr dw3 xmit txConfReg

  (cg2, rd, dw1, rxEven, syncStatus) =
    C.unbundle $ sync cg1

  (_, _, dw2, rudi, rxConfReg) =
    C.unbundle $ pcsReceive cg2 rd dw1 rxEven syncStatus xmit

  (xmit, txConfReg) =
    C.unbundle $ autoNeg 0b0100000000000001 syncStatus rudi rxConfReg

  txEn = maybe False isDw <$> dw2
  txEr = pure False
  dw3 = maybe 0 fromDw <$> dw2

-- | Function that takes a 'BitVector' that is ran through 'pcsTransmit' and
--   returned via 'pcsReceive', with the values for 'Xmit' set by hand (so
--   without auto negotiation)
loopbackSim1 ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom (C.BitVector 8) ->
  C.Signal dom (C.BitVector 8)
loopbackSim1 dw1 = fromDw . fromMaybe (Dw 0) <$> dw3
 where
  cg1 =
    C.unbundle $
      pcsTransmitSim
        (DataGo False Odd)
        (TxData Data)
        (pure True)
        (pure False)
        dw1
        (pure Data)
        (pure 1)

  (cg2, rd, dw2, rxEven1, syncStatus) =
    C.unbundle $ syncSim (SyncAcquired1 0 False (Dw 0) Even) cg1

  (_, _, dw3, _, _) =
    C.unbundle $
      pcsReceiveSim
        (RxData False (Dw 0))
        cg2
        rd
        dw2
        rxEven1
        syncStatus
        (pure Data)

-- | Loopback function that combines two full 'sgmii' systems, with one in
--   loopback configuration as defined in 'sgmiiLoopback', to check whether the
--   full system including auto negotiation works
loopbackSim2 ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom (Bool, Bool, C.BitVector 8) ->
  C.Signal dom (C.BitVector 8)
loopbackSim2 (C.unbundle -> (txEn, txEr, dw1)) = dw2
 where
  (_, _, dw2, cg2) =
    C.unbundle $ sgmii txEn txEr dw1 cg1

  cg1 = C.unbundle $ sgmiiLoopback cg2

-- | Function that runs two versions of 'sgmii' at the same time
duplexTransmissionSim ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom (Bool, Bool, C.BitVector 8, C.BitVector 8) ->
  C.Signal dom (C.BitVector 8, C.BitVector 8)
duplexTransmissionSim (C.unbundle -> (txEn, txEr, dw1, dw2)) =
  C.bundle (dw3, dw4)
 where
  (_, _, dw3, cg1) = C.unbundle $ sgmii txEn txEr dw1 cg2
  (_, _, dw4, cg2) = C.unbundle $ sgmii txEn txEr dw2 cg1

-- | Function that is used to propagate the config register via 'pcsTransmit'
--   'pcsReceive'
confRegSim ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom ConfReg ->
  C.Signal dom ConfReg
confRegSim confReg1 = fromMaybe 0 <$> confReg2
 where
  cg1 =
    C.unbundle $
      pcsTransmit (pure False) (pure False) (pure 0) (pure Conf) confReg1

  (cg2, rd, dw2, rxEven1, syncStatus) = C.unbundle $ sync cg1

  (_, _, _, _, confReg2) =
    C.unbundle $
      pcsReceive cg2 rd dw2 rxEven1 syncStatus (pure Data)

-- | Test that, with the correct initial states, the input value for the
--   transmit block is the same as the output of the receive block
prop_loopbackTest1 :: H.Property
prop_loopbackTest1 = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let setupSamples = 1
      delaySamples = 5

      simOut =
        drop (setupSamples + delaySamples) $
          C.sampleN
            (simDuration + setupSamples + delaySamples)
            ( loopbackSim1 @C.System
                ( C.fromList
                    ( replicate setupSamples 0
                        ++ inp
                        ++ replicate delaySamples 0
                    )
                )
            )

      expected = inp

  simOut H.=== expected

-- | Test that the completely integrated system works in a loopback mode, where
--   the output of the second 'sgmii' instance is connected to its own input.
--   The system starts in configuration mode, so the first 76 samples are used
--   to set it up fully, after which it is ready for data transmission. During
--   data transmission, the first several packages are replaced with control
--   packages, these are dropped from the output comparision. Then, the output
--   is shifted to deal with the delay that is introduced along the way due to
--   registers.
prop_loopbackTest2 :: H.Property
prop_loopbackTest2 = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 20 100))

  inp <-
    H.forAll
      (Gen.list (Range.singleton simDuration) (Gen.filter (/= 0) genDefinedBitVector))
  let setupSamples = 86
      delaySamples = 15
      controlCount = 9

      simOut =
        drop (setupSamples + delaySamples + controlCount) $
          C.sampleN
            (simDuration + setupSamples + delaySamples)
            ( loopbackSim2 @C.System
                ( C.fromList
                    ( replicate setupSamples (False, False, 0)
                        ++ map f inp
                        ++ replicate delaySamples (False, False, 0)
                    )
                )
            )
       where
        f dw = (True, False, dw)

      expected = drop controlCount inp

  simOut H.=== expected

-- | Similar to 'prop_loopbackTest2', however this time there is no loopback but
--   different signals are sent from left to right and right to left
prop_duplexTransmission :: H.Property
prop_duplexTransmission = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp1 <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  inp2 <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let setupSamples = 86
      delaySamples = 9
      controlCount = 9

      inp = zip inp1 inp2

      simOut =
        drop (setupSamples + delaySamples + controlCount) $
          C.sampleN
            (simDuration + setupSamples + delaySamples)
            ( duplexTransmissionSim @C.System
                ( C.fromList
                    ( replicate setupSamples (False, False, 0, 0)
                        ++ map f inp
                        ++ replicate delaySamples (False, False, 0, 0)
                    )
                )
            )
       where
        f (dw1, dw2) = (True, False, dw1, dw2)

      expected = map swap $ drop controlCount inp

  simOut H.=== expected

-- | Assert that the configuration register will be propagated from the transmit
--   block to the receive block in exactly 22 cycles
prop_confRegPropagated :: H.Property
prop_confRegPropagated = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.singleton 23))

  inp <- H.forAll genDefinedBitVector
  let simOut =
        drop 1 $
          C.sampleN
            (simDuration + 1)
            (confRegSim @C.System (pure inp))

  H.assert $ inp `elem` simOut

tests :: TestTree
tests = $(testGroupGenerator)
