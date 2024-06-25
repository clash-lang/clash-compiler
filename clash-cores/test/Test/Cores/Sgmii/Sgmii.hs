{-# LANGUAGE ViewPatterns #-}

module Test.Cores.Sgmii.Sgmii where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.AutoNeg
import Clash.Cores.Sgmii.BitSlip
import Clash.Cores.Sgmii.Common
import Clash.Cores.Sgmii.PcsReceive
import Clash.Cores.Sgmii.PcsTransmit
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

-- | The common part of both 'sgmii' and 'sgmiiLoopback', which implements
--   everything used for SGMII execept for 'pcsTransmit' which is different for
--   both implementations
sgmiiCommon ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom (C.BitVector 10) ->
  C.Signal dom (Maybe Bool, Maybe Bool, Maybe DataWord, Xmit, ConfReg)
sgmiiCommon cg1 = C.bundle (rxDv, rxEr, dw2, xmit, txConfReg)
 where
  (xmit, txConfReg) =
    C.unbundle $ autoNeg 0b0100000000000001 syncStatus rudi rxConfReg

  (rxDv, rxEr, dw2, rudi, rxConfReg) =
    C.unbundle $ pcsReceive cg3 rd dw1 rxEven syncStatus xmit

  (cg3, rd, dw1, rxEven, syncStatus) =
    C.unbundle $ sync cg2

  (cg2, _) = C.unbundle $ bitSlip cg1

-- | Placeholder integration function for all different parts of SGMII. This is
--   used to check whether the combination of the blocks in this module actually
--   work together, but it is by no means a finished end product.
sgmii ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom Bool ->
  C.Signal dom Bool ->
  C.Signal dom (C.BitVector 8) ->
  C.Signal dom (C.BitVector 10) ->
  C.Signal dom (Bool, Bool, C.BitVector 8, C.BitVector 10)
sgmii txEn txEr dw1 cg1 =
  C.bundle
    ( C.regMaybe False rxDv
    , C.regMaybe False rxEr
    , C.regMaybe 0 ((fmap . fmap) fromDw dw2)
    , cg2
    )
 where
  cg2 = pcsTransmit txEn txEr dw1 xmit txConfReg

  (rxDv, rxEr, dw2, xmit, txConfReg) = C.unbundle $ sgmiiCommon cg1

-- | Implementation of the generic 'sgmii' function that has its own output
--   (of the 'pcsReceive' block) connected to its own input (of the
--   'pcsTransmit' block). The enable signal for 'pcsTransmit' is asserted as
--   soon as there is valid coming from the 'pcsReceive' block.
sgmiiLoopback ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom (C.BitVector 10) ->
  C.Signal dom (C.BitVector 10)
sgmiiLoopback cg1 = cg2
 where
  cg2 = pcsTransmit txEn txEr dw2 xmit txConfReg
   where
    txEn = maybe False isDw <$> dw1
    txEr = pure False
    dw2 = maybe 0 fromDw <$> dw1

  (_, _, dw1, xmit, txConfReg) = C.unbundle $ sgmiiCommon cg1

-- | Loopback function that combines two full 'sgmii' systems, with one in
--   loopback configuration as defined in 'sgmiiLoopback', to check whether the
--   full system including auto negotiation works
loopbackSim ::
  (C.HiddenClockResetEnable dom) =>
  C.Signal dom (Bool, Bool, C.BitVector 8) ->
  C.Signal dom (C.BitVector 8)
loopbackSim (C.unbundle -> (txEn, txEr, dw1)) = dw2
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
  (_, _, _, _, confReg2) =
    C.unbundle $
      pcsReceive cg2 rd dw2 rxEven1 syncStatus (pure Conf)

  (cg2, rd, dw2, rxEven1, syncStatus) = C.unbundle $ sync cg1

  cg1 =
    C.unbundle $
      pcsTransmit (pure False) (pure False) (pure 0) (pure Conf) confReg1

-- | Test that the completely integrated system works in a loopback mode, where
--   the output of the second 'sgmii' instance is connected to its own input.
--   The system starts in configuration mode, so the first 76 samples are used
--   to set it up fully, after which it is ready for data transmission. During
--   data transmission, the first several packages are replaced with control
--   packages, these are dropped from the output comparision. Then, the output
--   is shifted to deal with the delay that is introduced along the way due to
--   registers.
prop_loopbackTest :: H.Property
prop_loopbackTest = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let setupSamples = 90
      delaySamples = 17
      controlCount = 9

      simOut =
        drop (setupSamples + delaySamples + controlCount) $
          C.sampleN
            (simDuration + setupSamples + delaySamples)
            ( loopbackSim @C.System
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

-- | Similar to 'prop_loopbackTest', however this time there is no loopback but
--   different signals are sent from left to right and right to left
prop_duplexTransmission :: H.Property
prop_duplexTransmission = H.property $ do
  simDuration <- H.forAll (Gen.integral (Range.linear 1 100))

  inp1 <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  inp2 <- H.forAll (Gen.list (Range.singleton simDuration) genDefinedBitVector)
  let setupSamples = 90
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
