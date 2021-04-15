{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cores.LatticeSemi.ICE40.IO where

import Clash.Prelude
import Clash.Cores.LatticeSemi.ICE40.IO

createDomain vSystem{vName="SystemFast", vPeriod=(vPeriod vSystem `div` 2)}

tests :: TestTree
tests = testGroup "SB_IO" []

-- | A sample from an instantiated SB_IO. This contains the values of all
-- output pins for a single clock cycle. PACKAGE_PIN is not sampled, as it
-- having the wrong value would be reflected in the d_in_0 and d_in_1 signals.
--
data Sample = Sample
  { sampledDIn0 :: Bit
  , sampledDIn1 :: Maybe Bit
  } deriving (Show, Generic, ShowX)

sampleSbio
  :: PinInputConfig
  -> PinOutputConfig
  -> [Bit]
  -- ^ Bit sent on d_out_0
  -> [Bit]
  -- ^ Bit sent on d_out_1
  -> [Bool]
  -- ^ Latch input value
  -> [Bool]
  -- ^ Enable output
  -> Int
  -- ^ Number of samples to take
  -> [Sample]
sampleSbio pinIn pinOut dOut0 dOut1 latch outputEnable samples
  | isDDRIn && isDDROut =
      goDDR pinIn pinOut dOut0 dOut1 outputEnable samples

  | not isDDRIn && not isDDROut =
      goNonDDR pinIn pinOut dOut0 latch outputEnable samples

  | otherwise =
      error "sampleSbio: unsupported configuration."
 where
  isDDROut =
    case pinOut of
      NoOutput -> True
      OutputDDR -> True
      OutputDDREnable -> True
      OutputDDREnableRegistered -> True
      _ -> False

  isDDRIn =
    case pinIn of
      PIN_INPUT -> isDDROut
      _ -> False

goNonDDR
  :: PinInputConfig
  -> PinOutputConfig
  -> [Bit]
  -> [Bool]
  -> [Bool]
  -> Int
  -> [Sample]
goNonDDR pinIn pinOut dOut0Vals latchVals outputEnableVals samples =
  (\x -> Sample x Nothing) <$> sampleN @System samples dIn0
 where
  clk = systemClockGen
  en = enableGen

  dOut0 = fromList dOut0Vals
  latch = fromList (fmap boolToBit latchVals)
  outputEnable = fromList outputEnableVals

  (pkgPin, dIn0) =
    withClock clk $ withEnable en
      (sbio pinIn pinOut (veryUnsafeToBiSignalIn pkgPin) latch dOut0 outputEnable)

goDDR
  :: PinInputConfig
  -> PinOutputConfig
  -> [Bit]
  -> [Bit]
  -> [Bool]
  -> Int
  -> [Sample]
goDDR pinIn pinOut dOut0Vals dOut1Vals outputEnableVals samples =
  (\(x, y) -> Sample x (Just y)) <$> sampleN @System samples (bundle (dIn0, dIn1))
 where
  clk = systemClockGen
  en = enableGen

  dOut0 = fromList dOut0Vals
  dOut1 = fromList dOut1Vals
  outputEnable = fromList outputEnableVals

  (pkgPin, dIn0, dIn1) =
    withSpecificClock @System clk $ withSpecificEnable @System en
      (sbioDDR @System @SystemFast pinIn pinOut (veryUnsafeToBiSignalIn pkgPin) dOut0 dOut1 outputEnable)

