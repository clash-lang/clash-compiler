{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Cores.LatticeSemi.ICE40.IO
  ( tests
  ) where

import Prelude

import Data.Bits (complement)
import Data.List (mapAccumL)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Gen, Property, (.&&.))
import qualified Test.Tasty.QuickCheck as QC

import Clash.Class.BitPack (isLike, boolToBit)
import Clash.Signal hiding ((.&&.))
import Clash.Sized.BitVector (Bit, low, high)
import Clash.XException (errorX, showX)

import Clash.Cores.LatticeSemi.ICE40.IO

import Debug.Trace -- TODO

-- | A variant of System which runs at double speed. Used when testing DDR.
createDomain vSystem{vName="SystemFast", vPeriod=(vPeriod vSystem `div` 2)}

tests :: TestTree
tests = testGroup "SB_IO"
  [ QC.testProperty "Simple Echo" simpleEcho
  , QC.testProperty "Latched Echo" latchedEcho
  , QC.testProperty "Enabled Echo" enabledEcho
  ]

-- | Test that dIn0 echoes dOut0 in simulation, with latchInput always being
-- set to False and outputEnable always set to True.
--
simpleEcho :: Property
simpleEcho =
  QC.property $ do
    (pinIn, pinOut) <- genConfig
    (len, dOut0, dOut1) <- genDOut

    let isDDR = isDDROut pinOut
    let latency = uncurry (+) (configLatency pinIn pinOut)
    let numCycles = latency + len

    -- Fixed LATCH_INPUT_VALUE and OUTPUT_ENABLE
    let latch = replicate numCycles False
    let outEn = replicate numCycles True

    -- When OUTPUT_ENABLE is behind a register, the first value is False. This
    -- means the output in the first cycle becomes high impedence / undefined.
    let dOut0' = if isEnableRegistered pinOut
                   then errorX "X" : tail dOut0
                   else dOut0

    -- The values are delayed by the total latency (i.e. the total numbe of
    -- registers along the data path).
    let dIn0 = replicate latency (errorX "X") ++ dOut0'
    let dIn1 = replicate latency (errorX "X") ++ dOut1

    let samples = sampleSbio pinIn pinOut dOut0 dOut1 latch outEn numCycles

    pure
      . QC.collect (pinIn, pinOut)
      . trace (showX (show pinIn, show pinOut, dOut0, dOut1, latch, outEn, dIn0, dIn1))
      $ checkDIn0 pinOut samples dIn0 .&&. checkDIn1 isDDR samples dIn1

-- | Test that configurations which use iCEgate low power latch correctly hold
-- the previous value when the latch is asserted.
--
latchedEcho :: Property
latchedEcho =
  QC.property $ do
    (pinIn, pinOut) <- genLatchedConfig
    (len, dOut0, dOut1) <- genDOut

    let (latencyIn, latencyOut) = configLatency pinIn pinOut
    let numCycles = latencyIn + latencyOut + len

    -- Variable LATCH_INPUT_VALUE, fixed OUTPUT_ENABLE
    latch <- QC.vector numCycles
    let outEn = replicate numCycles True

    -- When OUTPUT_ENABLE is behind a register, the first value is False. This
    -- means the output in the first cycle becomes high impedence / undefined.
    let dOut0' = if isEnableRegistered pinOut
                   then errorX "X" : tail dOut0
                   else dOut0

    -- The value of the PACKAGE_PIN is delayed by the output latency (i.e. the
    -- number of registers in the data path before the pin).
    let pkgPin = replicate latencyOut (errorX "X") ++ dOut0'

    -- The value may be latched in some cycles, which means the previous value
    -- is held instead of taking the next value from PACKAGE_PIN.
    let latched = snd $ mapAccumL
                    (\x0 (x1, l) -> if l then (x0, x0) else (x1, x1))
                    (errorX "latched: No previous value")
                    (zip pkgPin latch)

    -- The latched values are delayed by the input latency (i.e. the number of
    -- registers in the data path after the pin).
    let dIn0 = replicate latencyIn (errorX "X") ++ latched

    -- Sampling the simulation of the hardware should produce the same result
    -- as this list-based description of what the hardware does.
    let samples = sampleSbio pinIn pinOut dOut0 dOut1 latch outEn numCycles

    pure . QC.collect (pinIn, pinOut) $
      checkDIn0 pinOut samples dIn0

enabledEcho :: Property
enabledEcho =
  QC.property $ do
    (pinIn, pinOut) <- genEnableConfig
    (len, dOut0, dOut1) <- genDOut

    let isDDR = isDDROut pinOut
    let (latencyIn, latencyOut) = configLatency pinIn pinOut
    let numCycles = latencyIn + latencyOut + len

    -- Fixed LATCH_INPUT_VALUE, variable OUTPUT_ENABLE
    let latch = replicate numCycles False
    outEn <- QC.vector numCycles

    -- Delay the outputs by the number of registers before PACKAGE_PIN.
    let dOut0' = replicate latencyOut (errorX "X") ++ dOut0
    let dOut1' = replicate latencyOut (errorX "X") ++ dOut1

    -- When OUTPUT_ENABLE is behind a register, the first cycle is always False.
    let outEn' = if isEnableRegistered pinOut then False : outEn else outEn

    let nextBit x e = if e then x else errorX "X"

    -- The input is the value of the output if OUTPUT_ENABLE is high, otherwise
    -- it is high impedence (undefined).
    let dIn0 = zipWith nextBit dOut0' outEn'
    let dIn1 = zipWith nextBit dOut1' outEn'

    -- Delay the inputs by the number of registers after PACKAGE_PIN
    let dIn0' = replicate latencyIn (errorX "X") ++ dIn0
    let dIn1' = replicate latencyIn (errorX "X") ++ dIn1

    let samples = sampleSbio pinIn pinOut dOut0 dOut1 latch outEn numCycles

    pure
      . QC.collect (pinIn, pinOut)
      . trace (showX (show pinIn, show pinOut, dOut0, dOut1, latch, outEn, dIn0, dIn1))
      $ checkDIn0 pinOut samples dIn0' .&&. checkDIn1 isDDR samples dIn1'

isEnableRegistered :: PinOutputConfig -> Bool
isEnableRegistered = \case
  OutputEnableRegistered -> True
  OutputRegisteredEnableRegistered -> True
  OutputDDREnableRegistered -> True
  OutputRegisteredEnableRegisteredInverted -> True
  _ -> False

checkDIn0 :: PinOutputConfig -> [Sample] -> [Bit] -> Property
checkDIn0 pinOut samples expected =
  let actual = dIn0Samples samples
   in QC.counterexample
        ("dIn0: " <> showX actual <> " `isLike` " <> showX expected)
        (all (uncurry isLike) (zip actual expected))
 where
  dIn0Samples
    | isInverted pinOut = fmap (complement . sampledDIn0)
    | otherwise = fmap sampledDIn0

  isInverted = \case
    OutputRegisteredInverted -> True
    OutputRegisteredEnableInverted -> True
    OutputRegisteredEnableRegisteredInverted -> True
    _ -> False

checkDIn1 :: Bool -> [Sample] -> [Bit] -> Property
checkDIn1 isDDR samples dOut1
  | isDDR =
      let expected = fmap Just dOut1
          actual = fmap sampledDIn1 samples
       in QC.counterexample
            ("dIn1: " <> showX actual <> " `isLike` " <> showX expected)
            (all (uncurry isLike) (zip actual expected))

  | otherwise = QC.property True

-- | Determine the number of cycles latency it takes for an input/output value
-- to make it from/to the PACKAGE_PIN.
--
configLatency :: PinInputConfig -> PinOutputConfig -> (Int, Int)
configLatency pinIn pinOut =
  (inputLatency, outputLatency)
 where
  inputLatency =
    case pinIn of
      Input
        | isDDROut pinOut -> 1
        | otherwise -> 0

      InputRegistered -> 1
      InputLatch -> 0
      InputRegisteredLatch -> 1

  outputLatency =
    case pinOut of
      NoOutput -> 0
      Output -> 0
      OutputTristate -> 0
      OutputEnableRegistered -> 0
      _ -> 1

-- Generate a pin configuration supported by the implementation.
--
-- TODO: If SB_IO is generalised to allow DDR input without DDR output (or
-- vice-versa) this should be changed to something like
--
--   liftA2 (,) QC.arbitrary QC.arbitrary
--
genConfig :: Gen (PinInputConfig, PinOutputConfig)
genConfig = do
  pinOut <- QC.arbitrary
  pinIn <- if isDDROut pinOut then pure Input else QC.arbitrary

  pure (pinIn, pinOut)

-- | Generate a pin configuration where an iCEGate low power latch is used.
--
genLatchedConfig :: Gen (PinInputConfig, PinOutputConfig)
genLatchedConfig = do
  pinOut <- QC.arbitrary `QC.suchThat` (not . isDDROut)
  pinIn <- QC.elements [InputRegisteredLatch, InputLatch]

  pure (pinIn, pinOut)

-- | Generate a pin configuration where the output is tristated.
--
genEnableConfig :: Gen (PinInputConfig, PinOutputConfig)
genEnableConfig = do
  pinOut <- QC.arbitrary `QC.suchThat` isTristate
  pinIn <- if isDDROut pinOut then pure Input else QC.arbitrary

  pure (pinIn, pinOut)
 where
  isTristate = \case
    NoOutput -> False
    Output -> False
    OutputRegistered -> False
    OutputDDR -> False
    OutputRegisteredInverted -> False
    _ -> True

-- | Generate some arbitrary (but equal length) inputs for the D_OUT_0 and
-- D_OUT_1 inputs to the primitive.
genDOut :: Gen (Int, [Bit], [Bit])
genDOut = do
  len <- fmap QC.getPositive QC.arbitrary
  dOut0 <- QC.vectorOf len (QC.elements [low, high])
  dOut1 <- QC.vectorOf len (QC.elements [low, high])

  pure (len, dOut0, dOut1)

isDDROut :: PinOutputConfig -> Bool
isDDROut = \case
  OutputDDR -> True
  OutputDDREnable -> True
  OutputDDREnableRegistered -> True
  _ -> False

-- | A sample from an instantiated SB_IO. This contains the values of all
-- output pins for a single clock cycle. PACKAGE_PIN is not sampled, as it
-- having the wrong value would be reflected in the d_in_0 and d_in_1 signals.
--
data Sample = Sample
  { sampledDIn0 :: Bit
  , sampledDIn1 :: Maybe Bit
  }

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
  | validNonDDR =
      goNonDDR pinIn pinOut dOut0 latch outputEnable samples

  | validDDR =
      goDDR pinIn pinOut dOut0 dOut1 outputEnable samples

  | otherwise =
      error "sampleSbio: unsupported configuration."
 where
  validDDR =
    case pinIn of
      Input -> isDDROut pinOut || NoOutput == pinOut
      _ -> False

  validNonDDR =
    not (isDDROut pinOut)

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
    withClock @System clk $ withEnable @System en
      (sbioDDR @System @SystemFast pinIn pinOut (veryUnsafeToBiSignalIn pkgPin) dOut0 dOut1 outputEnable)
