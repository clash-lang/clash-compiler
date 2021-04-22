{-|
  Copyright   :  (C) 2019, Foamspace corp
                 (C) 2021, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  LATTICE ICE IO primitives. Implementations are documented in the
  <http://www.latticesemi.com/~/media/LatticeSemi/Documents/TechnicalBriefs/SBTICETechnologyLibrary201504.pdf LATTICE ICE Technology Library>,
  referred to as LITL.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.LatticeSemi.ICE40.IO
  ( sbio
  , sbioDDR
  , PinOutputConfig(..)
  , PinInputConfig(..)
  ) where

import           Clash.Annotations.Primitive  (Primitive(..), HDL(..), hasBlackBox)
import qualified Clash.Explicit.Signal as E
import           Clash.Prelude
import           Clash.Signal.Internal        (Signal(..))

import           Data.String.Interpolate      (i)
import           Data.String.Interpolate.Util (unindent)
import           GHC.Stack                    (HasCallStack)
import           Test.QuickCheck              (Arbitrary(..))
import qualified Test.QuickCheck as QC

-- | Input pinType  mnemonics as documented in the first table of LITL p88. Note
-- that @PIN_INPUT_DDR@ is missing. Use 'PIN_INPUTRegistered' instead.
data PinInputConfig
  = InputRegistered
  -- ^ Input data is registered in input cell. Same as @PIN_INPUT_DDR@.
  | Input
  -- ^ Simple input pin (D_IN_0)
  | InputRegisteredLatch
  -- ^ (Not supported by Clash simulation.) Disables internal data changes on
  -- the physical input pin by latching the value on the input register.
  | InputLatch
  -- ^ (Not supported by Clash simulation.) Disables internal data changes on
  -- the physical input pin by latching the value.
  deriving (Eq, Show, Generic, BitPack)

instance Arbitrary PinInputConfig where
  arbitrary = QC.elements
    [ InputRegistered
    , Input
    , InputRegisteredLatch
    , InputLatch
    ]

-- | Output pinType  mnemonics as documented in the second table of LITL p88.
data PinOutputConfig
  = NoOutput
  -- ^ Disables the output function
  | Output
  -- ^ Simple output pin, (no enable)
  | OutputTristate
  -- ^ The output pin may be tristated using the enable
  | OutputEnableRegistered
  -- ^ The output pin may be tristated using a registered enable signal
  | OutputRegistered
  -- ^ (Not supported by Clash simulation.)  Output registered, (no enable)
  | OutputRegisteredEnable
  -- ^ (Not supported by Clash simulation.) Output registered with enable (enable
  -- is not registered)
  | OutputRegisteredEnableRegistered
  -- ^ (Not supported by Clash simulation.) Output registered and enable
  -- registered
  | OutputDDR
  -- ^ (Not supported by Clash simulation.) Output DDR data is clocked out on
  -- rising and falling clock edges
  | OutputDDREnable
  -- ^ (Not supported by Clash simulation.) Output data is clocked out on rising
  -- and falling clock edges
  | OutputDDREnableRegistered
  -- ^ (Not supported by Clash simulation.) Output DDR data with registered
  -- enable signal
  | OutputRegisteredInverted
  --  ^ (Not supported by Clash simulation.) Output registered signal is inverted
  | OutputRegisteredEnableInverted
  -- ^ (Not supported by Clash simulation.) Output signal is registered and
  -- inverted (no enable function)
  | OutputRegisteredEnableRegisteredInverted
  -- ^ (Not supported by Clash simulation.) Output signal is registered and
  -- inverted, the enable/tristate control is registered.
  deriving (Eq, Show)

instance Arbitrary PinOutputConfig where
  arbitrary = QC.elements
    -- Never pick NoOutout, this means that the actual value will always be
    -- all undefined bits which is pointless to test for.
    [ Output
    , OutputTristate
    , OutputEnableRegistered
    , OutputRegistered
    , OutputRegisteredEnable
    , OutputRegisteredEnableRegistered
    , OutputDDR
    , OutputDDREnable
    , OutputDDREnableRegistered
    , OutputRegisteredInverted
    , OutputRegisteredEnableInverted
    , OutputRegisteredEnableRegisteredInverted
    ]

instance BitPack PinOutputConfig where
  type BitSize PinOutputConfig = 4
  pack =
    \case
      NoOutput -> 0b0000
      Output -> 0b0110
      OutputTristate -> 0b1010
      OutputEnableRegistered -> 0b1110
      OutputRegistered -> 0b0101
      OutputRegisteredEnable -> 0b1001
      OutputRegisteredEnableRegistered -> 0b1101
      OutputDDR -> 0b0100
      OutputDDREnable -> 0b1000
      OutputDDREnableRegistered -> 0b1100
      OutputRegisteredInverted -> 0b0111
      OutputRegisteredEnableInverted -> 0b1011
      OutputRegisteredEnableRegisteredInverted -> 0b1111

  unpack =
    \case
      0b0000 -> NoOutput
      0b0110 -> Output
      0b1010 -> OutputTristate
      0b1110 -> OutputEnableRegistered
      0b0101 -> OutputRegistered
      0b1001 -> OutputRegisteredEnable
      0b1101 -> OutputRegisteredEnableRegistered

      0b0100 -> OutputDDR
      0b1000 -> OutputDDREnable
      0b1100 -> OutputDDREnableRegistered

      0b0111 -> OutputRegisteredInverted
      0b1011 -> OutputRegisteredEnableInverted
      0b1111 -> OutputRegisteredEnableRegisteredInverted

      b -> errorX $ "Unrecognized bit pattern in PinOutputConfig.unpack: " <> show b

-- | ICE low-power latch
iceGate
  :: ( HasCallStack
     , HiddenClock dom
     , NFDataX a
     )
  => Signal dom Bool
  -> Signal dom a
  -> Signal dom a
iceGate latch x =
  let qR = dflipflop q
      q = mux latch qR x
   in q

-- | Tristate buffer
tristate
  :: ( HasCallStack
     )
  => Signal dom Bool
  -> Signal dom a
  -> Signal dom (Maybe a)
tristate =
  liftA2 (\e x -> if e then Just x else Nothing)

-- Variant of Clash.Explicit.DDR.ddrIn without a reset line.
ddrIn
  :: forall slow fast period edge reset init polarity a
   . ( HasCallStack
     , KnownConfiguration slow ('DomainConfiguration slow (2 * period) edge reset init polarity)
     , KnownConfiguration fast ('DomainConfiguration fast period edge reset init polarity)
     , HiddenClock slow
     , HiddenEnable slow
     , NFDataX a
     )
  => Signal fast a
  -> Signal slow (a, a)
ddrIn =
  let en = fromEnable (hasEnable @slow)
      o = deepErrorX "ddrIn: undefined"
   in go (o, o, o) en
 where
  go now@(o0, o1, o2) ~(e :- es) as@(~(x0 :- x1 :- xs)) =
    let next = (o2, x0, x1)
     in o0 `seqX` o1 `seqX` (o0, o1) :-
       (as `seq` if e then go next es xs else go now es xs)

-- Variant of Clash.Explicit.DDR.ddrOut without a reset line.
ddrOut
  :: forall slow fast period edge reset init polarity a
   . ( HasCallStack
     , KnownConfiguration slow ('DomainConfiguration slow (2 * period) edge reset init polarity)
     , KnownConfiguration fast ('DomainConfiguration fast period edge reset init polarity)
     , HiddenClock slow
     , HiddenEnable slow
     , NFDataX a
     )
  => Signal slow a
  -> Signal slow a
  -> Signal fast a
ddrOut ddr0 ddr1 =
  let (_ :- out) = zipSignals ddr0' ddr1' in out
 where
  ddr0' = delay (deepErrorX "ddr0: undefined") ddr0
  ddr1' = delay (deepErrorX "ddr1: undefined") ddr1

  zipSignals (x :- xs) (y :- ys) =
    x :- y :- zipSignals xs ys

-- TODO
--
-- The current implementations of the primitives here support only pin
-- configurations with no DDR (sbio) or DDR input and output (sbioDDR). The
-- real primitive supports a greater range of configurations (input only,
-- output only, DDR input / SDR output, SDR input / DDR output). This can be
-- implemented, but would probably require that the pin configuration types
-- are GADTs, and special types are made for D_OUT and D_IN.

-- | SB_IO primitive as described in LITL p87.
-- This does not support DDR configurations, see 'sbioDDR'.
sbio
  :: forall dom
   . ( HasCallStack
     , HiddenClock dom  -- INPUT_CLK / OUTPUT_CLK
     , HiddenEnable dom -- CLK_ENABLE
     )
  => PinInputConfig
  -- ^ Pin Input (PIN_TYPE[1:0])
  -> PinOutputConfig
  -- ^ Pin Output (PIN_TYPE[5:2])
  -> BiSignalIn 'Floating dom 1
  -- ^ PACKAGE_PIN
  -> Signal dom Bit
  -- ^ LATCH_INPUT_VALUE
  -> Signal dom Bit
  -- ^ D_OUT_0
  -> Signal dom Bool
  -- ^ OUTPUT_ENABLE
  -> ( BiSignalOut 'Floating dom 1  -- PACKAGE_PIN
     , Signal dom Bit               -- D_IN_0
     )
sbio pinIn pinOut =
  sbioPrim (pack pinOut ++# pack pinIn)
{-# INLINE sbio #-}

sbioPrim
  :: forall dom
   . ( HasCallStack
     , KnownDomain dom
     , HiddenClock dom  -- INPUT_CLK / OUTPUT_CLK
     , HiddenEnable dom -- CLK_ENABLE
     )
  => BitVector 6
  -- ^ PIN_TYPE, see 'PinInputConfig' and 'PinOutputConfig'
  -> BiSignalIn 'Floating dom 1
  -- ^ PACKAGE_PIN
  -> Signal dom Bit
  -- ^ LATCH_INPUT_VALUE
  -> Signal dom Bit
  -- ^ D_OUT_0
  -> Signal dom Bool
  -- ^ OUTPUT_ENABLE
  -> ( BiSignalOut 'Floating dom 1  -- PACKAGE_PIN
     , Signal dom Bit               -- D_IN_0
     )
sbioPrim pinConf pkgPinIn latchInput dOut0 outputEnable =
  (writeToBiSignal pkgPinIn pkgPinInWrite, dIn0)
 where
  pinIn = unpack (slice d1 d0 pinConf)
  pinOut = unpack (slice d5 d2 pinConf)
  pkgPinInRead = readFromBiSignal @Bit pkgPinIn

  dIn0 =
    case pinIn of
      Input ->
        pkgPinInRead

      InputRegistered ->
        delay (deepErrorX "dIn0: undefined") pkgPinInRead

      InputLatch ->
        iceGate (fmap bitToBool latchInput) pkgPinInRead

      InputRegisteredLatch ->
        delay (deepErrorX "dIn0: undefined") $ iceGate (fmap bitToBool latchInput) pkgPinInRead

  pkgPinInWrite =
    case pinOut of
      NoOutput ->
        pure Nothing

      Output ->
        fmap Just dOut0

      OutputTristate ->
        tristate outputEnable dOut0

      OutputEnableRegistered ->
        tristate (delay False outputEnable) dOut0

      OutputRegistered ->
        fmap Just (delay (deepErrorX "pkgPinInWrite: undefined") dOut0)

      OutputRegisteredEnable ->
        tristate outputEnable (delay (deepErrorX "pkgPinInWrite: undefined") dOut0)

      OutputRegisteredEnableRegistered ->
        tristate
          (delay False outputEnable)
          (delay (deepErrorX "pkgPinInWrite: undefined") dOut0)

      OutputRegisteredInverted ->
        fmap Just (delay (deepErrorX "pkgPinInWrite: undefined") (fmap complement dOut0))

      OutputRegisteredEnableInverted ->
        tristate outputEnable (delay (deepErrorX "pkgPinInWrite: undefined") (fmap complement dOut0))

      OutputRegisteredEnableRegisteredInverted ->
        tristate
          (delay False outputEnable)
          (delay (deepErrorX "pkgPinInWrite: undefined") (fmap complement dOut0))

      _ -> error "sbio: DDR is not supported, see sbioDDR"

{-# NOINLINE sbioPrim #-}
{-# ANN sbioPrim hasBlackBox #-}
{-# ANN sbioPrim (InlinePrimitive [VHDL,Verilog,SystemVerilog] $ unindent [i|
   [ { "BlackBox" :
        { "name" : "Clash.Cores.LatticeSemi.ICE40.IO.sbioPrim",
          "kind" : "Declaration",
          "format": "Haskell",
          "templateFunction": "Clash.Cores.LatticeSemi.ICE40.Blackboxes.IO.sbioTF"
        }
     }
   ]
   |]) #-}

sbioDDR
  :: forall slow fast period edge reset init polarity
   . ( HasCallStack
     , KnownDomain slow
     , KnownConfiguration slow ('DomainConfiguration slow (2 * period) edge reset init polarity)
     , KnownConfiguration fast ('DomainConfiguration fast period edge reset init polarity)
     , HiddenClock slow  -- INPUT_CLK / OUTPUT_CLK
     , HiddenEnable slow -- CLK_ENABLE
     )
  => PinInputConfig
  -- ^ Pin Input (PIN_TYPE[1:0])
  -> PinOutputConfig
  -- ^ Pin Output (PIN_TYPE[5:2])
  -> BiSignalIn 'Floating fast 1
  -- ^ PACKAGE_PIN
  -> Signal slow Bit
  -- ^ D_OUT_0
  -> Signal slow Bit
  -- ^ D_OUT_1
  -> Signal slow Bool
  -- ^ OUTPUT_ENABLE
  -> ( BiSignalOut 'Floating fast 1  -- PACKAGE_PIN
     , Signal slow Bit               -- D_IN_0
     , Signal slow Bit               -- D_IN_1
     )
sbioDDR pinIn pinOut =
  sbioDDRPrim (pack pinOut ++# pack pinIn)
{-# INLINE sbioDDR #-}

sbioDDRPrim
  :: forall slow fast period edge reset init polarity
   . ( HasCallStack
     , KnownDomain slow
     , KnownConfiguration slow ('DomainConfiguration slow (2 * period) edge reset init polarity)
     , KnownConfiguration fast ('DomainConfiguration fast period edge reset init polarity)
     , HiddenClock slow  -- INPUT_CLK / OUTPUT_CLK
     , HiddenEnable slow -- CLK_ENABLE
     )
  => BitVector 6
  -- ^ PIN_TYPE, see 'PinInputConfig' and 'PinOutputConfig'
  -> BiSignalIn 'Floating fast 1
  -- ^ PACKAGE_PIN
  -> Signal slow Bit
  -- ^ D_OUT_0
  -> Signal slow Bit
  -- ^ D_OUT_1
  -> Signal slow Bool
  -- ^ OUTPUT_ENABLE
  -> ( BiSignalOut 'Floating fast 1  -- PACKAGE_PIN
     , Signal slow Bit               -- D_IN_0
     , Signal slow Bit               -- D_IN_1
     )
sbioDDRPrim pinConf pkgPinIn dOut0 dOut1 outputEnable =
  (writeToBiSignal pkgPinIn pkgPinInWrite, dIn0, dIn1)
 where
  pinIn = unpack (slice d1 d0 pinConf)
  pinOut = unpack (slice d5 d2 pinConf)
  pkgPinInRead = readFromBiSignal @Bit pkgPinIn

  (dIn0, dIn1) =
    case pinIn of
      -- This is really PIN_INPUT_DDR because we're in DDR mode
      Input -> unbundle (ddrIn pkgPinInRead)
      _ -> error "sbioDDR: Non-DDR input is not supported, see sbio"

  pkgPinInWrite =
    case pinOut of
      NoOutput ->
        pure Nothing

      OutputDDR ->
        fmap Just (ddrOut dOut0 dOut1)

      OutputDDREnable ->
        let en = E.veryUnsafeSynchronizer 2 1 outputEnable
         in tristate en (ddrOut dOut0 dOut1)

      OutputDDREnableRegistered ->
        let en = E.veryUnsafeSynchronizer 2 1 (delay False outputEnable)
         in tristate en (ddrOut dOut0 dOut1)

      _ -> error "sbioDDR: Non-DDR output is not support, see sbio"
{-# NOINLINE sbioDDRPrim #-}
{-# ANN sbioDDRPrim hasBlackBox #-}
{-# ANN sbioDDRPrim (InlinePrimitive [VHDL,Verilog,SystemVerilog] $ unindent [i|
   [ { "BlackBox" :
        { "name" : "Clash.Cores.LatticeSemi.ICE40.IO.sbioDDRPrim",
          "kind" : "Declaration",
          "format": "Haskell",
          "templateFunction": "Clash.Cores.LatticeSemi.ICE40.Blackboxes.IO.sbioDDRTF"
        }
     }
   ]
   |]) #-}

