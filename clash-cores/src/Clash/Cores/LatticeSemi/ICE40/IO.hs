{-|
  Copyright   :  (C) 2019, Foamspace corp
                 (C) 2021, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  LATTICE ICE IO primitives. Implementations are documented in the
  <http://www.latticesemi.com/~/media/LatticeSemi/Documents/TechnicalBriefs/SBTICETechnologyLibrary201504.pdf LATTICE ICE Technology Library>,
  referred to as LITL.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.LatticeSemi.ICE40.IO
  ( sbio
  , sbioDDR
  , PinOutputConfig(..)
  , PinInputConfig(..)
  , sb_io
  ) where

import           Clash.Annotations.Primitive  (Primitive(..), HDL(..), hasBlackBox)
import qualified Clash.Explicit.Signal as E
import           Clash.Prelude
import           Clash.Signal.Internal        (Signal(..))

import           Data.String.Interpolate      (i)
import           Data.String.Interpolate.Util (unindent)
import           GHC.Stack                    (HasCallStack)
import           GHC.Natural                  (Natural)
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

sb_io ::
  forall slow fast iO_STANDARD .
  (KnownDomain slow, KnownDomain fast) =>
  -- | NEG_TRIGGER
  Bool ->
  -- | PIN_TYPE
  BitVector 6 ->
  -- | PULLUP
  Bool ->
  -- | IO_STANDARD
  SSymbol iO_STANDARD ->
  -- | D_OUT_1
  Signal slow Bit ->
  -- | D_OUT_0
  Signal slow Bit ->
  -- | CLOCK_ENABLE
  Signal slow Bool ->
  -- | LATCH_INPUT_VALUE
  Signal slow Bool ->
  -- | INPUT_CLK
  Clock slow ->
  -- | OUTPUT_ENABLE
  Signal slow Bool ->
  -- | OUTPUT_CLK
  Clock slow ->
  -- | PACKAGE_PIN
  BiSignalIn 'Floating fast 1 ->
  -- | (D_IN_1, D_IN_0, PACKAGE_PIN)
  ( Signal slow Bit
  , Signal slow Bit
  , BiSignalOut 'Floating fast 1
  )
sb_io nEG_TRIGGER pIN_TYPE !_pULLUP !_iO_STANDARD d_out_1 d_out_0 clock_enable
  latch_input_value !_input_clk output_enable !_output_clk package_pin_in
    = (d_in_1, d_in_0, package_pin_out)
  where
    package_pin_out = writeToBiSignal package_pin_in
                        (mux padoen (pure Nothing) (Just <$> padout))

    clockRatio = getClockRatio (snatToNatural (clockPeriod @slow))
                               (snatToNatural (clockPeriod @fast))

    inclk_n, outclk_n :: Signal fast Bool
    inclk_n  = case clockRatio of
                 SDR -> pure True
                 DDR -> not nEG_TRIGGER :- nEG_TRIGGER :- inclk_n
    outclk_n = case clockRatio of
                 SDR -> pure True
                 DDR -> not nEG_TRIGGER :- nEG_TRIGGER :- inclk_n

    inclke_sync = E.veryUnsafeSynchronizer
                    (snatToNum (clockPeriod @fast))
                    (snatToNum (clockPeriod @slow))
                    clock_enable

    outclke_sync = E.veryUnsafeSynchronizer
                    (snatToNum (clockPeriod @fast))
                    (snatToNum (clockPeriod @slow))
                    clock_enable

    inclk  = liftA2 (&&) inclk_n inclke_sync
    outclk = liftA2 (&&) outclk_n outclke_sync

    (d_in_1, d_in_0, padout, padoen) =
      prio_physical clockRatio d_out_1 d_out_0 output_enable latch_input_value
        inclk outclk pIN_TYPE (readFromBiSignal @Bit package_pin_in)
{-# NOINLINE sb_io #-}
{-# ANN sb_io hasBlackBox #-}
{-# ANN sb_io (InlinePrimitive [VHDL,Verilog,SystemVerilog] $ unindent [i|
   [ { "BlackBox" :
        { "name" : "Clash.Cores.LatticeSemi.ICE40.IO.sb_io",
          "kind" : "Declaration",
          "format": "Haskell",
          "templateFunction": "Clash.Cores.LatticeSemi.ICE40.Blackboxes.IO.sb_io_tf"
        }
     }
   ]
   |]) #-}

prio_physical ::
  forall slow fast .
  (KnownDomain slow, KnownDomain fast) =>
  ClockRatio ->
  -- | ddr1
  Signal slow Bit ->
  -- | ddr0
  Signal slow Bit ->
  -- | oepin
  Signal slow Bool ->
  -- | hold
  Signal slow Bool ->
  -- | inclk
  Signal fast Bool ->
  -- | outclk
  Signal fast Bool ->
  -- | cbit
  BitVector 6 ->
  -- | padin
  Signal fast Bit ->
  -- | (dout1, dout0, padout, padoen)
  ( Signal slow Bit
  , Signal slow Bit
  , Signal fast Bit
  , Signal fast Bool
  )
prio_physical clockRatio ddr1 ddr0 oepin hold inclk outclk cbit padin = (dout1, dout0, padout, padoen)
  where
    {- bs_en = '0'
       shift = '0'
       tclk  = '0'
       update = '0'
       sdi = '0'
       mode = '0'
       hiz_b = '1'
       rstio = '0'
       jtag_update_n30 = '1'
    -}
    dout1 = din_reg_1
    padin_n1 = padin
    inclk_n2 = inclk

    din_reg_0 = case clockRatio of
       SDR -> goSDR undefined inclk_n2 padin_n1
       DDR -> goDDR undefined inclk_n2 padin_n1
     where
       goSDR o ~(c0 :- cs) ~(p :- ps) =
         let oN | c0 = p
                | otherwise = o
          in o :- goSDR oN cs ps

       goDDR o ~(c0 :- c1 :- cs) ~(p1 :- p2 :- ps) =
         let oN | c0 && not c1 = p1
                | not c0 && c1 = p2
                | otherwise    = o
          in o :- goDDR oN cs ps

    pad_n3 = padin

    din_reg_1 = case clockRatio of
        SDR -> goSDR undefined inclk pad_n3
        DDR -> goDDR undefined inclk pad_n3
      where
        goSDR o ~(c0 :- cs) ~(p :- ps) =
         let oN | c0 = p
                | otherwise = o
          in o :- goSDR oN cs ps

        goDDR o ~(c0 :- c1 :- cs) ~(p1 :- p2 :- ps) =
         let oN | c0 && not c1 = p2
                | not c0 && c1 = p1
                | otherwise    = o
          in o :- goDDR oN cs ps

    hold_AND2 = (pack (cbit ! (1 :: Word)) .&.) <$> (bitCoerce <$> hold)
    -- Input mux
    temp1 :: Signal slow (BitVector 2)
    temp1 = liftA2 (++#) (pack <$> hold_AND2) (pure (pack (cbit ! (0 :: Word))))

    dout0 = case clockRatio of
        SDR -> goSDR undefined temp1 din_reg_0 padin
        DDR -> goDDR undefined temp1 din_reg_0 padin
      where
        goSDR :: Bit -> Signal slow (BitVector 2) -> Signal slow Bit -> Signal fast Bit -> Signal slow Bit
        goSDR o ~(t1 :- ts) ~(d :- ds) ~(p :- ps) = case t1 of
          0b00 -> d :- goSDR d ts ds ps
          0b01 -> p :- goSDR p ts ds ps
          0b10 -> o :- goSDR o ts ds ps
          0b11 -> o :- goSDR o ts ds ps
          _    -> low :- goSDR low ts ds ps

        goDDR :: Bit -> Signal slow (BitVector 2) -> Signal slow Bit -> Signal fast Bit -> Signal slow Bit
        goDDR o ~(t1 :- ts) ~(d :- ds) ~(p :- _ :- ps) = case t1 of
          0b00 -> d :- goDDR d ts ds ps
          0b01 -> p :- goDDR p ts ds ps
          0b10 -> o :- goDDR o ts ds ps
          0b11 -> o :- goDDR o ts ds ps
          _    -> low :- goDDR low ts ds ps

    dout_reg_0 :: Signal fast Bit
    dout_reg_0 = case clockRatio of
        SDR -> goSDR undefined outclk_n12 ddr0_n11
        DDR -> goDDR undefined outclk_n12 ddr0_n11
      where
        goSDR o ~(c0 :- cs) ~(d :- ds) =
          let oN | c0 = d
                 | otherwise = o
           in o :- goSDR oN cs ds

        goDDR o ~(c0 :- c1 :- cs) ~(p1 :- p2 :- ps) =
          let oN | c0 && not c1 = p1
                | not c0 && c1 = p2
                | otherwise    = o
          in o :- goDDR oN cs ps

    dout_reg_0_n = complement <$> dout_reg_0
    n19 = not <$> ((|| bitCoerce (cbit ! (2 :: Word))) <$> outclk_n12)

    reg_or_wire_n17 | cbit ! (2 :: Word) == high = dout_reg_0_n
                    | otherwise        = E.veryUnsafeSynchronizer
                                           (snatToNum (clockPeriod @slow))
                                           (snatToNum (clockPeriod @fast))
                                           ddr0

    n18 = mux n19 dout_reg_1 dout_reg_0
    n14 | cbit ! (3 :: Word) == high = reg_or_wire_n17
        | otherwise        = n18

    padout = n14

    ddr0_n11 = ddr0
    outclk_n12 = outclk
    ddr1_n13 = ddr1

    dout_reg_1 :: Signal fast Bit
    dout_reg_1 = case clockRatio of
        SDR -> goSDR undefined outclk_n12 ddr1_n13
        DDR -> goDDR undefined outclk_n12 ddr1_n13
      where
        goSDR o ~(c0 :- cs) ~(d :- ds) =
          let oN | c0 = d
                 | otherwise = o
           in o :- goSDR oN cs ds

        goDDR o ~(c0 :- c1 :- cs) ~(p1 :- p2 :- ps) =
          let oN | c0 && not c1 = p2
                | not c0 && c1 = p1
                | otherwise    = o
          in o :- goDDR oN cs ps

    outclk_n22 = outclk

    tristate   = E.veryUnsafeSynchronizer (snatToNum (clockPeriod @slow))
                                          (snatToNum (clockPeriod @fast))
                                          oepin
    tristate_q = E.delay clockGen (toEnable outclk_n22) undefined tristate

    oen_n_n24 = go (slice d5 d4 cbit) <$> tristate <*> tristate_q
      where
        go 0b00 _ _  = False
        go 0b01 _ _  = True
        go 0b10 op _ = op
        go 0b11 _ tq = tq
        go _ _ _     = False

    n26 = oen_n_n24
    padoen = not <$> n26


data ClockRatio = SDR | DDR

getClockRatio :: Natural -> Natural -> ClockRatio
getClockRatio slow fast
  | slow == fast     = SDR
  | 2 * slow == fast = DDR
  | otherwise        = error "Not SDR nor DDR"
