{-|
  Copyright   :  (C) 2019, Foamspace corp
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  LATTICE ICE IO primitives. Implementations are documented in the
  <http://www.latticesemi.com/~/media/LatticeSemi/Documents/TechnicalBriefs/SBTICETechnologyLibrary201504.pdf LATTICE ICE Technology Library>,
  referred to as LITL.
-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Cores.LatticeSemi.ICE40.IO
  ( sbio
  , spiConfig
  , PinOutputConfig(..)
  , PinInputConfig(..)
  ) where

import           Data.Functor                 ((<&>))
import           GHC.Stack                    (HasCallStack())

import           Clash.Annotations.Primitive  (Primitive(..), HDL(..), hasBlackBox)
import           Clash.Prelude

import           Data.String.Interpolate      (i)
import           Data.String.Interpolate.Util (unindent)

toMaybe :: Bool -> a -> Maybe a
toMaybe True a = Just a
toMaybe False _a = Nothing

-- | Create configuration bitvector based on pin function mnemonics. See
-- documentation on "PinInputConfig" and "PinOutputConfig" for more information.
spiConfig
  :: PinInputConfig
  -> PinOutputConfig
  -> BitVector 6
spiConfig pic poc = pack poc ++# pack pic

-- | Input pinType  mnemonics as documented in the first table of LITL p88. Note
-- that @PIN_INPUT_DDR@ is missing. Use 'PIN_INPUT_REGISTERED' instead.
data PinInputConfig
  = PIN_INPUT_REGISTERED
  -- ^ Input data is registered in input cell. Same as @PIN_INPUT_DDR@.
  | PIN_INPUT
  -- ^ Simple input pin (D_IN_0)
  | PIN_INPUT_REGISTERED_LATCH
  -- ^ (Not supported by Clash simulation.) Disables internal data changes on
  -- the physical input pin by latching the value on the input register.
  | PIN_INPUT_LATCH
  -- ^ (Not supported by Clash simulation.) Disables internal data changes on
  -- the physical input pin by latching the value.
  deriving (Show, Generic, BitPack)

-- | Output pinType  mnemonics as documented in the second table of LITL p88.
data PinOutputConfig
  = PIN_NO_OUTPUT
  -- ^ Disables the output function
  | PIN_OUTPUT
  -- ^ Simple output pin, (no enable)
  | PIN_OUTPUT_TRISTATE
  -- ^ The output pin may be tristated using the enable
  | PIN_OUTPUT_ENABLE_REGISTERED
  -- ^ The output pin may be tristated using a registered enable signal
  | PIN_OUTPUT_REGISTERED
  -- ^ (Not supported by Clash simulation.)  Output registered, (no enable)
  | PIN_OUTPUT_REGISTERED_ENABLE
  -- ^ (Not supported by Clash simulation.) Output registered with enable (enable
  -- is not registered)
  | PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED
  -- ^ (Not supported by Clash simulation.) Output registered and enable
  -- registered
  | PIN_OUTPUT_DDR
  -- ^ (Not supported by Clash simulation.) Output DDR data is clocked out on
  -- rising and falling clock edges
  | PIN_OUTPUT_DDR_ENABLE
  -- ^ (Not supported by Clash simulation.) Output data is clocked out on rising
  -- and falling clock edges
  | PIN_OUTPUT_DDR_ENABLE_REGISTERED
  -- ^ (Not supported by Clash simulation.) Output DDR data with registered
  -- enable signal
  | PIN_OUTPUT_REGISTERED_INVERTED
  --  ^ (Not supported by Clash simulation.) Output registered signal is inverted
  | PIN_OUTPUT_REGISTERED_ENABLE_INVERTED
  -- ^ (Not supported by Clash simulation.) Output signal is registered and
  -- inverted (no enable function)
  | PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED_INVERTED
  -- ^ (Not supported by Clash simulation.) Output signal is registered and
  -- inverted, the enable/tristate control is registered.
  deriving (Show)

instance BitPack PinOutputConfig where
  type BitSize PinOutputConfig = 4
  pack =
    \case
      PIN_NO_OUTPUT -> 0b0000
      PIN_OUTPUT -> 0b0110
      PIN_OUTPUT_TRISTATE -> 0b1010
      PIN_OUTPUT_ENABLE_REGISTERED -> 0b1110
      PIN_OUTPUT_REGISTERED -> 0b0101
      PIN_OUTPUT_REGISTERED_ENABLE -> 0b1001
      PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED -> 0b1101
      PIN_OUTPUT_DDR -> 0b0100
      PIN_OUTPUT_DDR_ENABLE -> 0b1000
      PIN_OUTPUT_DDR_ENABLE_REGISTERED -> 0b1100
      PIN_OUTPUT_REGISTERED_INVERTED -> 0b0111
      PIN_OUTPUT_REGISTERED_ENABLE_INVERTED -> 0b1011
      PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED_INVERTED -> 0b1111

  unpack =
    \case
      0b0000 -> PIN_NO_OUTPUT
      0b0110 -> PIN_OUTPUT
      0b1010 -> PIN_OUTPUT_TRISTATE
      0b1110 -> PIN_OUTPUT_ENABLE_REGISTERED
      0b0101 -> PIN_OUTPUT_REGISTERED
      0b1001 -> PIN_OUTPUT_REGISTERED_ENABLE
      0b1101 -> PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED

      0b0100 -> PIN_OUTPUT_DDR
      0b1000 -> PIN_OUTPUT_DDR_ENABLE
      0b1100 -> PIN_OUTPUT_DDR_ENABLE_REGISTERED

      0b0111 -> PIN_OUTPUT_REGISTERED_INVERTED
      0b1011 -> PIN_OUTPUT_REGISTERED_ENABLE_INVERTED
      0b1111 -> PIN_OUTPUT_REGISTERED_ENABLE_REGISTERED_INVERTED

      b -> errorX $ "Unrecognized bit pattern in PinOutputConfig.unpack: " <> show b

data OutputEnable
  = EnableLow
  | EnableHigh
  | EnablePass
  | EnableLatched
  deriving (Show, Generic, BitPack)

data OutputSelect
  = SelectDDR0
  | SelectDDR1
  | SelectD0
  | SelectLatchedInvertedD0
  deriving (Show, Generic, BitPack)

dflipflopE
  :: ( HasCallStack
     , HiddenClock dom
     , HiddenEnable dom
     , NFDataX a
     )
  => Signal dom a
  -> Signal dom a
dflipflopE = delay (deepErrorX "dflipflopE: undefined initial value")

-- | SB_IO primitive as described in LITL p87.
sbio
  :: forall dom
   . ( HasCallStack
     , HiddenClock dom  -- INPUT_CLK
     , HiddenEnable dom -- CLK_ENABLE
     )
  => BitVector 6
  -- ^ Config, see 'spiConfig'
  -> BiSignalIn 'Floating dom 1
  -- ^ PACKAGE_PIN
  -> Signal dom Bit
  -- ^ LATCH_INPUT_VALUE
  -> Signal dom Bit
  -- ^ D_OUT_0
  -> Signal dom Bit
  -- ^ D_OUT_1 (Not implemented yet.)
  -> Signal dom Bool
  -- ^ OUTPUT_ENABLE
  -> ( BiSignalOut 'Floating dom 1  -- PACKAGE_PIN
     , Signal dom Bit               -- D_IN_0
     , Signal dom Bit               -- D_IN_1
     )
sbio pinConf pkgPinIn latchInput dOut_0 _dOut_1 outputEnable0 =
  ( pkgPinOut
  , dIn_0
  , pure (errorX "d_in_1 not yet implemented")
  )
 where
  pkgPinInRead :: Signal dom Bit
  pkgPinInRead = readFromBiSignal pkgPinIn

  -- Combine (static) input pin configuration values with (dynamic) value
  -- of LATCH_INPUT_VALUE
  pinType =
    latchInput <&> \li ->
      unpack $
      pack $
        ( (li .|. (pinConf ! (1 :: Int)))
        , (pinConf ! (0 :: Int))
        )

  clockLessLatchErr =
    "Either LATCH_INPUT_VALUE was asserted or pin type was set to one of " <>
    "INPUT_REGISTERED_LATCH or PIN_INPUT_LATCH. This is currently not " <>
    "supported by this 'Clash.Cores.LatticeSemi.ICE40.IO.sbio', due to CLash not " <>
    "supporting clockless latches."

  latch_dIn_0 =
    pinType <&>
      \case
        PIN_INPUT_REGISTERED -> True
        PIN_INPUT -> False
        PIN_INPUT_REGISTERED_LATCH -> errorX clockLessLatchErr
        PIN_INPUT_LATCH -> errorX clockLessLatchErr

  dIn_0 =
    mux latch_dIn_0 (dflipflopE pkgPinInRead) pkgPinInRead

  outputEnable1 =
    case unpack (slice d5 d4 pinConf) of
      EnableLow     -> pure False
      EnableHigh    -> pure True
      EnablePass    -> outputEnable0
      EnableLatched -> error "OutputLatched: Not yet implemented"

  pkgPinWriteInput =
    case unpack (slice d3 d2 pinConf) of
      SelectD0 -> dOut_0
      opt -> error $ "OutputSelect." <> show opt <> " not yet implemented"

  pkgPinOut =
    writeToBiSignal
      pkgPinIn
      (toMaybe <$> outputEnable1 <*> pkgPinWriteInput)
{-# NOINLINE sbio #-}
{-# ANN sbio hasBlackBox #-}
{-# ANN sbio (InlinePrimitive [VHDL,Verilog,SystemVerilog] $ unindent [i|
   [ { "BlackBox" :
        { "name" : "Clash.Cores.LatticeSemi.ICE40.IO.sbio",
          "kind" : "Declaration",
          "format": "Haskell",
          "templateFunction": "Clash.Cores.LatticeSemi.ICE40.Blackboxes.IO.sbioTF"
        }
     }
   ]
   |]) #-}
