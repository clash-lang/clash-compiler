{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Cdc.ArraySingle
  ( xpmCdcArraySingle
  , XpmCdcArraySingleConfig(..)
  , xpmCdcArraySingleWith
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal((:-)))

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal

-- | Synchronizes an array of independent bits from the source clock domain to
-- the destination. Be careful when using this primitive: each bit will be
-- synchronized independently. That is to say, a word appearing in the source
-- domain will not necessarily appear in the destination domain. If this word
-- synchronization is needed, consider using a FIFO or
-- 'Clash.Cores.Xilinx.Xpm.Cdc.Handshake.xpmCdcHandshake'.
--
-- By default, it registers its input, uses four synchronization stages, and
-- auto-detects whether to use initial values for the synchronization registers.
-- Use 'xpmCdcArraySingleWith' to change these settings. For more information
-- see [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_ARRAY_SINGLE).
--
-- __N.B.__: In order to simulate initial values, both the source and destination
--           domain need to support them. If the source and destination domain
--           disagree on this property, use of this function will fail to
--           simulate and translate to HDL. You can explicitly set it using
--           'xpmCdcArraySingleWith'.
xpmCdcArraySingle ::
  forall a src dst.
  ( 1 <= BitSize a, BitSize a <= 1024
  , KnownDomain src
  , KnownDomain dst
  , HasCallStack
  , NFDataX a
  , BitPack a
  ) =>
  Clock src ->
  Clock dst ->
  Signal src a ->
  Signal dst a
xpmCdcArraySingle = xpmCdcArraySingleWith XpmCdcArraySingleConfig{..}
 where
  registerInput = True
  stages = d4
  initialValues =
    case (initBehavior @src, initBehavior @dst) of
      (SDefined, SDefined) -> True
      (SUnknown, SUnknown) -> False
      _ -> clashCompileError $ "xpmCdcArraySingle: domains need to agree on initial value "
                            <> "behavior. To set initial value usage explicitly, "
                            <> "consider using 'xpmCdcArraySingleWith'."
{-# INLINE xpmCdcArraySingle #-}

-- | Configuration for 'xpmCdcArraySingleWith'
data XpmCdcArraySingleConfig stages = XpmCdcArraySingleConfig
  { -- | Number of synchronization stages, i.e., number of registers in the
    -- destination domain.
    stages :: SNat stages

    -- | Initialize registers used within the primitive to /0/. Note that
    -- 'xpmCdcArraySingle' will set this to 'True' if both domains support initial
    -- values, to 'False' if neither domain does, and will otherwise emit an
    -- error.
  , initialValues :: Bool

    -- | Register input. This makes sure the synchronization pipeline does not
    -- see glitches.
  , registerInput :: Bool
  }

-- | Like 'xpmCdcArraySingle', but with a configurable number of stages, initial
-- values, and registered input. Also see 'XpmCdcArraySingleConfig'.
xpmCdcArraySingleWith ::
  forall stages a src dst.
  ( 2 <= stages, stages <= 10
  , 1 <= BitSize a, BitSize a <= 1024
  , KnownDomain src
  , KnownDomain dst
  , HasCallStack
  , NFDataX a
  , BitPack a
  ) =>
  XpmCdcArraySingleConfig stages ->
  Clock src ->
  Clock dst ->
  Signal src a ->
  Signal dst a
xpmCdcArraySingleWith XpmCdcArraySingleConfig{stages=stages@SNat, ..} clkSrc clkDst srcIn
  | clashSimulation = sim
  | otherwise = synth
 where
  -- Definition used in for HDL generation
  synth = unpack <$> unPort go
   where
    go :: Port "dest_out" dst (BitVector (BitSize a))
    go =
      inst
        (instConfig "xpm_cdc_array_single")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"DEST_SYNC_FF"   @Integer (natToNum @stages))
        (Param @"INIT_SYNC_FF"   @Integer (if initialValues then 1 else 0))
        (Param @"SIM_ASSERT_CHK" @Integer 0)
        (Param @"SRC_INPUT_REG"  @Integer (if registerInput then 1 else 0))
        (Param @"WIDTH"          @Integer (natToNum @(BitSize a)))

        (ClockPort @"src_clk"  clkSrc)
        (ClockPort @"dest_clk" clkDst)
        (Port      @"src_in"   (pack <$> srcIn))
  -- Definition used in Clash simulation
  sim
    | registerInput = go (snatToNum stages) (initVal :- srcIn)
    | otherwise     = go (snatToNum stages) srcIn
   where
    initVal
      | initialValues = unpack 0
      | otherwise = deepErrorX "xpmCdcSingle: initial values undefined"

    go :: Word -> Signal src a -> Signal dst a
    go 0 src = unsafeSynchronizer clkSrc clkDst src
    go n src = initVal :- go (n - 1) src

{-# INLINE xpmCdcArraySingleWith #-}
