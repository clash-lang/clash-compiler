{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Cdc.Pulse
  ( xpmCdcPulse
  , XpmCdcPulseConfig(..)
  , xpmCdcPulseWith
  ) where

import GHC.Stack (HasCallStack)

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Xpm.Cdc.Single (XpmCdcSingleConfig(..), xpmCdcSingleWith)

-- | Synchronizes a pulse from the source clock domain to the destination
-- domain. The pulse in the source domain can be of any length, while the pulse
-- in the destination domain will be asserted for a single cycle. For more
-- information see [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_PULSE).
--
-- __N.B.__: In order to simulate initial values, both the source and destination
--           domain need to support them. If the source and destination domain
--           disagree on this property, use of this function will fail to
--           simulate and translate to HDL. You can explicitly set it using
--           'xpmCdcPulseWith'.
xpmCdcPulse ::
  forall a src dst.
  ( KnownDomain src
  , KnownDomain dst
  , HasCallStack
  , Bits a
  , NFDataX a
  , BitPack a
  , BitSize a ~ 1
  ) =>
  Clock src ->
  Clock dst ->
  Signal src a ->
  Signal dst a
xpmCdcPulse = xpmCdcPulseWith XpmCdcPulseConfig{..}
 where
  registerOutput = True
  stages = d4
  initialValues =
    case (initBehavior @src, initBehavior @dst) of
      (SDefined, SDefined) -> True
      (SUnknown, SUnknown) -> False
      _ -> clashCompileError $ "xpmCdcPulse: domains need to agree on initial value "
                            <> "behavior. To set initial value usage explicitly, "
                            <> "consider using 'xpmCdcPulseWith'."
{-# INLINE xpmCdcPulse #-}

-- | Configuration for 'xpmCdcPulseWith'
data XpmCdcPulseConfig stages = XpmCdcPulseConfig
  { -- | Number of synchronization stages. I.e., number of registers in the
    -- destination domain.
    stages :: SNat stages

    -- | Initialize registers used within the primitive to /0/. Note that
    -- 'xpmCdcPulse' will set this to 'True' if both domains support initial
    -- values, to 'False' if neither domain does, and will otherwise emit an
    -- error.
  , initialValues :: Bool

    -- | Register output. Makes sure the combinatorial logic in @XPM_CDC_PULSE@
    -- doesn't contribute to any user critical paths.
  , registerOutput :: Bool
  }

-- | Like 'xpmCdcPulse', but with a configurable number of stages, initial values,
-- and registered input. Also see 'XpmCdcPulseConfig'.
xpmCdcPulseWith ::
  forall stages a src dst.
  ( 2 <= stages, stages <= 10
  , KnownDomain src
  , KnownDomain dst
  , HasCallStack
  , Bits a
  , NFDataX a
  , BitPack a
  , BitSize a ~ 1
  ) =>
  XpmCdcPulseConfig stages ->
  Clock src ->
  Clock dst ->
  Signal src a ->
  Signal dst a
xpmCdcPulseWith XpmCdcPulseConfig{..} clkSrc clkDst input =
  xpmCdcPulse#
    registerOutput initialValues False stages
    clkSrc (error "xpmCdcPulseWith: src: no reset")
    clkDst (error "xpmCdcPulseWith: dst: no reset")
    input
{-# INLINE xpmCdcPulseWith #-}

-- | Clash implementation of @XPM_CDC_PULSE@. It does not need a black box / internal
-- module, as it delegates clock domain crossings to @XPM_CDC_SINGLE@.
xpmCdcPulse# ::
  forall stages a src dst.
  ( 2 <= stages, stages <= 10
  , KnownDomain src
  , KnownDomain dst
  , HasCallStack
  , Bits a
  , NFDataX a
  , BitPack a
  , BitSize a ~ 1
  ) =>
  -- | Register output
  Bool ->
  -- | Initial values supported
  Bool ->
  -- | Reset used
  Bool ->
  -- | Sync stages
  SNat stages ->

  "src_clk"     ::: Clock src ->
  "src_rst"     ::: Reset src ->
  "dest_clk"    ::: Clock dst ->
  "dest_rst_in" ::: Reset dst ->
  "src_pulse"   ::: Signal src a ->
  "dest_pulse"  ::: Signal dst a
xpmCdcPulse# regOutput initVals resetUsed stages clkSrc rstSrc0 clkDst rstDst0 srcPulse
  | regOutput = delay clkDst enableGen initVal go
  | otherwise = go
 where
  initVal
    | initVals  = unpack 0
    | otherwise = errorX "xpmCdcPulse: initial values undefined"

  rstSrc1
    | resetUsed = rstSrc0
    | otherwise = noRst

  rstDst1
    | resetUsed = rstDst0
    | otherwise = noRst

  noRst :: forall dom . KnownDomain dom => Reset dom
  noRst = unsafeFromHighPolarity (pure False)

  go :: Signal dst a
  go = dstPulse
   where
    -- Source domain
    srcInFf = register clkSrc rstSrc1 enableGen initVal srcPulse
    srcEdge = liftA2 (.&.) srcPulse (complement <$> srcInFf)
    srcLevelNext = liftA2 xor srcEdge srcLevelFf
    srcLevelFf = register clkSrc rstSrc1 enableGen initVal srcLevelNext

    -- Crossing
    opts = XpmCdcSingleConfig stages initVals False
    syncedPulse = xpmCdcSingleWith opts clkSrc clkDst srcLevelFf

    -- Destination domain
    syncedPulseFf = register clkDst rstDst1 enableGen initVal syncedPulse
    dstPulse = liftA2 xor syncedPulse syncedPulseFf
{-# NOINLINE xpmCdcPulse# #-}
