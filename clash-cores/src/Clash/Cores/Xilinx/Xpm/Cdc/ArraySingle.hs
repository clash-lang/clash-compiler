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

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.ArraySingle.Internal (xpmCdcArraySingle#)

-- | Synchronizes an array of independent bits from the source clock domain to
-- the destination. Be careful when using this primitive: each bit will be
-- synchronized independently. By default, it registers its input, uses four
-- synchronization stages, and auto-detects whether to use initial values for
-- the synchronization registers. Use 'xpmCdcArraySingleWith' to change these
-- settings. For more information see [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_ARRAY_SINGLE).
--
-- __N.B.__: In order to simulate initial values, both the source and destination
--           domain need to support them. If the source and destination domain
--           disagree on this property, use of this function will fail to
--           simulate and translate to HDL. You can explicitly set it using
--           'xpmCdcArraySingleWith'.
xpmCdcArraySingle ::
  forall a src dst.
  ( 1 <= BitSize a, BitSize a <= 1024
  , HasCallStack
  , NFDataX a
  , BitPack a
  ) =>
  Clock src ->
  Clock dst ->
  Signal src a ->
  Signal dst a
xpmCdcArraySingle clkSrc@ExtractClockDom clkDst@ExtractClockDom = xpmCdcArraySingleWith XpmCdcArraySingleConfig{..} clkSrc clkDst
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
  , HasCallStack
  , NFDataX a
  , BitPack a
  ) =>
  XpmCdcArraySingleConfig stages ->
  Clock src ->
  Clock dst ->
  Signal src a ->
  Signal dst a
xpmCdcArraySingleWith XpmCdcArraySingleConfig{..} =
  xpmCdcArraySingle# registerInput initialValues stages
{-# INLINE xpmCdcArraySingleWith #-}
