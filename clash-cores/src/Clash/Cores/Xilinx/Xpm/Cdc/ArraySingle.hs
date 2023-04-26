{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Cdc.ArraySingle
  ( xpmCdcArraySingle
  , XpmCdcArraySingleConfig(..)
  , xpmCdcArraySingleWith
  ) where

import GHC.Stack (HasCallStack)

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Xpm.Cdc.ArraySingle.Internal (xpmCdcArraySingle#)

-- | Synchronizes an array of independent bits from the source clock domain to
-- the destination. Be careful when using this primitive: each bit will be
-- synchronized independently. For more information see [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_ARRAY_SINGLE).
--
-- __N.B.__: In order to simulate initial values, both the source and destination
--           domain need to support them. If the source and destination domain
--           disagree on this property, use of this function will fail to
--           simulate and translate to HDL. You can explicitly set it using
--           'xpmCdcArraySingleWith'.
xpmCdcArraySingle ::
  forall n src dst.
  ( 1 <= n, n <= 1024
  , KnownDomain src
  , KnownDomain dst
  , KnownNat n
  , HasCallStack
  ) =>
  Clock src ->
  Clock dst ->
  Signal src (BitVector n) ->
  Signal dst (BitVector n)
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
  { -- | Number of synchronization stages. I.e., number of registers in the
    -- destination domain. Note that there is always a register in the source
    -- domain.
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

-- | Like 'xpmCdcArraySingle', but with a configurable number of stages, initial values,
-- and registered input. Also see 'XpmCdcArraySingleConfig'.
xpmCdcArraySingleWith ::
  forall stages n src dst.
  ( 2 <= stages, stages <= 10
  , 1 <= n, n <= 1024
  , KnownDomain src
  , KnownDomain dst
  , KnownNat n
  ) =>
  XpmCdcArraySingleConfig stages ->
  Clock src ->
  Clock dst ->
  Signal src (BitVector n) ->
  Signal dst (BitVector n)
xpmCdcArraySingleWith XpmCdcArraySingleConfig{..} clkSrc clkDst input =
  xpmCdcArraySingle# registerInput initialValues stages clkSrc clkDst input
{-# INLINE xpmCdcArraySingleWith #-}
