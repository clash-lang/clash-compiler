{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Cdc.Gray
  ( xpmCdcGray
  , XpmCdcGrayConfig(..)
  , xpmCdcGrayWith
  ) where

import GHC.Stack (HasCallStack)

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Xpm.Cdc.Gray.Internal (xpmCdcGray#)

-- | Synchronizes an 'Unsigned' from the source clock domain to the destination
-- clock domain using Gray code. It instantiates Xilinx's @XPM_CDC_GRAY@, so no
-- additional constraint definitions are needed. In order to synchronize data
-- from the source domain to the destination domain, consecutive inputs need to
-- be unchanged, successors (+1), or predecessors (-1). Overflows are okay. That is,
-- 'maxBound' can be followed by zero, and vice versa. If this invariant is
-- violated, the component might not transfer data correctly until the invariant
-- is upheld again and the source clock has sampled the input once, followed by
-- /4/ samples in the destination domain.
--
-- If you need all data to be transferred from the source domain to the destination
-- domain, make sure the destination domain samples the input domain at least
-- twice. Alternatively, use a FIFO.
--
-- Read more in [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_GRAY).
--
-- __N.B.__: The simulation model does not detect invariant violations.
--
-- __N.B.__: In order to simulate initial values, both the source and destination
--           domain need to support them. If the source and destination domain
--           disagree on this property, use of this function will fail to
--           simulate and translate to HDL. You can explicitly set initial value
--           usage by using 'xpmCdcGrayWith'.
--
xpmCdcGray ::
  forall n src dst.
  ( 2 <= n, n <= 32
  , KnownNat n
  , HasCallStack
  ) =>
  Clock src ->
  Clock dst ->
  Signal src (Unsigned n) ->
  Signal dst (Unsigned n)
xpmCdcGray clkSrc@ExtractClockDom clkDst@ExtractClockDom = xpmCdcGrayWith XpmCdcGrayConfig{..}  clkSrc clkDst
 where
  stages = d4
  initialValues =
    case (initBehavior @src, initBehavior @dst) of
      (SDefined, SDefined) -> True
      (SUnknown, SUnknown) -> False
      _ -> clashCompileError $ "xpmCdcGray: domains need to agree on initial value "
                            <> "behavior. To set initial value usage explicitly, "
                            <> "consider using 'xpmCdcGrayWith'."
{-# INLINE xpmCdcGray #-}

-- | Configuration for 'xpmCdcGrayWith'
data XpmCdcGrayConfig stages = XpmCdcGrayConfig
  { -- | Number of synchronization stages. I.e., number of registers in the
    -- destination domain. Note that there is always a register in the source
    -- domain.
    stages :: SNat stages

    -- | Initialize registers used within the primitive to /0/. Note that
    -- 'xpmCdcGray' will set this to 'True' if both domains support initial
    -- values, to 'False' if neither domain does, and will otherwise emit an
    -- error.
  , initialValues :: Bool
  }

-- | Like 'xpmCdcGray', but with a configurable number of stages. Also see
-- 'XpmCdcGrayConfig'.
xpmCdcGrayWith ::
  forall stages n src dst.
  ( 2 <= n, n <= 32
  , 2 <= stages, stages <= 10
  , KnownNat n
  , HasCallStack
  ) =>
  XpmCdcGrayConfig stages ->
  Clock src ->
  Clock dst ->
  Signal src (Unsigned n) ->
  Signal dst (Unsigned n)
xpmCdcGrayWith XpmCdcGrayConfig{..} = xpmCdcGray# initialValues stages
{-# INLINE xpmCdcGrayWith #-}
