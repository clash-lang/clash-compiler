{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Cdc.AsyncRst
  ( xpmCdcAsyncRst
  , XpmCdcAsyncRstConfig(..)
  , xpmCdcAsyncRstWith
  ) where

import GHC.Stack (HasCallStack)

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Xpm.Cdc.AsyncRst.Internal (xpmCdcAsyncRst#)

-- | This macro synchronizes an asynchronous reset signal to the destination clock
-- domain. The resulting reset output will be guaranteed to assert asynchronously
-- in relation to the input but the deassertion of the output will always be
-- synchronous to the destination clock domain.
--
-- __N.B.__: In order to simulate initial values, both the source and destination
--           domain need to support them. If the source and destination domain
--           disagree on this property, use of this function will fail to
--           simulate and translate to HDL. You can explicitly set it using
--           'xpmCdcAsyncRstWith'.
xpmCdcAsyncRst ::
  forall src dst.
  ( KnownDomain src
  , KnownDomain dst
  , HasCallStack
  ) =>
  Clock src ->
  Clock dst ->
  Reset src ->
  Reset dst
xpmCdcAsyncRst = xpmCdcAsyncRstWith XpmCdcAsyncRstConfig{..}
 where
  stages = d4
  initialValues =
    case (initBehavior @src, initBehavior @dst) of
      (SDefined, SDefined) -> True
      (SUnknown, SUnknown) -> False
      _ -> clashCompileError $ "xpmCdcAsyncRst: domains need to agree on initial value "
                            <> "behavior. To set initial value usage explicitly, "
                            <> "consider using 'xpmCdcAsyncRstWith'."
{-# INLINE xpmCdcAsyncRst #-}

-- | Configuration for 'xpmCdcAsyncRstWith'
data XpmCdcAsyncRstConfig stages = XpmCdcAsyncRstConfig
  { -- | Number of synchronization stages. I.e., number of registers in the
    -- destination domain. Note that there is always a register in the source
    -- domain.
    stages :: SNat stages

    -- | Initialize registers used within the primitive to /0/. Note that
    -- 'xpmCdcAsyncRst' will set this to 'True' if both domains support initial
    -- values, to 'False' if neither domain does, and will otherwise emit an
    -- error.
  , initialValues :: Bool
  }

-- | Like 'xpmCdcAsyncRst', but with a configurable number of stages, initial values,
-- and registered input. Also see 'XpmCdcAsyncRstConfig'.
xpmCdcAsyncRstWith ::
  forall stages src dst.
  ( 2 <= stages, stages <= 10
  , KnownDomain src
  , KnownDomain dst
  ) =>
  XpmCdcAsyncRstConfig stages ->
  Clock src ->
  Clock dst ->
  Reset src ->
  Reset dst
xpmCdcAsyncRstWith XpmCdcAsyncRstConfig{..} clkSrc clkDst input =
  xpmCdcAsyncRst# initialValues stages clkSrc clkDst input
{-# INLINE xpmCdcAsyncRstWith #-}
