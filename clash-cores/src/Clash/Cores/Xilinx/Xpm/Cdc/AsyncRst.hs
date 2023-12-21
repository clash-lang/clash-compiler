{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Cdc.AsyncRst
  ( xpmCdcAsyncRst
  , XpmCdcAsyncRstConfig(..)
  , xpmCdcAsyncRstWith
  , asyncRstClash
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal((:-)), asyncRegister#)

import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal

-- | TODO
-- registers. Use 'xpmCdcAsyncRstWith' to change these settings. For more
-- information see [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_SYNC_RST).
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
    case initBehavior @dst of
      SDefined -> True
      SUnknown -> False
{-# INLINE xpmCdcAsyncRst #-}

-- | Configuration for 'xpmCdcAsyncRstWith'
data XpmCdcAsyncRstConfig stages = XpmCdcAsyncRstConfig
  { -- | Number of synchronization stages, i.e., number of registers in the
    -- destination domain.
    stages :: SNat stages

   -- TODO
    -- | Whether to initialize registers used within the primitive, and if they should. Note that
    -- 'xpmCdcAsyncRst' will set this to @Just True@ if both domains support initial
    -- values, to @Nothing@ if neither domain does, and will otherwise emit an
    -- error.
  , initialValues :: Bool

  }

-- | Like 'xpmCdcAsyncRst', but with a configurable number of stages and initial values.
-- Also see 'XpmCdcAsyncRstConfig'.
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
xpmCdcAsyncRstWith XpmCdcAsyncRstConfig{stages=stages@SNat, ..} clkSrc clkDst srcRst
  -- | clashSimulation = sim
  | clashSimulation = asyncRstClash stages initialValues clkSrc clkDst srcRst
  | otherwise = synth
 where
  -- Definition used in for HDL generation
  synth = unPort go
   where
    go :: ResetPort "dest_arst" ActiveHigh dst
    go =
      inst
        (instConfig "xpm_cdc_async_rst")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"DEST_SYNC_FF"   @Integer (natToNum @stages))

        (Param @"INIT_SYNC_FF"   @Integer (if initialValues then 1 else 0))
        (Param @"RST_ACTIVE_HIGH" @Integer 1)

        (ClockPort @"dest_clk" clkDst)
        (ResetPort @"src_arst" @ActiveHigh srcRst)

  -- Definition used in Clash simulation
  -- sim = unsafeFromActiveHigh $ go (snatToNum stages)
  --  where
  --   initVal
  --     | initialValues = True
  --     | otherwise = deepErrorX "xpmCdcAsyncRst: initial values undefined"
  --
  --   dstRst0 = unsafeFromActiveHigh $ unsafeSynchronizer clkSrc clkDst $ unsafeToActiveHigh srcRst
  --
  --   go :: Word -> Signal dst Bool
  --   go 0 = pure False
  --   go n = asyncRegister# clkDst dstRst0 enableGen initVal True (go (n - 1))
{-# INLINE xpmCdcAsyncRstWith #-}

asyncRstClash ::
  forall stages src dst.
  ( 2 <= stages, stages <= 10
  , KnownDomain src
  , KnownDomain dst
  ) =>
  SNat stages ->
  Bool ->
  Clock src ->
  Clock dst ->
  Reset src ->
  Reset dst
asyncRstClash stages initialValues clkSrc clkDst srcRst = sim
 where
  sim = unsafeFromActiveHigh $ go (snatToNum stages)
   where
    initVal
      | initialValues = True
      | otherwise = deepErrorX "xpmCdcAsyncRst: initial values undefined"

    dstRst0 = unsafeFromActiveHigh $ unsafeSynchronizer clkSrc clkDst $ unsafeToActiveHigh srcRst

    go :: Word -> Signal dst Bool
    go 0 = pure False
    go n = asyncRegister# clkDst dstRst0 enableGen initVal True (go (n - 1))
