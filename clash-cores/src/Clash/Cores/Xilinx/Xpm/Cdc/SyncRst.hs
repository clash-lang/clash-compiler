{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Cdc.SyncRst
  ( xpmCdcSyncRst
  , XpmCdcSyncRstConfig(..)
  , xpmCdcSyncRstWith
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal((:-)))

import Data.Maybe (fromMaybe, isJust)
import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal

-- | TODO
-- registers. Use 'xpmCdcSyncRstWith' to change these settings. For more
-- information see [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_SYNC_RST).
xpmCdcSyncRst ::
  forall src dst.
  ( KnownDomain src
  , KnownDomain dst
  , HasCallStack
  ) =>
  Clock src ->
  Clock dst ->
  Reset src ->
  Reset dst
xpmCdcSyncRst = xpmCdcSyncRstWith XpmCdcSyncRstConfig{..}
 where
  stages = d4
  initialValues =
    case initBehavior @dst of
      SDefined -> Just True
      SUnknown -> Nothing
{-# INLINE xpmCdcSyncRst #-}

-- | Configuration for 'xpmCdcSyncRstWith'
data XpmCdcSyncRstConfig stages = XpmCdcSyncRstConfig
  { -- | Number of synchronization stages, i.e., number of registers in the
    -- destination domain.
    stages :: SNat stages

   -- TODO
    -- | Whether to initialize registers used within the primitive, and if they should. Note that
    -- 'xpmCdcSyncRst' will set this to @Just True@ if both domains support initial
    -- values, to @Nothing@ if neither domain does, and will otherwise emit an
    -- error.
  , initialValues :: Maybe Bool

  }

-- | Like 'xpmCdcSyncRst', but with a configurable number of stages and initial values.
-- Also see 'XpmCdcSyncRstConfig'.
xpmCdcSyncRstWith ::
  forall stages src dst.
  ( 2 <= stages, stages <= 10
  , KnownDomain src
  , KnownDomain dst
  ) =>
  XpmCdcSyncRstConfig stages ->
  Clock src ->
  Clock dst ->
  Reset src ->
  Reset dst
xpmCdcSyncRstWith XpmCdcSyncRstConfig{stages=stages@SNat, ..} clkSrc clkDst srcIn
  | clashSimulation = sim
  | otherwise = synth
 where
  -- Definition used in for HDL generation
  synth = unPort go
   where
    go :: ResetPort "dest_rst" ActiveHigh dst
    go =
      inst
        (instConfig "xpm_cdc_sync_rst")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"DEST_SYNC_FF"   @Integer (natToNum @stages))
        (Param @"INIT"           @Integer (if fromMaybe True initialValues then 1 else 0))

        (Param @"INIT_SYNC_FF"   @Integer (if isJust initialValues then 1 else 0))
        (Param @"SIM_ASSERT_CHK" @Integer 0)

        (ClockPort @"dest_clk" clkDst)
        (ResetPort @"src_rst" @ActiveHigh srcIn)

  -- Definition used in Clash simulation
  sim = unsafeFromActiveHigh $ go (snatToNum stages) $ unsafeToActiveHigh srcIn
   where
    initVal = case initialValues of
      Just x -> x
      Nothing -> deepErrorX "xpmCdcSyncRst: initial values undefined"

    go :: Word -> Signal src Bool -> Signal dst Bool
    go 0 src = unsafeSynchronizer clkSrc clkDst src
    go n src = initVal :- go (n - 1) src
{-# INLINE xpmCdcSyncRstWith #-}
