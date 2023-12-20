{-|
  Copyright   :  (C) 2023, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Xilinx.Xpm.Cdc.SyncRst
  ( xpmCdcSyncRst
  , XpmCdcSyncRstConfig(..)
  , Asserted(..)
  , xpmCdcSyncRstWith
  ) where

import Clash.Explicit.Prelude
import Clash.Signal.Internal (Signal((:-)))

import Data.Maybe (fromMaybe, isJust)
import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.Xpm.Cdc.Internal

-- | Used to specify initial values for 'xpmCdcSyncRst' and 'xpmCdcSyncRstWith'.
data Asserted = Asserted | Deasserted
  deriving (Show, Eq)

-- | This macro synchronizes a reset signal to the destination clock domain. The
-- generated output will both assert and deassert synchronously to the
-- destination clock domain. For proper operation, the input data must be sampled
-- two or more times by the destination clock. You can define the number of
-- register stages used in the synchronizers and the initial value of these
-- registers after configuration. Use 'xpmCdcSyncRstWith' to change these
-- settings. For more information see
-- [PG382](https://docs.xilinx.com/r/en-US/pg382-xpm-cdc-generator/XPM_CDC_SYNC_RST).
xpmCdcSyncRst ::
  forall src dst.
  ( KnownDomain src
  , KnownDomain dst
  , HasCallStack
  ) =>
  -- | If destination domain supports initial values, this will be used as the
  -- initial value for the synchronization registers.
  Asserted ->
  Clock src ->
  Clock dst ->
  Reset src ->
  Reset dst
xpmCdcSyncRst asserted = xpmCdcSyncRstWith XpmCdcSyncRstConfig{..}
 where
  stages = d4
  initialValues =
    case initBehavior @dst of
      SDefined -> Just asserted
      SUnknown -> Nothing
{-# INLINE xpmCdcSyncRst #-}

-- | Configuration for 'xpmCdcSyncRstWith'
data XpmCdcSyncRstConfig stages = XpmCdcSyncRstConfig
  { -- | Number of synchronization stages, i.e., number of registers in the
    -- destination domain.
    stages :: SNat stages

    -- | Whether to initialize registers used within the primitive. Note that
    -- 'xpmCdcSyncRst' will set this to @Just True@ if both domains support initial
    -- values, to @Nothing@ if neither domain does, and will otherwise emit an
    -- error.
  , initialValues :: Maybe Asserted
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
    go :: ResetPort "dest_rst" 'ActiveHigh dst
    go =
      inst
        (instConfig "xpm_cdc_sync_rst")
          { library = Just "xpm"
          , libraryImport = Just "xpm.vcomponents.all" }

        (Param @"DEST_SYNC_FF"   @Integer (natToNum @stages))
        (Param @"INIT"           @Integer (case fromMaybe Asserted initialValues of
                                             Asserted -> 1
                                             Deasserted -> 0))

        (Param @"INIT_SYNC_FF"   @Integer (if isJust initialValues then 1 else 0))
        (Param @"SIM_ASSERT_CHK" @Integer 0)

        (ClockPort @"dest_clk" clkDst)
        (ResetPort @"src_rst" @'ActiveHigh srcIn)

  -- Definition used in Clash simulation
  sim = unsafeFromActiveHigh $ go (snatToNum stages) $ unsafeToActiveHigh srcIn
   where
    initVal = case initialValues of
      Just Asserted -> True
      Just Deasserted -> False
      Nothing -> deepErrorX "xpmCdcSyncRst: initial values undefined"

    go :: Word -> Signal src Bool -> Signal dst Bool
    go 0 src = unsafeSynchronizer clkSrc clkDst src
    go n src = initVal :- go (n - 1) src
{-# INLINE xpmCdcSyncRstWith #-}
