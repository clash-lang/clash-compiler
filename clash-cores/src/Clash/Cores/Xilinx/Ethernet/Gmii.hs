{- |
Copyright   :  (C) 2024, Google LLC
License     :  BSD2 (see the file LICENSE)
Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

Contains a wrapper for the Xilinx GMII to SGMII PMA

"LogiCORE IP Ethernet 1000BASE-X PCS/PMA or SGMII"

https://www.xilinx.com/xsw/gig_ethernet_pcs_pma

Currently only the Verilog primitive is supported.
-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.Xilinx.Ethernet.Gmii (
  -- * GMII SGMII bridge function
  gmiiSgmiiBridge,
  -- * GMII interface
  Gmii (..),
  -- * GMII SGMII bridge function output
  BridgeOutput (..),
  -- ** Status fields
  Status (..),
  Pause (..),
  -- * PHY LVDS signals
  Lvds (..),
  -- * Bridge configuration
  Config (..),
  AutoNegConfig (..),
  DuplexMode (..),
  LinkSpeed (..)) where

import Clash.Explicit.Prelude

import Clash.Cores.Xilinx.Ethernet.Gmii.Internal
import Clash.Signal.Internal (DiffClock (..))

-- | A subset of the relevant output signals of the GMII to SGMII bridge
data BridgeOutput gmii125 sgmii625 = BridgeOutput
  { bridgeLvdsOut :: Signal sgmii625 Lvds
    -- ^ LVDS output to the PHY
  , bridgeGmiiRx :: Signal gmii125 Gmii
    -- ^ GMII output to the MAC
  , bridgeClk125 :: Clock gmii125
    -- ^ Clock output for the 125 MHz domain
  , bridgeRst125 :: Reset gmii125
    -- ^ Active high reset output for the 125 MHz domain
  , bridgeStatus :: Signal gmii125 Status
    -- ^ Status vector
  }

{- | Wrapper for the LogiCORE IP Ethernet 1000BASE-X PCS/PMA or SGMII, configured to
function as a GMII to SGMII bridge using LVDS in MAC mode. Currently only the Verilog
primitive is supported.
-}
gmiiSgmiiBridge ::
  forall sgmii625 gmii125.
  KnownDomain sgmii625 =>
  KnownDomain gmii125 =>
  DomainPeriod sgmii625 ~ Picoseconds 1600 =>
  DomainPeriod gmii125 ~ Nanoseconds 8 =>
  DomainActiveEdge sgmii625 ~ 'Rising =>
  DomainActiveEdge gmii125 ~ 'Rising =>
  HasAsynchronousReset sgmii625 =>
  HasSynchronousReset gmii125 =>
  DomainResetPolarity sgmii625 ~ 'ActiveHigh =>
  DomainResetPolarity gmii125 ~ 'ActiveHigh =>
  -- | Reference clock coming from the PHY
  DiffClock sgmii625 ->
  -- | Asynchronous reset for the bridge
  Reset sgmii625 ->
  -- | Signal detect from the PHY. Either connect to the PHY's signal detect or use
  -- a constant @True@, otherwise the link will never come up. The IP core considers this
  -- an asynchronous signal, so synchronisation logic is not needed.
  Signal sgmii625 Bool ->
  -- | Configuration for the bridge
  Signal gmii125 Config ->
  -- | Auto negotiation configuration for the bridge
  Signal gmii125 AutoNegConfig ->
  -- | Restart auto negotiation
  Signal gmii125 Bool ->
  -- | LVDS input from the PHY
  Signal sgmii625 Lvds ->
  -- | GMII input from the MAC
  Signal gmii125 Gmii ->
  -- | Output record, see @BridgeOutput@
  BridgeOutput gmii125 sgmii625
gmiiSgmiiBridge refClk refRst signalDetect bridgeConfig anConfig anRestart lvdsIn gmiiTx = BridgeOutput{..}
 where
  ( bridgeClk125
    , activeHighRxRst
    , lvdsOutP
    , lvdsOutN
    , gmiiRxD
    , gmiiRxDv
    , gmiiRxEr
    , fmap fromStatusVector -> bridgeStatus
    ) =
      gmiiSgmiiBridgePrim
        clockP
        clockN
        refRst
        signalDetect
        bridgeConfig
        (toAutoNegConfigVector <$> anConfig)
        anRestart
        lvdsInP
        lvdsInN
        gmiiTxD
        gmiiTxEn
        gmiiTxEr

  bridgeRst125 = unsafeFromActiveHigh activeHighRxRst
  bridgeLvdsOut = Lvds <$> lvdsOutP <*> lvdsOutN
  (DiffClock clockP clockN) = refClk
  lvdsInP = pChannel <$> lvdsIn
  lvdsInN = nChannel <$> lvdsIn
  gmiiTxD = gmiiData <$> gmiiTx
  gmiiTxEn = gmiiValid <$> gmiiTx
  gmiiTxEr = gmiiError <$> gmiiTx
  bridgeGmiiRx = Gmii <$> gmiiRxD <*> gmiiRxDv <*> gmiiRxEr
