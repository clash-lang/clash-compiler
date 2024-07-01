{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

{- |
  Copyright   :  (C) 2024, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

 Contains the types used in the implementation of the GMII to SGMII bridge wrapper
-}
module Clash.Cores.Xilinx.Ethernet.Gmii.Internal where

import Clash.Explicit.Prelude

import Clash.Annotations.Primitive
import Clash.Backend (Backend)
import Clash.Netlist.Types (BlackBoxContext (..), TemplateFunction (..))
import Clash.Signal.Internal
import Control.Monad.State (State)
import Data.List.Infinite ((...))
import Data.String.Interpolate
import Data.Text.Prettyprint.Doc.Extra (Doc)
import Prettyprinter.Interpolate (__di)

import qualified Data.List.Infinite as Inf

-- | A differential pair of Low Voltage Differential Signaling (LVDS) signals
data Lvds = Lvds {pChannel :: Bit, nChannel :: Bit}
  deriving (Generic, NFDataX, BitPack)

-- TODO: Move to re-usable module?

-- | A generic GMII interface
data Gmii = Gmii
  { gmiiData :: BitVector 8
  , gmiiValid :: Bit
  , gmiiError :: Bit
  }
  deriving (Generic, BitPack, NFDataX)

data DuplexMode = HalfDuplex | FullDuplex
  deriving (Generic, NFDataX, BitPack, Eq, Show)

{- | Link speed in megabits per second. The order of the constructors is important,
as it is used to encode the link speed in the an_adv_config_vector.
-}
data LinkSpeed = Speed10 | Speed100 | Speed1000
  deriving (Generic, NFDataX, BitPack, Eq, Show)

{- | Whether the link partner supports pause frames. The order of the constructors
is important, as it is used to encode the link speed in the an_adv_config_vector.
-}
data Pause = NoPause | SymmetricPause | AsymmetricPause | SymmetricAsymmetricPause
  deriving (Generic, NFDataX, BitPack, Eq, Show)

newtype AutoNegConfigVector = AutoNegConfigVector (BitVector 16)
  deriving (Generic, NFDataX, BitPack, Eq, Show)

-- | A subset of the an_adv_config_vector that is applicable for the GMII to SGMII bridge
data AutoNegConfig
  = AutoNegConfig
  { cAcknowledge :: Bool
  , cDuplexMode :: DuplexMode
  , cLinkSpeed :: LinkSpeed
  , cPhyLinkStatus :: Bool
  }
  deriving (Generic, NFDataX, BitPack, Eq, Show)

-- | Converts the AutoNegConfig Records to the an_adv_config_vector format
toAutoNegConfigVector :: AutoNegConfig -> AutoNegConfigVector
toAutoNegConfigVector AutoNegConfig{..} = AutoNegConfigVector $ pack cfg
 where
  -- See LogiCORE IP Ethernet 1000BASE-X PCS/PMA or SGMII v11.3, page 17,
  -- table 7: Optional Auto-Negotiation Interface Signal Pinout, signal
  -- an_adv_config_vector[15:0]
  -- (SGMII operating in PHY mode)
  cfg :: Vec 16 Bit
  cfg =
    ( bitCoerce cPhyLinkStatus
        :> bitCoerce cAcknowledge -- Bit    15 - PHY Link Status
        :> 0 -- Bit    14 - Acknowledge
        :> bitCoerce cDuplexMode -- Bit    13 - Reserved
        :> Nil -- Bit    12 - Duplex Mode
    )
      ++ bitCoerce cLinkSpeed -- Bit 11:10 - Speed
      ++ ( repeat 0
            :< 1 -- Bit  9: 1 - Reserved
            -- Bit     0 - Always 1
         )

-- | A conservative default configuration for the AutoNegConfig record
instance Default AutoNegConfig where
  def = AutoNegConfig{..}
   where
    cAcknowledge = False
    cDuplexMode = HalfDuplex
    cLinkSpeed = Speed10
    cPhyLinkStatus = False

{- | A record representation of the configuration_vector. The order of the fields
should match the order of the bits in the configuration_vector.
-}
data Config = Config
  { cAutoNegEnable :: Bool
  , cIsolateGmii :: Bool
  , cPowerDown :: Bool
  , cLoopback :: Bool
  , cUnidirectional :: Bool
  }
  deriving (Generic, NFDataX, BitPack)

instance Default Config where
  def = Config{..}
   where
    cUnidirectional = False
    cLoopback = False
    cPowerDown = False
    cIsolateGmii = False
    cAutoNegEnable = False

type StatusVector = BitVector 16

-- | A record representation of the status_vector
data Status = Status
  { sLinkStatus :: Bool
  , sLinkSynchronized :: Bool
  , sRudiC :: Bool
  , sRudiI :: Bool
  , sRudiInvalid :: Bool
  , sReceivedDisparityError :: Bool
  , sReceivedInvalidCode :: Bool
  , sPhyLinkStatus :: Bool
  , sRemoteFault :: Maybe (BitVector 2)
  , sLinkSpeed :: LinkSpeed
  , sDuplexMode :: DuplexMode
  , sPause :: Pause
  }
  deriving (Generic, NFDataX, BitPack)

-- | Transform the status_vector to a Status record
fromStatusVector :: StatusVector -> Status
fromStatusVector v = Status{..}
 where
  -- There is no BitPack instance for 13-tuples without -flarge-tuples, so we break it up.
  ( ( sLinkStatus
      , sLinkSynchronized
      , sRudiC
      , sRudiI
      , sRudiInvalid
      , sReceivedDisparityError
      )
    , sReceivedInvalidCode
    , sPhyLinkStatus
    , remoteFaultCode
    , sLinkSpeed
    , sDuplexMode
    , remoteFaultValid
    , sPause
    ) = unpack v
  sRemoteFault = if remoteFaultValid then Just remoteFaultCode else Nothing

-- | Primitive for the GMII to SGMII bridge
gmiiSgmiiBridgePrim ::
  forall sgmii625 gmii125.
  ( KnownDomain sgmii625
  , KnownDomain gmii125
  , DomainPeriod sgmii625 ~ Picoseconds 1600
  , DomainPeriod gmii125 ~ Nanoseconds 8
  , DomainActiveEdge sgmii625 ~ 'Rising
  , DomainActiveEdge gmii125 ~ 'Rising
  , HasAsynchronousReset sgmii625
  , HasSynchronousReset gmii125
  , DomainResetPolarity sgmii625 ~ 'ActiveHigh
  , DomainResetPolarity gmii125 ~ 'ActiveHigh
  ) =>
  -- | P channel of the reference clock coming from the PHY
  Clock sgmii625 ->
  -- | N channel of the reference clock coming from the PHY
  ClockN sgmii625 ->
  -- | Signal detect from the PHY. Either connect to the PHY's signal detect or use
  -- a constant @True@, otherwise the link will never come up. The IP core considers this
  -- an asynchronous signal, so synchronisation logic is not needed.
  Reset sgmii625 ->
  -- | Signal detect from the PHY. Either connect to the PHY's signal detect or use
  -- a constant @True@, otherwise the link will never come up.
  Signal sgmii625 Bool ->
  -- | Configuration for the bridge
  Signal gmii125 Config ->
  -- | Auto negotiation configuration for the bridge
  Signal gmii125 AutoNegConfigVector ->
  -- | Restart auto negotiation
  Signal gmii125 Bool ->
  -- | P channel of the LVDS input from the PHY
  Signal sgmii625 Bit ->
  -- | N channel of the LVDS input from the PHY
  Signal sgmii625 Bit ->
  -- | GMII data input from the MAC
  Signal gmii125 (BitVector 8) ->
  -- | GMII valid input from the MAC
  Signal gmii125 Bit ->
  -- | GMII error input from the MAC
  Signal gmii125 Bit ->
  -- |
  -- 1. P channel of the LVDS output to the PHY
  -- 2. N channel of the LVDS output to the PHY
  -- 3. GMII data output to the MAC
  -- 4. GMII valid output to the MAC
  -- 5. GMII error output to the MAC
  -- 6. Clock output for the 125 MHz domain
  -- 7. Active high reset output for the 125 MHz domain
  -- 8. Status vector
  ( Clock gmii125
  , Signal gmii125 Bool
  , Signal sgmii625 Bit
  , Signal sgmii625 Bit
  , Signal gmii125 (BitVector 8)
  , Signal gmii125 Bit
  , Signal gmii125 Bit
  , Signal gmii125 StatusVector
  )
-- Can't use @deepErrorX@ because one of the results is a clock
gmiiSgmiiBridgePrim !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ =
  (clockGen, undefined, undefined, undefined, undefined, undefined, undefined, undefined)
{-# CLASH_OPAQUE gmiiSgmiiBridgePrim #-}
{-# ANN
  gmiiSgmiiBridgePrim
  ( let
      tfName = 'tclTemplateFunction
      primName = 'gmiiSgmiiBridgePrim
      -- Constraints
      _knownDomSgmii
        Inf.:< _knownDomGmii
        Inf.:< _domPeriodsgmii
        Inf.:< _domPeriodgmii1
        Inf.:< _domEdgesgmii
        Inf.:< _domEdgegmii
        Inf.:< _domRstKindasync
        Inf.:< _domRstKindgmii
        Inf.:< _domRstPolasync
        Inf.:< _domRstPolgmii
        Inf.:<
        -- Arguments
        smiiClkP
        Inf.:< smiiClkN
        Inf.:< rst
        Inf.:< signalDetect
        Inf.:< pmaConfig
        Inf.:< pmaAnAdvancedConfig
        Inf.:< restartAn
        Inf.:< lvdsInP
        Inf.:< lvdsInN
        Inf.:< gmiiTxD
        Inf.:< gmiiTxDv
        Inf.:< gmiiTxEr
        Inf.:< _ =
          ((0 :: Int) ...)

      -- Symbols
      clk125
        Inf.:< rst125
        Inf.:< lvdsOutP
        Inf.:< lvdsOutN
        Inf.:< gmiiRxD
        Inf.:< gmiiRxDv
        Inf.:< gmiiRxEr
        Inf.:< statusVector
        Inf.:< compName
        Inf.:< _ =
          ((0 :: Int) ...)
     in
      InlineYamlPrimitive
        [Verilog]
        [__i|
      BlackBox:
        kind: Declaration
        name: #{primName}
        template: |-
          wire ~GENSYM[pmaClk125][#{clk125}];
          wire ~GENSYM[pmaRst125][#{rst125}];
          wire ~GENSYM[lvdsOutP][#{lvdsOutP}];
          wire ~GENSYM[lvdsOutN][#{lvdsOutN}];
          wire [7:0] ~GENSYM[gmiiRxD][#{gmiiRxD}];
          wire ~GENSYM[gmiiRxDv][#{gmiiRxDv}];
          wire ~GENSYM[gmiiRxEr][#{gmiiRxEr}];
          wire [15:0] ~GENSYM[pmaStatusVector][#{statusVector}];

          ~INCLUDENAME[0] ~GENSYM[gig_ethernet_pcs_pma][#{compName}] (
            .refclk625_p(~ARG[#{smiiClkP}]),
            .refclk625_n(~ARG[#{smiiClkN}]),
            .reset(~ARG[#{rst}]),
            .speed_is_100(0),
            .speed_is_10_100(0),
            .signal_detect(~ARG[#{signalDetect}]),
            .configuration_vector(~ARG[#{pmaConfig}]),
            .an_adv_config_vector(~ARG[#{pmaAnAdvancedConfig}]),
            .an_restart_config(~ARG[#{restartAn}]),
            .rxp(~ARG[#{lvdsInP}]),
            .rxn(~ARG[#{lvdsInN}]),
            .gmii_txd(~ARG[#{gmiiTxD}]),
            .gmii_tx_en(~ARG[#{gmiiTxDv}]),
            .gmii_tx_er(~ARG[#{gmiiTxEr}]),
            .clk125_out(~SYM[#{clk125}]),
            .rst_125_out(~SYM[#{rst125}]),
            .txp(~SYM[#{lvdsOutP}]),
            .txn(~SYM[#{lvdsOutN}]),
            .gmii_rxd(~SYM[#{gmiiRxD}]),
            .gmii_rx_dv(~SYM[#{gmiiRxDv}]),
            .gmii_rx_er(~SYM[#{gmiiRxEr}]),
            .status_vector(~SYM[#{statusVector}])
          );
          assign ~RESULT =
            { ~SYM[#{clk125}]
            , ~SYM[#{rst125}]
            , ~SYM[#{lvdsOutP}]
            , ~SYM[#{lvdsOutN}]
            , ~SYM[#{gmiiRxD}]
            , ~SYM[#{gmiiRxDv}]
            , ~SYM[#{gmiiRxEr}]
            , ~SYM[#{statusVector}]
            };
        includes:
          - extension: clash.tcl
            name: gig_ethernet_pcs_pma
            format: Haskell
            templateFunction: #{tfName}

          |]
  )
  #-}

tclTemplateFunction :: TemplateFunction
tclTemplateFunction = TemplateFunction used valid tclTemplate
 where
  used = [10 .. 21] -- use all arguments
  valid = const True

tclTemplate :: (Backend s) => BlackBoxContext -> State s Doc
tclTemplate bbCtx
  | [compName] <- bbQsysIncName bbCtx =
      let
        bbText =
          [__di|
      namespace eval $tclIface {
        variable api 1
        variable scriptPurpose createIp
        variable ipName {#{compName}}
        proc createIp {ipName0 args} {
          create_ip -name gig_ethernet_pcs_pma -vendor xilinx.com -library ip \
            -version 16.2 -module_name $ipName0
          set_property -dict [list \
            CONFIG.LvdsRefClk {625} \
            CONFIG.Standard {SGMII} \
            CONFIG.Physical_Interface {LVDS} \
            CONFIG.Management_Interface {false} \
            CONFIG.Ext_Management_Interface {false} \
            CONFIG.SGMII_PHY_Mode {false} \
            CONFIG.SupportLevel {Include_Shared_Logic_in_Core}] [get_ips $ipName0]
        return
          }
        }
      |]
       in
        pure bbText
  | otherwise = error "gmiiSgmiiBridgePrim: Expected exactly one component name"
