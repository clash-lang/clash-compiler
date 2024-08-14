{-# LANGUAGE CPP #-}

-- |
--   Copyright   :  (C) 2024, QBayLogic B.V.
--   License     :  BSD2 (see the file LICENSE)
--   Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
--
--   Top-level SGMII module that combines all the blocks that are defined in the
--   sub-modules to one function that can be used in different projects.
--
--   Example usage:
--
-- @
-- topEntity ::
--   Clock Dom0 ->
--   Clock Dom1 ->
--   Reset Dom0 ->
--   Reset Dom1 ->
--   Signal Dom1 Bool ->
--   Signal Dom1 Bool ->
--   Signal Dom1 (BitVector 8) ->
--   Signal Dom0 (BitVector 10) ->
--   ( Signal rxDom SgmiiStatus
--   , Signal rxDom Bool
--   , Signal rxDom Bool
--   , Signal rxDom (BitVector 8)
--   , Signal txDom (BitVector 10)
--   )
-- topEntity = sgmii rxTxCdc
-- @
--   Here, the type of @rxTxCdc@, which is the function that handles the
--   clock domain crossing between the transmit and receive domain between the
--   auto-negotiation block and transmission block, needs to be the following:
--
-- @
-- rxTxCdc ::
--   forall dom0 dom1.
--   (KnownDomain dom0, KnownDomain dom1) =>
--   Clock rxDom ->
--   Clock txDom ->
--   Signal rxDom (Maybe Xmit) ->
--   Signal rxDom (Maybe ConfReg) ->
--   Signal rxDom (Maybe ConfReg) ->
--   ( Signal txDom (Maybe Xmit)
--   , Signal txDom (Maybe ConfReg)
--   , Signal txDom (Maybe ConfReg)
--   )
-- @
--
--   For Xilinx boards, this could be implemented by using, for example, the
--   function 'Clash.Cores.Xilinx.Xpm.Cdc.Handshake.xpmCdcHandshake', but
--   vendor-neutral implementations could make use of other word-synchronizers.
--
--   As the decoding of incoming 10-bit code groups is done on a best-effort
--   basis and they are always transmitted to @TXD@, this port should only be
--   read when @RX_DV@ is asserted as invalid data might be provided when it is
--   not.
module Clash.Cores.Sgmii
  ( sgmii
  , sgmiiRA
  , sgmiiRx
  , sgmiiTx
  )
where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.AutoNeg
import Clash.Cores.Sgmii.BitSlip
import Clash.Cores.Sgmii.Common
import Clash.Cores.Sgmii.PcsReceive
import Clash.Cores.Sgmii.PcsTransmit
import Clash.Cores.Sgmii.RateAdapt
import Clash.Cores.Sgmii.Sync
import Clash.Prelude
import Data.Maybe (fromMaybe, isJust)

-- | Receive side of the SGMII block, that combines all the functions that are
--   in the receive domain
sgmiiRx ::
  (HiddenClockResetEnable dom) =>
  -- | Input code group
  Signal dom CodeGroup ->
  -- | Output tuple
  ( Signal dom SgmiiStatus
  , Signal dom Bool
  , Signal dom Bool
  , Signal dom (BitVector 8)
  , Signal dom (Maybe Xmit)
  , Signal dom (Maybe ConfReg)
  , Signal dom (Maybe ConfReg)
  , Signal dom CodeGroup
  )
sgmiiRx rxCg =
  ( rxStatus
  , regMaybe False rxDv
  , regMaybe False rxEr
  , regMaybe 0 ((fmap . fmap) fromSymbol rxDw)
  , xmit
  , txConfReg
  , rxConfReg
  , bsCg
  )
 where
  rxStatus =
    SgmiiStatus
      <$> bsStatus
      <*> syncStatus
      <*> (toLinkSpeed <$> regMaybe 0 rxConfReg)
      <*> regMaybe Conf xmit
      <*> regMaybe Invalid rudi

  rxConfReg = toConfReg <$> regMaybe (C 0) rudi
  (xmit, txConfReg) = autoNeg syncStatus rudi
  (rxDv, rxEr, rxDw, rudi) = pcsReceive cg rd dw rxEven syncStatus xmit
  (cg, rd, dw, rxEven, syncStatus) = sync bsCg
  (bsCg, bsStatus) = bitSlip rxCg syncStatus

-- | Transmit side of the SGMII block, that combines all the functions that are
--   in the transmit domain
sgmiiTx ::
  (HiddenClockResetEnable dom) =>
  -- | @TX_EN@ signal
  Signal dom Bool ->
  -- | @TX_ER@ signal
  Signal dom Bool ->
  -- | Input data word
  Signal dom (BitVector 8) ->
  -- | 'Xmit' signal
  Signal dom (Maybe Xmit) ->
  -- | Configuration register from MAC
  Signal dom (Maybe ConfReg) ->
  -- | Configuration register from PHY
  Signal dom (Maybe ConfReg) ->
  -- | Output code group
  Signal dom CodeGroup
sgmiiTx txEn txEr txDw xmit txConfReg _ =
  pcsTransmit txEn txEr txDw xmit txConfReg

-- | Top-level SGMII function that takes as its second argument a function that
--   implements a clock domain crossing between the auto-negotiation and
--   transmission blocks. This block does not implement the serialization and
--   deserialization of the 10-bit code groups, but leaves this to be
--   implemented externally. These 10-bit code groups do not have to be word
--   aligned as this is implemented internally in the 'bitSlip' block.
--
--   This function implements SGMII as described
--   [here](https://archive.org/download/sgmii/SGMII.pdf), without the optional
--   EEE (Energy-Efficient Ethernet) capability.
--
--   This function reports its internal status by using 'SgmiiStatus', this
--   reports the synchronization status of the line in the first bit, the
--   configuration register in the following 16 bits, and the values of 'Rudi'
--   and 'Xmit' in the following 2-bit groups.
sgmii ::
  forall rxDom txDom.
  (KnownDomain rxDom, KnownDomain txDom) =>
  -- | Function used for the clock domain crossing between 'autoNeg' and
  --   'pcsTransmit', for the values of 'Xmit' and 'ConfReg'
  ( Clock rxDom ->
    Clock txDom ->
    Signal rxDom (Maybe Xmit) ->
    Signal rxDom (Maybe ConfReg) ->
    Signal rxDom (Maybe ConfReg) ->
    ( Signal txDom (Maybe Xmit)
    , Signal txDom (Maybe ConfReg)
    , Signal txDom (Maybe ConfReg)
    )
  ) ->
  -- | Clock of the receive domain, which is a 125 MHz clock that is derived
  --   from the 625 MHz clock that is received from the PHY
  Clock rxDom ->
  -- | Clock of the transmit domain, which can be an internal clock of the FPGA
  Clock txDom ->
  -- | Reset of the receive domain
  Reset rxDom ->
  -- | Reset of the transmit domain
  Reset txDom ->
  -- | Input signal @TX_EN@, which enables data transmission when 'Xmit' from
  --   'autoNeg' is set to 'Data'
  Signal txDom Bool ->
  -- | Input signal @TX_ER@, which is used to propagate error values to the PHY
  Signal txDom Bool ->
  -- | Data octet @TXD@ to be transmitted to the PHY
  Signal txDom (BitVector 8) ->
  -- | Input code group from the PHY
  Signal rxDom CodeGroup ->
  -- | Tuple that contains the output signals from the SGMII block which are the
  --   current status of the receive block 'SgmiiStatus', the @RX_DV@ signal
  --   that indicates an incoming data packet, @RX_ER@ which indicates a receive
  --   error, @RXD@ which is the incoming data octet from the PHY, and a 10-bit
  --   code word that can be serialized and transmitted to the PHY.
  ( Signal rxDom SgmiiStatus
  , Signal rxDom Bool
  , Signal rxDom Bool
  , Signal rxDom (BitVector 8)
  , Signal rxDom CodeGroup
  , Signal txDom CodeGroup
  )
sgmii rxTxCdc rxClk txClk rxRst txRst txEn txEr txDw rxCg =
  (rxStatus, rxDv, rxEr, rxDw, bsCg, txCg)
 where
  txCg = sgmiiTx' txEn txEr txDw xmit2 txConfReg2 rxConfReg2
   where
    sgmiiTx' = exposeClockResetEnable sgmiiTx txClk txRst enableGen

  (xmit2, txConfReg2, rxConfReg2) =
    rxTxCdc rxClk txClk xmit1 txConfReg1 rxConfReg1

  (rxStatus, rxDv, rxEr, rxDw, xmit1, txConfReg1, rxConfReg1, bsCg) =
    sgmiiRx' rxCg
   where
    sgmiiRx' = exposeClockResetEnable sgmiiRx rxClk rxRst enableGen

{-# CLASH_OPAQUE sgmii #-}

-- | Receive side of the SGMII block, that combines all the functions that are
--   in the receive domain, rate-adapted
sgmiiRxRA ::
  (HiddenClockResetEnable dom) =>
  -- | Input code group
  Signal dom CodeGroup ->
  -- | Output tuple
  ( Signal dom SgmiiStatus
  , Signal dom Bool
  , Signal dom (Maybe (BitVector 8))
  , Signal dom (Maybe Xmit)
  , Signal dom (Maybe ConfReg)
  , Signal dom (Maybe ConfReg)
  , Signal dom CodeGroup
  )
sgmiiRxRA rxCg = (rxStatus, rxEr, out, xmit, txConfReg, rxConfReg, bsCg)
 where
  out = rateAdaptRx linkSpeed $ orNothing <$> rxDv <*> rxDw
  linkSpeed = toLinkSpeed <$> regMaybe 0 rxConfReg
  (rxStatus, rxDv, rxEr, rxDw, xmit, txConfReg, rxConfReg, bsCg) = sgmiiRx rxCg

-- | Transmit side of the SGMII block, that combines all the functions that are
--   in the transmit domain, rate-adapted
sgmiiTxRA ::
  (HiddenClockResetEnable dom) =>
  -- | @TX_ER@ signal
  Signal dom Bool ->
  -- | Input data word
  Signal dom (Maybe (BitVector 8)) ->
  -- | 'Xmit' signal
  Signal dom (Maybe Xmit) ->
  -- | Configuration register from MAC
  Signal dom (Maybe ConfReg) ->
  -- | Configuration register from PHY
  Signal dom (Maybe ConfReg) ->
  -- | Ready signal and output code group
  (Signal dom Bool, Signal dom CodeGroup)
sgmiiTxRA txEr txDw xmit txConfReg rxConfReg = (txReady, txCg)
 where
  linkSpeed = toLinkSpeed <$> regMaybe 0 rxConfReg
  txCg =
    sgmiiTx (isJust <$> out) txEr (fromMaybe 0 <$> out) xmit txConfReg rxConfReg
  (txReady, out) = rateAdaptTx linkSpeed txDw

-- | Rate-adapted version of 'sgmii'
sgmiiRA ::
  forall rxDom txDom.
  (KnownDomain rxDom, KnownDomain txDom) =>
  -- | Function used for the clock domain crossing between 'autoNeg' and
  --   'pcsTransmit', for the values of 'Xmit' and 'ConfReg'
  ( Clock rxDom ->
    Clock txDom ->
    Signal rxDom (Maybe Xmit) ->
    Signal rxDom (Maybe ConfReg) ->
    Signal rxDom (Maybe ConfReg) ->
    ( Signal txDom (Maybe Xmit)
    , Signal txDom (Maybe ConfReg)
    , Signal txDom (Maybe ConfReg)
    )
  ) ->
  -- | Clock of the receive domain, which is a 125 MHz clock that is derived
  --   from the 625 MHz clock that is received from the PHY
  Clock rxDom ->
  -- | Clock of the transmit domain, which can be an internal clock of the FPGA
  Clock txDom ->
  -- | Reset of the receive domain
  Reset rxDom ->
  -- | Reset of the transmit domain
  Reset txDom ->
  -- | @TX_ER@ signal
  Signal txDom Bool ->
  -- | Data octet @TXD@ to be transmitted to the PHY
  Signal txDom (Maybe (BitVector 8)) ->
  -- | Input code group from the PHY
  Signal rxDom CodeGroup ->
  -- | Tuple that contains the output signals from the SGMII block which are the
  --   current status of the receive block 'SgmiiStatus', @RX_ER@ which
  --   indicates a receive error, @RXD@ which is the incoming data octet from
  --   the PHY, and a 10-bit code word that can be serialized and transmitted to
  --   the PHY. For debugging purposes, also a word-aligned version of the input
  --   word is outputted.
  ( Signal rxDom SgmiiStatus
  , Signal rxDom Bool
  , Signal rxDom (Maybe (BitVector 8))
  , Signal rxDom CodeGroup
  , Signal txDom CodeGroup
  , Signal txDom Bool
  )
sgmiiRA rxTxCdc rxClk txClk rxRst txRst txEr txDw rxCg =
  (rxStatus, rxEr, rxDw, bsCg, txCg, txReady)
 where
  (txReady, txCg) = sgmiiTx' txEr txDw xmit2 txConfReg2 rxConfReg2
   where
    sgmiiTx' = exposeClockResetEnable sgmiiTxRA txClk txRst enableGen

  (xmit2, txConfReg2, rxConfReg2) =
    rxTxCdc rxClk txClk xmit1 txConfReg1 rxConfReg1

  (rxStatus, rxEr, rxDw, xmit1, txConfReg1, rxConfReg1, bsCg) = sgmiiRx' rxCg
   where
    sgmiiRx' = exposeClockResetEnable sgmiiRxRA rxClk rxRst enableGen

{-# CLASH_OPAQUE sgmiiRA #-}
