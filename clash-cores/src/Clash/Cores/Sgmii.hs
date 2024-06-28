{-# LANGUAGE CPP #-}

module Clash.Cores.Sgmii where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.AutoNeg
import Clash.Cores.Sgmii.BitSlip
import Clash.Cores.Sgmii.Common
import Clash.Cores.Sgmii.PcsReceive
import Clash.Cores.Sgmii.PcsTransmit
import Clash.Cores.Sgmii.Sync
import Clash.Prelude

sgmiiCdc ::
  forall rxDom txDom.
  (KnownDomain rxDom, KnownDomain txDom) =>
  ( Clock rxDom ->
    Clock txDom ->
    Signal rxDom (Maybe Xmit) ->
    Signal rxDom (Maybe ConfReg) ->
    Signal txDom (Maybe Xmit, Maybe ConfReg)
  ) ->
  Clock rxDom ->
  Clock txDom ->
  Reset rxDom ->
  Reset txDom ->
  Signal txDom Bool ->
  Signal txDom Bool ->
  Signal txDom (BitVector 8) ->
  Signal rxDom (BitVector 10) ->
  ( Signal rxDom (Bool, Bool, BitVector 8, BitVector 8, BitVector 10)
  , Signal txDom (BitVector 10)
  )
sgmiiCdc autoNegCdc rxClk txClk rxRst txRst txEn txEr dw1 cg1 =
  ( bundle
      ( exposeClockResetEnable regMaybe rxClk rxRst enableGen False rxDv
      , exposeClockResetEnable regMaybe rxClk rxRst enableGen False rxEr
      , exposeClockResetEnable regMaybe rxClk rxRst enableGen 0 dw4
      , fromDw . head <$> dw2
      , cg2
      )
  , cg4
  )
 where
  cg4 = pcsTransmit' txClk txRst enableGen txEn txEr dw1 xmit2 txConfReg2
   where
    pcsTransmit' = exposeClockResetEnable pcsTransmit

  (xmit2, txConfReg2) =
    unbundle $ autoNegCdc rxClk txClk xmit1 txConfReg1

  (xmit1, txConfReg1) =
    unbundle
      $ autoNeg' rxClk rxRst enableGen confReg syncStatus rudi rxConfReg
   where
    autoNeg' = exposeClockResetEnable autoNeg
    confReg = pure 0b0100000000000001

  dw4 = (fmap . fmap) fromDw dw3

  (rxDv, rxEr, dw3, rudi, rxConfReg) =
    unbundle
      $ pcsReceive' rxClk rxRst enableGen cg3 rd dw2 rxEven syncStatus xmit1
   where
    pcsReceive' = exposeClockResetEnable pcsReceive

  (cg3, rd, dw2, rxEven, syncStatus) =
    unbundle $ sync' rxClk rxRst enableGen cg2
   where
    sync' = exposeClockResetEnable sync

  (cg2, _) = unbundle $ bitSlip' rxClk rxRst enableGen cg1
   where
    bitSlip' = exposeClockResetEnable bitSlip

{-# CLASH_OPAQUE sgmiiCdc #-}
