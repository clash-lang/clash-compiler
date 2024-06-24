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
  forall dom0 dom1.
  (KnownDomain dom0, KnownDomain dom1) =>
  (Signal dom0 Xmit -> Signal dom0 ConfReg -> Signal dom1 (Xmit, ConfReg)) ->
  Clock dom0 ->
  Clock dom1 ->
  Reset dom0 ->
  Reset dom1 ->
  Signal dom1 Bool ->
  Signal dom1 Bool ->
  Signal dom1 (BitVector 8) ->
  Signal dom0 (BitVector 10) ->
  (Signal dom0 (Bool, Bool, BitVector 8), Signal dom1 (BitVector 10))
sgmiiCdc autoNegCdc clk0 clk1 rst0 rst1 txEn txEr dw1 cg1 =
  ( bundle
      ( exposeClockResetEnable regMaybe clk0 rst0 enableGen False rxDv
      , exposeClockResetEnable regMaybe clk0 rst0 enableGen False rxEr
      , exposeClockResetEnable
          regMaybe
          clk0
          rst0
          enableGen
          0
          ((fmap . fmap) fromDw dw3)
      )
  , exposeClockResetEnable
      pcsTransmit
      clk1
      rst1
      enableGen
      txEn
      txEr
      dw1
      xmit2
      txConfReg2
  )
 where
  (cg2, rd, dw2, rxEven, syncStatus) =
    unbundle
      $ exposeClockResetEnable sync clk0 rst0 enableGen
      $ fst
      <$> exposeClockResetEnable bitSlip clk0 rst0 enableGen cg1

  (rxDv, rxEr, dw3, rudi, rxConfReg) =
    unbundle
      $ exposeClockResetEnable
        pcsReceive
        clk0
        rst0
        enableGen
        cg2
        rd
        dw2
        rxEven
        syncStatus
        xmit1

  (xmit2, txConfReg2) = unbundle $ autoNegCdc xmit1 txConfReg1

  (xmit1, txConfReg1) =
    unbundle
      $ exposeClockResetEnable
        autoNeg
        clk0
        rst0
        enableGen
        0b0100000000000001
        syncStatus
        rudi
        rxConfReg

{-# CLASH_OPAQUE sgmiiCdc #-}
