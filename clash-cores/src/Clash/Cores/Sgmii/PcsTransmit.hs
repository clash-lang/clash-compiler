{-# LANGUAGE CPP #-}

module Clash.Cores.Sgmii.PcsTransmit where

import Clash.Cores.Sgmii.Common
import Clash.Cores.Sgmii.PcsTransmit.CodeGroup
import Clash.Cores.Sgmii.PcsTransmit.OrderedSet
import Clash.Prelude

-- | Takes the signals that are defined in IEEE 802.3 Clause 36 and runs them
--   through the state machines as defined for the PCS transmit block. These are
--   implemented in 'codeGroupT' and 'orderedSetT'.
pcsTransmit ::
  (HiddenClockResetEnable dom) =>
  -- | The @TX_EN@ signal
  Signal dom Bool ->
  -- | The @TX_ER@ signal
  Signal dom Bool ->
  -- | The new data word that needs to be transmitted
  Signal dom (BitVector 8) ->
  -- | The 'Xmit' signal from 'Sgmii.autoNeg'
  Signal dom (Maybe Xmit) ->
  -- | The 'ConfReg' from 'Sgmii.autoNeg'
  Signal dom (Maybe ConfReg) ->
  -- | The 8b/10b encoded output value
  Signal dom (BitVector 10)
pcsTransmit txEn txEr dw xmit txConfReg = cg
 where
  (_, cg, txEven, cgSent) =
    mooreB
      codeGroupT
      codeGroupO
      (SpecialGo False 0 0 Even OSetC)
      (txOSet, dw, txConfReg)

  (_, txOSet) =
    mealyB
      orderedSetT
      (Configuration Conf False)
      (txEn, txEr, dw, xmit, txEven, cgSent)

{-# CLASH_OPAQUE pcsTransmit #-}
