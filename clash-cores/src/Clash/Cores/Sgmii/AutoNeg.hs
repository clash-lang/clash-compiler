{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

-- |
--   Copyright   :  (C) 2024, QBayLogic B.V.
--   License     :  BSD2 (see the file LICENSE)
--   Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
--
--   Auto-negotiation process, as defined in IEEE 802.3 Figure 37-6
module Clash.Cores.Sgmii.AutoNeg where

import Clash.Cores.Sgmii.Common
import Clash.Prelude
import Data.Maybe (fromJust, isJust)
import Data.Proxy

-- | List of values for 'ConfReg'
type ConfRegs = Vec 3 ConfReg

-- | List of values for 'Rudi'
type Rudis = Vec 3 Rudi

-- | Type that specifies an 'Index' for the timeout of the link timer and the
--   timer used to qualify the 'Fail' status of 'SyncStatus'

--   TODO: Replace this with @PeriodToCycles dom (Microseconds 1600)@, currently
--   this doesn't work because then I need to specify @1 <= DomainPeriod dom)
--   everywhere.
type Timeout dom = Index (DivRU (Microseconds 1600) (Max 1 (DomainPeriod dom)))

-- | State type of 'autoNeg'. This contains all states as they are defined in
--   IEEE 802.3 Clause 37, with exception of the @AN_DISABLE_LINK_OK@ state as
--   SGMII always requires auto-negotiation to be available.
data AutoNegState dom
  = AnEnable {_rudis :: Rudis, _rxConfRegs :: ConfRegs, _failT :: Timeout dom}
  | AnRestart
      { _rudis :: Rudis
      , _rxConfRegs :: ConfRegs
      , _failT :: Timeout dom
      , _linkT :: Timeout dom
      }
  | AbilityDetect
      { _rudis :: Rudis
      , _rxConfRegs :: ConfRegs
      , _failT :: Timeout dom
      , _txConfReg :: ConfReg
      }
  | AcknowledgeDetect
      { _rudis :: Rudis
      , _rxConfRegs :: ConfRegs
      , _failT :: Timeout dom
      , _txConfReg :: ConfReg
      }
  | CompleteAcknowledge
      { _rudis :: Rudis
      , _rxConfRegs :: ConfRegs
      , _failT :: Timeout dom
      , _linkT :: Timeout dom
      }
  | IdleDetect
      { _rudis :: Rudis
      , _rxConfRegs :: ConfRegs
      , _failT :: Timeout dom
      , _linkT :: Timeout dom
      }
  | LinkOk {_rudis :: Rudis, _rxConfRegs :: ConfRegs, _failT :: Timeout dom}
  deriving (Generic, NFDataX, Eq, Show)

-- | The default configuration of the MAC as defined in the SGMII standard
mrAdvAbility :: ConfReg
mrAdvAbility = 0b0100000000000001

-- | The duration of @linkT@ is 1.6 ms according to the SGMII reference,
--   which means that it has a frequency of 625 Hz. This is the same as 200000
--   cycles of the 125 MHz clock: @1.6*10^-3 / (1 / (125*10^6))@.
--
--   For simulation and testing, this is set to a more reasonable amount of 3
--   to decrease the amount of test values that are needed to trigger a timeout.
timeout :: (KnownDomain dom) => Proxy dom -> Timeout dom
timeout Proxy = if clashSimulation then 3 else maxBound

-- | Function that handles the reset to 'AnEnable', this is split out to reduce
--   the amount of state transitions in every state
anEnable ::
  forall dom.
  (KnownDomain dom) =>
  -- | Fail timer value
  Timeout dom ->
  -- | New incoming RUDI value
  Maybe Rudi ->
  -- | History of RUDI values
  Rudis ->
  -- | History of configuration registers
  ConfRegs ->
  -- | Possible state transition
  Maybe (AutoNegState dom)
anEnable failT rudi rudis rxConfRegs
  | failT >= timeout (Proxy @dom) =
      Just $ AnEnable rudis rxConfRegs (timeout (Proxy @dom) - 1)
  | rudi == Just Invalid = Just $ AnEnable rudis rxConfRegs failT
  | otherwise = Nothing

-- | General part of the status update of the auto negotiation function, where
--   the new values of 'Rudi', 'ConfReg' and the 'Timeout's are handled.
anUpdate ::
  (KnownDomain dom) =>
  AutoNegState dom ->
  SyncStatus ->
  Maybe Rudi ->
  Maybe ConfReg ->
  (Rudis, ConfRegs, Timeout dom, Timeout dom)
anUpdate s syncStatus rudi rxConfReg = (rudis, rxConfRegs, failT, linkT)
 where
  rudis = maybe s._rudis (s._rudis <<+) rudi
  rxConfRegs = maybe s._rxConfRegs (s._rxConfRegs <<+) rxConfReg
  failT = if syncStatus == Fail then s._failT + 1 else 0
  linkT = s._linkT + 1

-- | Check if the the last three received values of @rxConfReg@ are the same
--   (with the exception for bit 14, the acknowledge bit, which is discarded).
--   If there has been 'Rudi' value of 'I' in the same set of values, then
--   return 'False'.
abilityMatch ::
  -- | Last three values for 'Rudi'
  Rudis ->
  -- | Last three values for 'ConfReg'
  ConfRegs ->
  -- | Whether they satisfy the 'abilityMatch' condition
  Bool
abilityMatch rudis rxConfRegs =
  repeat (head rxConfRegs') == rxConfRegs' && I `notElem` rudis
 where
  rxConfRegs' = map (replaceBit (14 :: Index 16) 0) rxConfRegs

-- | Check if the last three values for 'ConfReg' are all the same, and also
--   check whether bit 14 (the acknowledge bit) has been asserted
acknowledgeMatch ::
  -- | Last three values for 'ConfReg'
  ConfRegs ->
  -- | Whether they satisfy the 'acknowledgeMatch' condition
  Bool
acknowledgeMatch rxConfRegs =
  repeat (head rxConfRegs) == rxConfRegs && testBit (head rxConfRegs) 14

-- | Check if both 'abilityMatch' and 'acknowledgeMatch' are true for the same
--   set of 'Rudi' and 'ConfReg' values.
consistencyMatch ::
  -- | Last three values for 'Rudi'
  Rudis ->
  -- | Last three values for 'ConfReg'
  ConfRegs ->
  -- | Whether they satisfy the 'consistencyMatch' condition
  Bool
consistencyMatch rudis rxConfigRegs =
  abilityMatch rudis rxConfigRegs && acknowledgeMatch rxConfigRegs

-- | Function that checks that the last three values of 'Rudi' have been 'I'
idleMatch :: Rudis -> Bool
idleMatch = (==) (repeat I)

-- | State transition function for 'autoNeg' as defined in IEEE 802.3 Clause 37.
--   It takes the current 'SyncStatus' from 'Sgmii.sync' as well as the 'Rudi'
--   and 'ConfReg' signals from 'Sgmii.pcsReceive'.
autoNegT ::
  forall dom.
  (KnownDomain dom) =>
  -- | Current state
  AutoNegState dom ->
  -- | New input values
  (SyncStatus, Maybe Rudi, Maybe ConfReg) ->
  -- | New state
  AutoNegState dom
autoNegT self@AnEnable{} (syncStatus, rudi, rxConfReg)
  | isJust s = fromJust s
  | otherwise = AnRestart rudis rxConfRegs failT 0
 where
  s = anEnable failT rudi rudis rxConfRegs
  (rudis, rxConfRegs, failT, _) = anUpdate self syncStatus rudi rxConfReg
autoNegT self@AnRestart{} (syncStatus, rudi, rxConfReg)
  | isJust s = fromJust s
  | linkT >= timeout (Proxy @dom) =
      AbilityDetect rudis rxConfRegs failT mrAdvAbility
  | otherwise = AnRestart rudis rxConfRegs failT linkT
 where
  s = anEnable failT rudi rudis rxConfRegs
  (rudis, rxConfRegs, failT, linkT) = anUpdate self syncStatus rudi rxConfReg
autoNegT self@AbilityDetect{} (syncStatus, rudi, rxConfReg)
  | isJust s = fromJust s
  | abilityMatch rudis rxConfRegs && last rxConfRegs /= 0 =
      AcknowledgeDetect rudis rxConfRegs failT txConfReg
  | otherwise = AbilityDetect rudis rxConfRegs failT mrAdvAbility
 where
  s = anEnable failT rudi rudis rxConfRegs
  (rudis, rxConfRegs, failT, _) = anUpdate self syncStatus rudi rxConfReg
  txConfReg = replaceBit (14 :: Index 16) 0 mrAdvAbility
autoNegT self@AcknowledgeDetect{..} (syncStatus, rudi, rxConfReg)
  | isJust s = fromJust s
  | acknowledgeMatch rxConfRegs && not (consistencyMatch rudis rxConfRegs) =
      AnEnable rudis rxConfRegs failT
  | abilityMatch rudis rxConfRegs && last rxConfRegs == 0 =
      AnEnable rudis rxConfRegs failT
  | acknowledgeMatch rxConfRegs && consistencyMatch rudis rxConfRegs =
      CompleteAcknowledge rudis rxConfRegs failT 0
  | otherwise = AcknowledgeDetect rudis rxConfRegs failT txConfReg
 where
  s = anEnable failT rudi rudis rxConfRegs
  (rudis, rxConfRegs, failT, _) = anUpdate self syncStatus rudi rxConfReg
  txConfReg = replaceBit (14 :: Index 16) 1 _txConfReg
autoNegT self@CompleteAcknowledge{} (syncStatus, rudi, rxConfReg)
  | isJust s = fromJust s
  | abilityMatch rudis rxConfRegs && last rxConfRegs == 0 =
      AnEnable rudis rxConfRegs failT
  | linkT >= timeout (Proxy @dom) && not (abilityMatch rudis rxConfRegs) =
      IdleDetect rudis rxConfRegs failT 0
  | linkT >= timeout (Proxy @dom) && last rxConfRegs /= 0 =
      IdleDetect rudis rxConfRegs failT 0
  | otherwise = CompleteAcknowledge rudis rxConfRegs failT linkT
 where
  s = anEnable failT rudi rudis rxConfRegs
  (rudis, rxConfRegs, failT, linkT) = anUpdate self syncStatus rudi rxConfReg
autoNegT self@IdleDetect{} (syncStatus, rudi, rxConfReg)
  | isJust s = fromJust s
  | abilityMatch rudis rxConfRegs && last rxConfRegs == 0 =
      AnEnable rudis rxConfRegs failT
  | linkT >= timeout (Proxy @dom) && idleMatch rudis =
      LinkOk rudis rxConfRegs failT
  | otherwise = IdleDetect rudis rxConfRegs failT linkT
 where
  s = anEnable failT rudi rudis rxConfRegs
  (rudis, rxConfRegs, failT, linkT) = anUpdate self syncStatus rudi rxConfReg
autoNegT self@LinkOk{} (syncStatus, rudi, rxConfReg)
  | isJust s = fromJust s
  | abilityMatch rudis rxConfRegs = AnEnable rudis rxConfRegs failT
  | otherwise = LinkOk rudis rxConfRegs failT
 where
  s = anEnable failT rudi rudis rxConfRegs
  (rudis, rxConfRegs, failT, _) = anUpdate self syncStatus rudi rxConfReg

-- | Output function for 'autoNeg' as defined in IEEE 802.3 Clause 37. Returns
--   the new value for 'Xmit' and 'ConfReg' for 'Sgmii.pcsTransmit'.
autoNegO ::
  forall dom.
  (KnownDomain dom) =>
  -- | Current state
  AutoNegState dom ->
  -- | New outputs
  (AutoNegState dom, Maybe Xmit, Maybe ConfReg)
autoNegO self@AnEnable{} = (self, Just Conf, Just 0)
autoNegO self@AnRestart{} = (self, Nothing, Just 0)
autoNegO self@AbilityDetect{..} = (self, Nothing, Just txConfReg)
 where
  txConfReg = replaceBit (14 :: Index 16) 0 _txConfReg
autoNegO self@AcknowledgeDetect{..} = (self, Nothing, Just txConfReg)
 where
  txConfReg = replaceBit (14 :: Index 16) 1 _txConfReg
autoNegO self@CompleteAcknowledge{} = (self, Nothing, Nothing)
autoNegO self@IdleDetect{} = (self, Just Idle, Nothing)
autoNegO self@LinkOk{} = (self, Just Data, Nothing)

-- | Function that implements the auto-negotiation block as defined in IEEE
--   802.3 Clause 37, but modified to comply to the SGMII standard. This
--   modification is the decrease of 'Timeout' from 10 ms to 1.6 ms. SGMII also
--   uses a different layout of the configuration register, but this does not
--   affect the state machine as the acknowledge bit is in the same location.
--
--   __N.B.__: This function does not implement the optional Next Page function.
autoNeg ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  -- | Current 'SyncStatus' from 'Sgmii.sync'
  Signal dom SyncStatus ->
  -- | A new value of 'Rudi' from 'Sgmii.pcsReceive'
  Signal dom (Maybe Rudi) ->
  -- | A new value of 'ConfReg' from 'Sgmii.pcsReceive'
  Signal dom (Maybe ConfReg) ->
  -- | Tuple containing the new value for 'Xmit' and a new 'ConfReg'
  (Signal dom (Maybe Xmit), Signal dom (Maybe ConfReg))
autoNeg syncStatus rudi rxConfReg = (xmit, txConfReg)
 where
  (_, xmit, txConfReg) =
    mooreB
      (autoNegT @dom)
      (autoNegO @dom)
      (AnEnable (repeat Invalid) (repeat 0) 0)
      (syncStatus, rudi, rxConfReg)

{-# CLASH_OPAQUE autoNeg #-}
