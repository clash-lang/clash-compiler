{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

module Clash.Cores.Sgmii.AutoNeg where

import Clash.Cores.Sgmii.Common
import Clash.Prelude
import Data.Proxy

-- | Type that specifies an 'Index' for the timeout of the link timer and the
--   timer used to qualify the 'Fail' status of 'SyncStatus'
type Timeout dom = Index (DivRU (1600000000) (Max 1 (DomainPeriod dom)))

-- | State type of 'autoNeg'. This contains all states as they are defined in
--   IEEE 802.3 Clause 37, with exception of the @AN_DISABLE_LINK_OK@ state as
--   SGMII always requires auto negotiation to be available
data AutoNegState dom
  = AnEnable
      { _rudis :: Vec 3 Rudi
      , _rxConfRegs :: Vec 3 ConfReg
      , _failTimer :: Timeout dom
      }
  | AnRestart
      { _rudis :: Vec 3 Rudi
      , _rxConfRegs :: Vec 3 ConfReg
      , _failTimer :: Timeout dom
      , _linkTimer :: Timeout dom
      }
  | AbilityDetect
      { _rudis :: Vec 3 Rudi
      , _rxConfRegs :: Vec 3 ConfReg
      , _failTimer :: Timeout dom
      }
  | AcknowledgeDetect
      { _rudis :: Vec 3 Rudi
      , _rxConfRegs :: Vec 3 ConfReg
      , _failTimer :: Timeout dom
      , _txConfReg :: ConfReg
      }
  | CompleteAcknowledge
      { _rudis :: Vec 3 Rudi
      , _rxConfRegs :: Vec 3 ConfReg
      , _failTimer :: Timeout dom
      , _linkTimer :: Timeout dom
      , _txConfReg :: ConfReg
      }
  | IdleDetect
      { _rudis :: Vec 3 Rudi
      , _rxConfRegs :: Vec 3 ConfReg
      , _failTimer :: Timeout dom
      , _linkTimer :: Timeout dom
      , _txConfReg :: ConfReg
      }
  | LinkOk
      { _rudis :: Vec 3 Rudi
      , _rxConfRegs :: Vec 3 ConfReg
      , _failTimer :: Timeout dom
      , _txConfReg :: ConfReg
      }
  deriving (Generic, NFDataX, Eq, Show)

-- | The duration of @linkTimer@ is 1.6 ms according to the SGMII reference,
--   which means that it has a frequency of 625 Hz. This is the same as 200000
--   cycles of the 125 MHz clock: @1.6*10^−3 / (1 / (125×10^6))@.
--
--   For simulation and testing, this is set to a more reasonable amount of 3
--   to decrease the amount of test values that are needed to trigger a timeout.
timeout :: (KnownDomain dom) => Proxy dom -> Timeout dom
timeout Proxy = if clashSimulation then 3 else maxBound

-- | Check if the the last three received values of @rxConfReg@ are the same
--   (with the exception for bit 14, the acknowledge bit, which is discarded).
--   If there has been 'Rudi' value of 'I' in the same set of values, then
--   return False.
abilityMatch ::
  -- | Last three values for 'Rudi'
  Vec 3 Rudi ->
  -- | Last three values for 'ConfReg'
  Vec 3 ConfReg ->
  -- | Whether they satisfy the 'abilityMatch' condition
  Bool
abilityMatch rudis rxConfRegs =
  repeat (head rxConfRegs1) == rxConfRegs1 && I `notElem` rudis
 where
  rxConfRegs1 = map (replaceBit (14 :: Index 16) 0) rxConfRegs

-- | Check if the last three values for 'ConfReg' are all the same, and also
--   check whether bit 14 (the acknowledge bit) has been asserted
acknowledgeMatch ::
  -- | Last three values for 'ConfReg'
  Vec 3 ConfReg ->
  -- | Whether they satisfy the 'acknowledgeMatch' condition
  Bool
acknowledgeMatch rxConfRegs =
  repeat (head rxConfRegs) == rxConfRegs && testBit (head rxConfRegs) 14

-- | Check if both 'abilityMatch' and 'acknowledgeMatch' are true for the same
--   set of 'Rudi' and 'ConfReg' values.
consistencyMatch ::
  -- | Last three values for 'Rudi'
  Vec 3 Rudi ->
  -- | Last three values for 'ConfReg'
  Vec 3 ConfReg ->
  -- | Whether they satisfy the 'consistencyMatch' condition
  Bool
consistencyMatch rudis rxConfigRegs =
  abilityMatch rudis rxConfigRegs && acknowledgeMatch rxConfigRegs

-- | Function that checks that the last three values of 'Rudi' have been 'I'
idleMatch :: Vec 3 Rudi -> Bool
idleMatch = (==) (repeat I)

-- | State transition function for 'autoNeg' as defined in IEEE 802.3 Clause 37.
--   It takes the current 'SyncStatus' from 'Sgmii.sync' as well as the 'Rudi'
--   and 'ConfReg' signals from 'Sgmii.pcsReceive', and returns the new value
--   for 'Xmit' and 'ConfReg' for 'Sgmii.pcsTransmit'.
autoNegT ::
  forall dom.
  (KnownDomain dom) =>
  -- | Initial state of the auto negotiation process
  AutoNegState dom ->
  -- | Inputs of the state transition function
  (ConfReg, SyncStatus, Maybe Rudi, Maybe ConfReg) ->
  -- | Tuple with the new state and the outputs of the state transition function
  (AutoNegState dom, (AutoNegState dom, Xmit, ConfReg))
autoNegT self@AnEnable{..} (_, syncStatus, rudi, rxConfRegNew) =
  (nextState, out)
 where
  nextState
    | failTimer >= timeout (Proxy @dom) =
        AnEnable rudis rxConfRegs (timeout (Proxy @dom) - 1)
    | rudi == Just Invalid = AnEnable rudis rxConfRegs failTimer
    | otherwise = AnRestart rudis rxConfRegs failTimer 0

  rudis = maybe _rudis (_rudis <<+) rudi
  rxConfRegs = maybe _rxConfRegs (_rxConfRegs <<+) rxConfRegNew
  txConfReg = 0
  failTimer = if syncStatus == Fail then _failTimer + 1 else 0
  xmit = Conf

  out = (self, xmit, txConfReg)
autoNegT self@AnRestart{..} (_, syncStatus, rudi, rxConfRegNew) =
  (nextState, out)
 where
  nextState
    | failTimer >= timeout (Proxy @dom) =
        AnEnable rudis rxConfRegs (timeout (Proxy @dom) - 1)
    | rudi == Just Invalid = AnEnable rudis rxConfRegs failTimer
    | linkTimer >= timeout (Proxy @dom) =
        AbilityDetect rudis rxConfRegs failTimer
    | otherwise = AnRestart rudis rxConfRegs failTimer linkTimer

  rudis = maybe _rudis (_rudis <<+) rudi
  rxConfRegs = maybe _rxConfRegs (_rxConfRegs <<+) rxConfRegNew
  linkTimer = _linkTimer + 1
  failTimer = if syncStatus == Fail then _failTimer + 1 else 0
  xmit = Conf

  out = (self, xmit, 0)
autoNegT self@AbilityDetect{..} (mrAdvAbility, syncStatus, rudi, rxConfRegNew) =
  (nextState, out)
 where
  nextState
    | failTimer >= timeout (Proxy @dom) =
        AnEnable rudis rxConfRegs (timeout (Proxy @dom) - 1)
    | rudi == Just Invalid = AnEnable rudis rxConfRegs failTimer
    | abilityMatch rudis rxConfRegs && last rxConfRegs /= 0 =
        AcknowledgeDetect rudis rxConfRegs failTimer txConfReg
    | otherwise = AbilityDetect rudis rxConfRegs failTimer

  rudis = maybe _rudis (_rudis <<+) rudi
  rxConfRegs = maybe _rxConfRegs (_rxConfRegs <<+) rxConfRegNew
  txConfReg = replaceBit (14 :: Index 16) 0 mrAdvAbility
  failTimer = if syncStatus == Fail then _failTimer + 1 else 0
  xmit = Conf

  out = (self, xmit, txConfReg)
autoNegT self@AcknowledgeDetect{..} (_, syncStatus, rudi, rxConfRegNew) =
  (nextState, out)
 where
  nextState
    | failTimer >= timeout (Proxy @dom) =
        AnEnable rudis rxConfRegs (timeout (Proxy @dom) - 1)
    | rudi == Just Invalid = AnEnable rudis rxConfRegs failTimer
    | acknowledgeMatch rxConfRegs && not (consistencyMatch rudis rxConfRegs) =
        AnEnable rudis rxConfRegs failTimer
    | abilityMatch rudis rxConfRegs && last rxConfRegs == 0 =
        AnEnable rudis rxConfRegs failTimer
    | acknowledgeMatch rxConfRegs && consistencyMatch rudis rxConfRegs =
        CompleteAcknowledge rudis rxConfRegs failTimer 0 txConfReg
    | otherwise = AcknowledgeDetect rudis rxConfRegs failTimer txConfReg

  rudis = maybe _rudis (_rudis <<+) rudi
  rxConfRegs = maybe _rxConfRegs (_rxConfRegs <<+) rxConfRegNew
  txConfReg = replaceBit (14 :: Index 16) 1 _txConfReg
  failTimer = if syncStatus == Fail then _failTimer + 1 else 0
  xmit = Conf

  out = (self, xmit, txConfReg)
autoNegT self@CompleteAcknowledge{..} (_, syncStatus, rudi, rxConfRegNew) =
  (nextState, out)
 where
  nextState
    | failTimer >= timeout (Proxy @dom) =
        AnEnable rudis rxConfRegs (timeout (Proxy @dom) - 1)
    | rudi == Just Invalid = AnEnable rudis rxConfRegs failTimer
    | abilityMatch rudis rxConfRegs && last rxConfRegs == 0 =
        AnEnable rudis rxConfRegs failTimer
    | linkTimer >= timeout (Proxy @dom) && not (abilityMatch rudis rxConfRegs) =
        IdleDetect rudis rxConfRegs failTimer 0 _txConfReg
    | linkTimer >= timeout (Proxy @dom) && last rxConfRegs /= 0 =
        IdleDetect rudis rxConfRegs failTimer 0 _txConfReg
    | otherwise =
        CompleteAcknowledge rudis rxConfRegs failTimer linkTimer _txConfReg

  rudis = maybe _rudis (_rudis <<+) rudi
  rxConfRegs = maybe _rxConfRegs (_rxConfRegs <<+) rxConfRegNew
  linkTimer = _linkTimer + 1
  failTimer = if syncStatus == Fail then _failTimer + 1 else 0
  xmit = Conf

  out = (self, xmit, _txConfReg)
autoNegT self@IdleDetect{..} (_, syncStatus, rudi, rxConfRegNew) =
  (nextState, out)
 where
  nextState
    | failTimer >= timeout (Proxy @dom) =
        AnEnable rudis rxConfRegs (timeout (Proxy @dom) - 1)
    | rudi == Just Invalid = AnEnable rudis rxConfRegs failTimer
    | abilityMatch rudis rxConfRegs && last rxConfRegs == 0 =
        AnEnable rudis rxConfRegs failTimer
    | linkTimer >= timeout (Proxy @dom) && idleMatch rudis =
        LinkOk rudis rxConfRegs failTimer _txConfReg
    | otherwise = IdleDetect rudis rxConfRegs failTimer linkTimer _txConfReg

  rudis = maybe _rudis (_rudis <<+) rudi
  rxConfRegs = maybe _rxConfRegs (_rxConfRegs <<+) rxConfRegNew
  linkTimer = _linkTimer + 1
  failTimer = if syncStatus == Fail then _failTimer + 1 else 0
  xmit = Idle

  out = (self, xmit, _txConfReg)
autoNegT self@LinkOk{..} (_, syncStatus, rudi, rxConfRegNew) = (nextState, out)
 where
  nextState
    | failTimer >= timeout (Proxy @dom) =
        AnEnable rudis rxConfRegs (timeout (Proxy @dom) - 1)
    | rudi == Just Invalid = AnEnable rudis rxConfRegs failTimer
    | abilityMatch rudis rxConfRegs = AnEnable rudis rxConfRegs failTimer
    | otherwise = LinkOk rudis rxConfRegs failTimer _txConfReg

  rudis = maybe _rudis (_rudis <<+) rudi
  rxConfRegs = maybe _rxConfRegs (_rxConfRegs <<+) rxConfRegNew
  failTimer = if syncStatus == Fail then _failTimer + 1 else 0
  xmit = Data

  out = (self, xmit, _txConfReg)

-- | Function that implements the auto negotiation block as defined in IEEE
--   802.3 Clause 37
autoNeg ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  -- | Abilities of the current system
  Signal dom ConfReg ->
  -- | Current 'SyncStatus' from 'Sgmii.sync'
  Signal dom SyncStatus ->
  -- | A new value of 'Rudi' from 'Sgmii.pcsReceive'
  Signal dom (Maybe Rudi) ->
  -- | A new value of 'ConfReg' from 'Sgmii.pcsReceive'
  Signal dom (Maybe ConfReg) ->
  -- | Tuple containing the new value for 'Xmit' and a new 'ConfReg'
  Signal dom (Xmit, ConfReg)
autoNeg confReg syncStatus rudi rxConfReg = bundle (xmit, txConfReg)
 where
  (_, xmit, txConfReg) =
    mealyB
      (autoNegT @dom)
      (AnEnable (repeat Invalid) (repeat 0) 0)
      (confReg, syncStatus, rudi, rxConfReg)

{-# CLASH_OPAQUE autoNeg #-}
