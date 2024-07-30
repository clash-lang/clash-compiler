{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=10 #-}

-- |
--   Copyright   :  (C) 2024, QBayLogic B.V.
--   License     :  BSD2 (see the file LICENSE)
--   Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
--
--   Auto-negotiation process, as defined in IEEE 802.3 Figure 37-6
module Clash.Cores.Sgmii.AutoNeg
  ( AutoNegState (..)
  , Rudis
  , Timeout
  , autoNeg
  , autoNegO
  , autoNegT
  )
where

import Clash.Cores.Sgmii.Common
import Clash.Prelude
import Data.Maybe (fromMaybe)
import Data.Proxy

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
  = AnEnable
      {_rudis :: Maybe Rudis, _rxConfReg :: ConfReg, _failT :: Timeout dom}
  | AnRestart
      { _rudis :: Maybe Rudis
      , _rxConfReg :: ConfReg
      , _failT :: Timeout dom
      , _linkT :: Timeout dom
      }
  | AbilityDetect
      {_rudis :: Maybe Rudis, _rxConfReg :: ConfReg, _failT :: Timeout dom}
  | AckDetect
      { _rudis :: Maybe Rudis
      , _rxConfReg :: ConfReg
      , _failT :: Timeout dom
      , _hist :: ConfReg
      }
  | CompleteAck
      { _rudis :: Maybe Rudis
      , _rxConfReg :: ConfReg
      , _failT :: Timeout dom
      , _linkT :: Timeout dom
      }
  | IdleDetect
      { _rudis :: Maybe Rudis
      , _rxConfReg :: ConfReg
      , _failT :: Timeout dom
      , _linkT :: Timeout dom
      }
  | LinkOk
      {_rudis :: Maybe Rudis, _rxConfReg :: ConfReg, _failT :: Timeout dom}
  deriving (Generic, NFDataX, Eq, Show)

-- | Set the acknowledge bit of a 'ConfReg' to zero
noAckBit :: ConfReg -> ConfReg
noAckBit = replaceBit (14 :: Index 16) 0

-- | The duration of @linkT@ is 1.6 ms according to the SGMII reference,
--   which means that it has a frequency of 625 Hz. This is the same as 200000
--   cycles of the 125 MHz clock: @1.6*10^-3 / (1 / (125*10^6))@.
--
--   For simulation and testing, this is set to a more reasonable amount of 3
--   to decrease the amount of test values that are needed to trigger a timeout.
timeout :: (KnownDomain dom) => Proxy dom -> Timeout dom
timeout Proxy = if clashSimulation then 3 else maxBound

-- | Check if the the last three received values of @rxConfReg@ are the same
--   (with the exception for bit 14, the acknowledge bit, which is discarded).
--   If there has been 'Rudi' value of 'I' in the same set of values, then
--   return 'False'.
abilityMatch :: Rudis -> Bool
abilityMatch rudis = repeat (head rxConfRegs) == rxConfRegs && I `notElem` rudis
 where
  rxConfRegs = map (noAckBit . fromMaybe 0 . toConfReg) rudis

-- | Check if the last three values for 'ConfReg' are all the same, and also
--   check whether bit 14 (the acknowledge bit) has been asserted
ackMatch :: Rudis -> Bool
ackMatch rudis =
  repeat (head rxConfRegs) == rxConfRegs && testBit (head rxConfRegs) 14
 where
  rxConfRegs = map (fromMaybe 0 . toConfReg) rudis

-- | Check if ability match and acknowledge match are set for the same value of
--   'ConfReg'
consistencyMatch :: ConfReg -> Rudis -> Bool
consistencyMatch rxConfReg rudis = noAckBit rxConfReg == head rxConfRegs'
 where
  rxConfRegs' = map (noAckBit . fromMaybe 0 . toConfReg) rudis

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
  (SyncStatus, Maybe Rudi) ->
  -- | New state
  AutoNegState dom
autoNegT self (syncStatus, rudi)
  | failT >= timeout (Proxy @dom) =
      AnEnable (Just rudis) rxConfReg (timeout (Proxy @dom) - 1)
  | rudi == Just Invalid = AnEnable (Just rudis) rxConfReg failT
  | otherwise = case self of
      AnEnable{}
        | otherwise -> AnRestart Nothing rxConfReg failT 0
      AnRestart{}
        | linkT >= timeout (Proxy @dom) -> AbilityDetect Nothing rxConfReg failT
        | otherwise -> AnRestart (Just rudis) rxConfReg failT linkT
      AbilityDetect{}
        | abilityMatch rudis && rxConfReg /= 0 ->
            AckDetect Nothing rxConfReg failT rxConfReg
        | otherwise -> AbilityDetect (Just rudis) rxConfReg failT
      AckDetect{}
        | ackMatch rudis && not (consistencyMatch (_rxConfReg self) rudis) ->
            AnEnable Nothing rxConfReg failT
        | abilityMatch rudis && rxConfReg == 0 ->
            AnEnable Nothing rxConfReg failT
        | ackMatch rudis && consistencyMatch (_rxConfReg self) rudis ->
            CompleteAck Nothing rxConfReg failT 0
        | otherwise -> AckDetect (Just rudis) rxConfReg failT (_hist self)
      CompleteAck{}
        | abilityMatch rudis && rxConfReg == 0 ->
            AnEnable Nothing rxConfReg failT
        | linkT >= timeout (Proxy @dom) && not (abilityMatch rudis) ->
            IdleDetect Nothing rxConfReg failT 0
        | linkT >= timeout (Proxy @dom) && rxConfReg /= 0 ->
            IdleDetect Nothing rxConfReg failT 0
        | otherwise -> CompleteAck (Just rudis) rxConfReg failT linkT
      IdleDetect{}
        | abilityMatch rudis && rxConfReg == 0 ->
            AnEnable Nothing rxConfReg failT
        | linkT >= timeout (Proxy @dom) && idleMatch rudis ->
            LinkOk Nothing rxConfReg failT
        | otherwise -> IdleDetect (Just rudis) rxConfReg failT linkT
      LinkOk{}
        | abilityMatch rudis -> AnEnable Nothing rxConfReg failT
        | otherwise -> LinkOk (Just rudis) rxConfReg failT
 where
  rudis = maybe rudis' (rudis' <<+) rudi
   where
    rudis' = fromMaybe (repeat I) (_rudis self)
  rxConfReg = fromMaybe (_rxConfReg self) (toConfReg =<< rudi)
  failT = if syncStatus == Fail then _failT self + 1 else 0
  linkT = _linkT self + 1

-- | Output function for 'autoNeg' as defined in IEEE 802.3 Clause 37. Returns
--   the new value for 'Xmit' and 'ConfReg' for 'Sgmii.pcsTransmit'.
--
--   __TODO__: The state diagram shows that in the state @ABILITY_DETECT@ the
--   acknowledge bit should be set to zero. However, if this is done, the PHY
--   does not always (~50% of the time) exit auto-negotiation mode, which means
--   that no data can be transmitted. This can be resolved by resetting the PCS
--   receive. The documentation for SGMII, specifically Table 1, shows that the
--   acknowledge bit of the configuration register is always asserted, that is
--   why here the decision has been made to remove this deassertion to zero in
--   @ABILITY_DETECT@. Now the PHY does correctly exit auto-negotiation mode,
--   and the configuration register will be transmitted correctly. However, due
--   to the lack of a description of specific changes in the documentation, it
--   is not clear whether this is indeed the correct solution, and it should be
--   investigated further.
autoNegO ::
  forall dom.
  (KnownDomain dom) =>
  -- | Current state
  AutoNegState dom ->
  -- | New outputs
  (AutoNegState dom, Maybe Xmit, Maybe ConfReg)
autoNegO self = case self of
  AnEnable{} -> (self, Just Conf, Just 0)
  AnRestart{} -> (self, Nothing, Just 0)
  -- According to IEEE 802.3 this should have the acknowledge bit deasserted,
  -- but for SGMII the acknowledge bit is always asserted
  AbilityDetect{} -> (self, Nothing, Just txConfReg)
  AckDetect{} -> (self, Nothing, Just txConfReg)
  CompleteAck{} -> (self, Nothing, Nothing)
  IdleDetect{} -> (self, Just Idle, Nothing)
  LinkOk{} -> (self, Just Data, Nothing)
 where
  txConfReg = 0b0100000000000001

-- | Function that implements the auto-negotiation block as defined in IEEE
--   802.3 Clause 37, but modified to comply to the SGMII standard. This
--   modification is the decrease of 'Timeout' from 10 ms to 1.6 ms, and the
--   fact that SGMII always requires the acknowledge bit to be asserted. SGMII
--   also uses a different layout of the configuration register, but this does
--   not affect the state machine as the acknowledge bit is in the same
--   location.
--
--   __N.B.__: This function does not implement the optional Next Page function.
autoNeg ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  -- | Current 'SyncStatus' from 'Sgmii.sync'
  Signal dom SyncStatus ->
  -- | A new value of 'Rudi' from 'Sgmii.pcsReceive'
  Signal dom (Maybe Rudi) ->
  -- | Tuple containing the new value for 'Xmit' and a new 'ConfReg'
  (Signal dom (Maybe Xmit), Signal dom (Maybe ConfReg))
autoNeg syncStatus rudi = (xmit, txConfReg)
 where
  (_, xmit, txConfReg) =
    mooreB
      (autoNegT @dom)
      (autoNegO @dom)
      (AnEnable Nothing 0 0)
      (syncStatus, rudi)

{-# CLASH_OPAQUE autoNeg #-}
