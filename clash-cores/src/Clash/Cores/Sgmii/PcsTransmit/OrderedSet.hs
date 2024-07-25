{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- |
--   Copyright   :  (C) 2024, QBayLogic B.V.
--   License     :  BSD2 (see the file LICENSE)
--   Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
--
--   Ordered set process of the PCS transmit block, as defined in IEEE 802.3
--   Figure 36-5
module Clash.Cores.Sgmii.PcsTransmit.OrderedSet where

import Clash.Cores.Sgmii.Common
import Clash.Prelude
import Data.Maybe (fromJust, fromMaybe, isJust)

-- | State type of 'orderedSetT' as defined in IEEE 802.3 Clause 36, with the
--   exeception of @TX_TEST_XMIT@, @TX_PACKET@ and @ALIGN_ERR_START@ as these
--   states do not transmit an ordered set to the code group process
data OrderedSetState
  = Configuration {_xmit :: Xmit, _xmitChange :: Bool}
  | IdleS {_xmit :: Xmit, _xmitChange :: Bool}
  | XmitData {_xmit :: Xmit, _xmitChange :: Bool}
  | StartOfPacket {_xmit :: Xmit, _xmitChange :: Bool}
  | TxData {_xmit :: Xmit, _xmitChange :: Bool}
  | EndOfPacketNoExt {_xmit :: Xmit, _xmitChange :: Bool}
  | Epd2NoExt {_xmit :: Xmit, _xmitChange :: Bool}
  | Epd3 {_xmit :: Xmit, _xmitChange :: Bool}
  | EndOfPacketExt {_xmit :: Xmit, _xmitChange :: Bool}
  | ExtendBy1 {_xmit :: Xmit, _xmitChange :: Bool}
  | CarrierExtend {_xmit :: Xmit, _xmitChange :: Bool}
  | StartError {_xmit :: Xmit, _xmitChange :: Bool}
  | TxDataError {_xmit :: Xmit, _xmitChange :: Bool}
  deriving (Generic, NFDataX, Eq, Show)

-- | State transitions from @TX_TEST_XMIT@ from Figure 36-5, which need to be
--   set in all parent states of @TX_TEST_XMIT@ as this state itself is not
--   implemented as it does not transmit a code group
txTestXmit ::
  Bool -> Bool -> Xmit -> Even -> Bool -> Bool -> Maybe OrderedSetState
txTestXmit txEn txEr xmit txEven tx xmitChange
  | not (xmitChange && tx && txEven == Odd) = Nothing
  | xmit == Conf = Just (Configuration xmit False)
  | xmit == Idle = Just (IdleS xmit False)
  | xmit == Data && txEn = Just (IdleS xmit False)
  | xmit == Data && txEr = Just (IdleS xmit False)
  | otherwise = Just (XmitData xmit False)

-- | Void function that is used to check whether @/V/@ needs to be propagated
--   based on the values of the input pins
void :: OrderedSet -> Bool -> Bool -> BitVector 8 -> OrderedSet
void txOSet txEn txEr dw
  | not txEn && txEr && dw /= 0b00001111 = OSetV
  | txEn && txEr = OSetV
  | otherwise = txOSet

-- | Function to update the current values for 'Xmit' and @xmitChange@
xmitUpdate :: OrderedSetState -> Maybe Xmit -> (Xmit, Bool)
xmitUpdate s xmit = (xmit', xmitChange)
 where
  xmit' = fromMaybe s._xmit xmit
  xmitChange = (xmit' /= s._xmit) || s._xmitChange

-- | State transition function for the states as defined in IEEE 802.3 Clause
--   36, specifically Figure 36-5. This function receives the input values and
--   generates an ordered set to be transmitted by the code group process.
--
--   __N.B.__: This function does not implement the optional EEE
--   (Energy-Efficient Ethernet) capability.
orderedSetT ::
  -- | State variable
  OrderedSetState ->
  -- | The new input values, partly from the outside world, and partly from
  --   'Sgmii.autoNeg' and 'PcsTransmit.codeGroupT'
  (Bool, Bool, BitVector 8, Maybe Xmit, Even, Bool) ->
  -- | The new state and the new output values
  (OrderedSetState, (OrderedSetState, OrderedSet))
orderedSetT self@Configuration{} (txEn, txEr, _, xmit, txEven, tx) =
  (nextState, out)
 where
  nextState = fromMaybe (Configuration xmit' xmitChange) s

  (xmit', xmitChange) = xmitUpdate self xmit
  s = txTestXmit txEn txEr xmit' txEven tx xmitChange
  out = (self, OSetC)
orderedSetT self@IdleS{} (txEn, txEr, _, xmit, txEven, tx) = (nextState, out)
 where
  nextState
    | isJust s = fromJust s
    | xmit' == Data && not txEn && not txEr && tx = XmitData xmit' xmitChange
    | otherwise = IdleS xmit' xmitChange

  (xmit', xmitChange) = xmitUpdate self xmit
  s = txTestXmit txEn txEr xmit' txEven tx xmitChange
  out = (self, OSetI)
orderedSetT self@XmitData{} (txEn, txEr, _, xmit, txEven, tx) = (nextState, out)
 where
  nextState
    | isJust s = fromJust s
    | txEn && not txEr && tx = StartOfPacket xmit' xmitChange
    | txEn && txEr && tx = StartError xmit' xmitChange
    | otherwise = XmitData xmit' xmitChange

  (xmit', xmitChange) = xmitUpdate self xmit
  s = txTestXmit txEn txEr xmit' txEven tx xmitChange
  out = (self, OSetI)
orderedSetT self@StartOfPacket{} (txEn, txEr, _, xmit, txEven, tx) =
  (nextState, out)
 where
  nextState
    | isJust s = fromJust s
    | txEn && tx = TxData xmit' xmitChange
    | not txEn && not txEr && tx = EndOfPacketNoExt xmit' xmitChange
    | not txEn && txEr && tx = EndOfPacketExt xmit' xmitChange
    | otherwise = StartOfPacket xmit' xmitChange

  (xmit', xmitChange) = xmitUpdate self xmit
  s = txTestXmit txEn txEr xmit' txEven tx xmitChange
  out = (self, OSetS)
orderedSetT self@TxData{} (txEn, txEr, dw, xmit, txEven, tx) = (nextState, out)
 where
  nextState
    | isJust s = fromJust s
    | txEn && tx = TxData xmit' xmitChange
    | not txEn && not txEr && tx = EndOfPacketNoExt xmit' xmitChange
    | not txEn && txEr && tx = EndOfPacketExt xmit' xmitChange
    | otherwise = TxData xmit' xmitChange

  (xmit', xmitChange) = xmitUpdate self xmit
  s = txTestXmit txEn txEr xmit' txEven tx xmitChange
  txOSet = void OSetD txEn txEr dw
  out = (self, txOSet)
orderedSetT self@EndOfPacketNoExt{} (txEn, txEr, _, xmit, txEven, tx) =
  (nextState, out)
 where
  nextState
    | isJust s = fromJust s
    | tx = Epd2NoExt xmit' xmitChange
    | otherwise = EndOfPacketNoExt xmit' xmitChange

  (xmit', xmitChange) = xmitUpdate self xmit
  s = txTestXmit txEn txEr xmit' txEven tx xmitChange
  out = (self, OSetT)
orderedSetT self@Epd2NoExt{} (txEn, txEr, _, xmit, txEven, tx) =
  (nextState, out)
 where
  nextState
    | isJust s = fromJust s
    | txEven == Odd && tx = XmitData xmit' xmitChange
    | txEven == Even && tx = Epd3 xmit' xmitChange
    | otherwise = Epd2NoExt xmit' xmitChange

  (xmit', xmitChange) = xmitUpdate self xmit
  s = txTestXmit txEn txEr xmit' txEven tx xmitChange
  out = (self, OSetR)
orderedSetT self@Epd3{} (txEn, txEr, _, xmit, txEven, tx) = (nextState, out)
 where
  nextState
    | isJust s = fromJust s
    | tx = XmitData xmit' xmitChange
    | otherwise = Epd3 xmit' xmitChange

  (xmit', xmitChange) = xmitUpdate self xmit
  s = txTestXmit txEn txEr xmit' txEven tx xmitChange
  out = (self, OSetR)
orderedSetT self@EndOfPacketExt{} (txEn, txEr, dw, xmit, txEven, tx) =
  (nextState, out)
 where
  nextState
    | isJust s = fromJust s
    | not txEr && tx = ExtendBy1 xmit' xmitChange
    | txEr && tx = CarrierExtend xmit' xmitChange
    | otherwise = EndOfPacketExt xmit' xmitChange

  (xmit', xmitChange) = xmitUpdate self xmit
  s = txTestXmit txEn txEr xmit' txEven tx xmitChange
  txOSet = void OSetT txEn txEr dw
  out = (self, txOSet)
orderedSetT self@ExtendBy1{} (txEn, txEr, _, xmit, txEven, tx) =
  (nextState, out)
 where
  nextState
    | isJust s = fromJust s
    | tx = Epd2NoExt xmit' xmitChange
    | otherwise = ExtendBy1 xmit' xmitChange

  (xmit', xmitChange) = xmitUpdate self xmit
  s = txTestXmit txEn txEr xmit' txEven tx xmitChange
  out = (self, OSetR)
orderedSetT self@CarrierExtend{} (txEn, txEr, dw, xmit, txEven, tx) =
  (nextState, out)
 where
  nextState
    | isJust s = fromJust s
    | not txEn && not txEr && tx = ExtendBy1 xmit' xmitChange
    | txEn && txEr && tx = StartError xmit' xmitChange
    | txEn && not txEr && tx = StartOfPacket xmit' xmitChange
    | otherwise = CarrierExtend xmit' xmitChange

  (xmit', xmitChange) = xmitUpdate self xmit
  s = txTestXmit txEn txEr xmit' txEven tx xmitChange
  txOSet = void OSetR txEn txEr dw
  out = (self, txOSet)
orderedSetT self@StartError{} (txEn, txEr, _, xmit, txEven, tx) =
  (nextState, out)
 where
  nextState
    | isJust s = fromJust s
    | tx = TxDataError xmit' xmitChange
    | otherwise = StartError xmit' xmitChange

  (xmit', xmitChange) = xmitUpdate self xmit
  s = txTestXmit txEn txEr xmit' txEven tx xmitChange
  out = (self, OSetS)
orderedSetT self@TxDataError{} (txEn, txEr, _, xmit, txEven, tx) =
  (nextState, out)
 where
  nextState
    | isJust s = fromJust s
    | txEn && tx = TxData xmit' xmitChange
    | not txEn && not txEr && tx = EndOfPacketNoExt xmit' xmitChange
    | not txEn && txEr && tx = EndOfPacketExt xmit' xmitChange
    | otherwise = TxDataError xmit' xmitChange

  (xmit', xmitChange) = xmitUpdate self xmit
  s = txTestXmit txEn txEr xmit' txEven tx xmitChange
  out = (self, OSetV)

{-# CLASH_OPAQUE orderedSetT #-}
