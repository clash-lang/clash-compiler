{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

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

-- | State transitions from @GENERATE_CODE_GROUPS@ from Figure 36-6
generateCg ::
  Bool -> Bool -> Xmit -> Even -> Bool -> Bool -> Maybe OrderedSetState
generateCg txEn txEr xmit txEven cgSent xmitChange = nextState
 where
  nextState
    | xmitChange && cgSent && txEven == Odd && xmit == Conf =
        Just (Configuration xmit False)
    | xmitChange && cgSent && txEven == Odd && xmit == Idle =
        Just (IdleS xmit False)
    | xmitChange && cgSent && txEven == Odd && xmit == Data && txEn =
        Just (IdleS xmit False)
    | xmitChange && cgSent && txEven == Odd && xmit == Data && txEr =
        Just (IdleS xmit False)
    | xmitChange
        && cgSent
        && txEven
        == Odd
        && xmit
        == Data
        && not txEn
        && not txEr =
        Just (XmitData xmit False)
    | otherwise = Nothing

-- | Void function that is used to check whether @/V/@ needs to be propagated
--   based on the values of the input pins
void :: OrderedSet -> Bool -> Bool -> BitVector 8 -> OrderedSet
void txOSet txEn txEr dw
  | not txEn && txEr && dw /= 0b00001111 = OSetV
  | txEn && txEr = OSetV
  | otherwise = txOSet

-- | State transition function for the states as defined in IEEE 802.3 Clause
--   36, specifically Figure 36-5. This function receives the input values and
--   generates an ordered set to be transmitted by the code group process.
orderedSetT ::
  -- | State variable
  OrderedSetState ->
  -- | The new input values, partly from the outside world, and partly from
  --   'autoNeg' and 'codeGroupT'
  (Bool, Bool, BitVector 8, Maybe Xmit, Even, Bool) ->
  -- | The new state and the new output values
  (OrderedSetState, (OrderedSetState, OrderedSet))
orderedSetT self@Configuration{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  xmitChange = (xmit' /= _xmit) || _xmitChange
  s = generateCg txEn txEr xmit' txEven cgSent xmitChange

  nextState = fromMaybe (Configuration xmit' xmitChange) s

  out = (self, OSetC)
orderedSetT self@IdleS{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  xmitChange = (xmit' /= _xmit) || _xmitChange
  s = generateCg txEn txEr xmit' txEven cgSent xmitChange

  nextState
    | isJust s = fromJust s
    | xmit' == Data && cgSent && not txEn && not txEr =
        XmitData xmit' xmitChange
    | otherwise = IdleS xmit' xmitChange

  out = (self, OSetI)
orderedSetT self@XmitData{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  xmitChange = (xmit' /= _xmit) || _xmitChange
  s = generateCg txEn txEr xmit' txEven cgSent xmitChange

  nextState
    | isJust s = fromJust s
    | txEn && not txEr && cgSent = StartOfPacket xmit' xmitChange
    | txEn && txEr && cgSent = StartError xmit' xmitChange
    | not txEn && cgSent = XmitData xmit' xmitChange
    | otherwise = XmitData xmit' xmitChange

  out = (self, OSetI)
orderedSetT self@StartOfPacket{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  xmitChange = (xmit' /= _xmit) || _xmitChange
  s = generateCg txEn txEr xmit' txEven cgSent xmitChange

  nextState
    | isJust s = fromJust s
    | txEn && cgSent = TxData xmit' xmitChange
    | not txEn && not txEr && cgSent = EndOfPacketNoExt xmit' xmitChange
    | not txEn && txEr && cgSent = EndOfPacketExt xmit' xmitChange
    | otherwise = StartOfPacket xmit' xmitChange

  out = (self, OSetS)
orderedSetT self@TxData{..} (txEn, txEr, dw, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  xmitChange = (xmit' /= _xmit) || _xmitChange
  s = generateCg txEn txEr xmit' txEven cgSent xmitChange

  nextState
    | isJust s = fromJust s
    | txEn && cgSent = TxData xmit' xmitChange
    | not txEn && not txEr && cgSent = EndOfPacketNoExt xmit' xmitChange
    | not txEn && txEr && cgSent = EndOfPacketExt xmit' xmitChange
    | otherwise = TxData xmit' xmitChange

  txOSet = void OSetD txEn txEr dw

  out = (self, txOSet)
orderedSetT self@EndOfPacketNoExt{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  xmitChange = (xmit' /= _xmit) || _xmitChange
  s = generateCg txEn txEr xmit' txEven cgSent xmitChange

  nextState
    | isJust s = fromJust s
    | cgSent = Epd2NoExt xmit' xmitChange
    | otherwise = EndOfPacketNoExt xmit' xmitChange

  out = (self, OSetT)
orderedSetT self@Epd2NoExt{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  xmitChange = (xmit' /= _xmit) || _xmitChange
  s = generateCg txEn txEr xmit' txEven cgSent xmitChange

  nextState
    | isJust s = fromJust s
    | txEven == Odd && cgSent = XmitData xmit' xmitChange
    | txEven == Even && cgSent = Epd3 xmit' xmitChange
    | otherwise = Epd2NoExt xmit' xmitChange

  out = (self, OSetR)
orderedSetT self@Epd3{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  xmitChange = (xmit' /= _xmit) || _xmitChange
  s = generateCg txEn txEr xmit' txEven cgSent xmitChange

  nextState
    | isJust s = fromJust s
    | cgSent = XmitData xmit' xmitChange
    | otherwise = Epd3 xmit' xmitChange

  out = (self, OSetR)
orderedSetT self@EndOfPacketExt{..} (txEn, txEr, dw, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  xmitChange = (xmit' /= _xmit) || _xmitChange
  s = generateCg txEn txEr xmit' txEven cgSent xmitChange

  nextState
    | isJust s = fromJust s
    | not txEr && cgSent = ExtendBy1 xmit' xmitChange
    | txEr && cgSent = CarrierExtend xmit' xmitChange
    | otherwise = EndOfPacketExt xmit' xmitChange

  txOSet = void OSetT txEn txEr dw

  out = (self, txOSet)
orderedSetT self@ExtendBy1{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  xmitChange = (xmit' /= _xmit) || _xmitChange
  s = generateCg txEn txEr xmit' txEven cgSent xmitChange

  nextState
    | isJust s = fromJust s
    | cgSent = Epd2NoExt xmit' xmitChange
    | otherwise = ExtendBy1 xmit' xmitChange

  out = (self, OSetR)
orderedSetT self@CarrierExtend{..} (txEn, txEr, dw, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  xmitChange = (xmit' /= _xmit) || _xmitChange
  s = generateCg txEn txEr xmit' txEven cgSent xmitChange

  nextState
    | isJust s = fromJust s
    | not txEn && not txEr && cgSent = ExtendBy1 xmit' xmitChange
    | txEn && txEr && cgSent = StartError xmit' xmitChange
    | txEn && not txEr && cgSent = StartOfPacket xmit' xmitChange
    | not txEn && txEr && cgSent = CarrierExtend xmit' xmitChange
    | otherwise = CarrierExtend xmit' xmitChange

  txOSet = void OSetR txEn txEr dw

  out = (self, txOSet)
orderedSetT self@StartError{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  xmitChange = (xmit' /= _xmit) || _xmitChange
  s = generateCg txEn txEr xmit' txEven cgSent xmitChange

  nextState
    | isJust s = fromJust s
    | cgSent = TxDataError xmit' xmitChange
    | otherwise = StartError xmit' xmitChange

  out = (self, OSetS)
orderedSetT self@TxDataError{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  xmitChange = (xmit' /= _xmit) || _xmitChange
  s = generateCg txEn txEr xmit' txEven cgSent xmitChange

  nextState
    | isJust s = fromJust s
    | txEn && cgSent = TxData xmit' xmitChange
    | not txEn && not txEr && cgSent = EndOfPacketNoExt xmit' xmitChange
    | not txEn && txEr && cgSent = EndOfPacketExt xmit' xmitChange
    | otherwise = TxDataError xmit' xmitChange

  out = (self, OSetV)

{-# CLASH_OPAQUE orderedSetT #-}
