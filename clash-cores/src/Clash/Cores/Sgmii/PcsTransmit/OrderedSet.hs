{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Sgmii.PcsTransmit.OrderedSet where

import Clash.Cores.Sgmii.Common
import Clash.Prelude
import Data.Maybe (fromJust, fromMaybe, isJust)

-- | State type of 'orderedSetT' as defined in IEEE 802.3 Clause 36, with the
--   exeception of @TX_TEST_XMIT@, @TX_PACKET@ and @ALIGN_ERR_START@ as these
--   states do not transmit an ordered set to the code group process
data OrderedSetState
  = Configuration {_xmit :: Xmit}
  | IdleS {_xmit :: Xmit}
  | XmitData {_xmit :: Xmit}
  | StartOfPacket {_xmit :: Xmit}
  | TxData {_xmit :: Xmit}
  | EndOfPacketNoExt {_xmit :: Xmit}
  | Epd2NoExt {_xmit :: Xmit}
  | Epd3 {_xmit :: Xmit}
  | EndOfPacketExt {_xmit :: Xmit}
  | ExtendBy1 {_xmit :: Xmit}
  | CarrierExtend {_xmit :: Xmit}
  | StartError {_xmit :: Xmit}
  | TxDataError {_xmit :: Xmit}
  deriving (Generic, NFDataX, Eq, Show)

-- | State transitions from @GENERATE_CODE_GROUPS@ from Figure 36-6
generateCg ::
  Bool -> Bool -> Xmit -> Xmit -> Even -> Bool -> Maybe OrderedSetState
generateCg txEn txEr xmit _xmit txEven cgSent = nextState
 where
  xmitChange = xmit /= _xmit && cgSent && txEven == Odd

  nextState
    | xmitChange && xmit == Conf = Just (Configuration xmit)
    | xmitChange && xmit == Idle = Just (IdleS xmit)
    | xmitChange && xmit == Data && txEn = Just (IdleS xmit)
    | xmitChange && xmit == Data && txEr = Just (IdleS xmit)
    | xmitChange && xmit == Data && not txEn && not txEr = Just (XmitData xmit)
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
  s = generateCg txEn txEr xmit' _xmit txEven cgSent

  nextState = fromMaybe self s

  out = (self, OSetC)
orderedSetT self@IdleS{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  s = generateCg txEn txEr xmit' _xmit txEven cgSent

  nextState
    | isJust s = fromJust s
    | xmit' == Data && cgSent && not txEn && not txEr = XmitData xmit'
    | otherwise = self

  out = (self, OSetI)
orderedSetT self@XmitData{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  s = generateCg txEn txEr xmit' _xmit txEven cgSent

  nextState
    | isJust s = fromJust s
    | txEn && not txEr && cgSent = StartOfPacket xmit'
    | txEn && txEr && cgSent = StartError xmit'
    | not txEn && cgSent = XmitData xmit'
    | otherwise = self

  out = (self, OSetI)
orderedSetT self@StartOfPacket{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  s = generateCg txEn txEr xmit' _xmit txEven cgSent

  nextState
    | isJust s = fromJust s
    | txEn && cgSent = TxData xmit'
    | not txEn && not txEr && cgSent = EndOfPacketNoExt xmit'
    | not txEn && txEr && cgSent = EndOfPacketExt xmit'
    | otherwise = self

  out = (self, OSetS)
orderedSetT self@TxData{..} (txEn, txEr, dw, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  s = generateCg txEn txEr xmit' _xmit txEven cgSent

  nextState
    | isJust s = fromJust s
    | txEn && cgSent = TxData xmit'
    | not txEn && not txEr && cgSent = EndOfPacketNoExt xmit'
    | not txEn && txEr && cgSent = EndOfPacketExt xmit'
    | otherwise = self

  txOSet = void OSetD txEn txEr dw

  out = (self, txOSet)
orderedSetT self@EndOfPacketNoExt{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  s = generateCg txEn txEr xmit' _xmit txEven cgSent

  nextState
    | isJust s = fromJust s
    | cgSent = Epd2NoExt xmit'
    | otherwise = self

  out = (self, OSetT)
orderedSetT self@Epd2NoExt{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  s = generateCg txEn txEr xmit' _xmit txEven cgSent

  nextState
    | isJust s = fromJust s
    | txEven == Odd && cgSent = XmitData xmit'
    | txEven == Even && cgSent = Epd3 xmit'
    | otherwise = self

  out = (self, OSetR)
orderedSetT self@Epd3{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  s = generateCg txEn txEr xmit' _xmit txEven cgSent

  nextState
    | isJust s = fromJust s
    | cgSent = XmitData xmit'
    | otherwise = self

  out = (self, OSetR)
orderedSetT self@EndOfPacketExt{..} (txEn, txEr, dw, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  s = generateCg txEn txEr xmit' _xmit txEven cgSent

  nextState
    | isJust s = fromJust s
    | not txEr && cgSent = ExtendBy1 xmit'
    | txEr && cgSent = CarrierExtend xmit'
    | otherwise = self

  txOSet = void OSetT txEn txEr dw

  out = (self, txOSet)
orderedSetT self@ExtendBy1{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  s = generateCg txEn txEr xmit' _xmit txEven cgSent

  nextState
    | isJust s = fromJust s
    | cgSent = Epd2NoExt xmit'
    | otherwise = self

  out = (self, OSetR)
orderedSetT self@CarrierExtend{..} (txEn, txEr, dw, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  s = generateCg txEn txEr xmit' _xmit txEven cgSent

  nextState
    | isJust s = fromJust s
    | not txEn && not txEr && cgSent = ExtendBy1 xmit'
    | txEn && txEr && cgSent = StartError xmit'
    | txEn && not txEr && cgSent = StartOfPacket xmit'
    | not txEn && txEr && cgSent = CarrierExtend xmit'
    | otherwise = self

  txOSet = void OSetR txEn txEr dw

  out = (self, txOSet)
orderedSetT self@StartError{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  s = generateCg txEn txEr xmit' _xmit txEven cgSent

  nextState
    | isJust s = fromJust s
    | cgSent = TxDataError xmit'
    | otherwise = self

  out = (self, OSetS)
orderedSetT self@TxDataError{..} (txEn, txEr, _, xmit, txEven, cgSent) =
  (nextState, out)
 where
  xmit' = fromMaybe _xmit xmit
  s = generateCg txEn txEr xmit' _xmit txEven cgSent

  nextState
    | isJust s = fromJust s
    | txEn && cgSent = TxData xmit'
    | not txEn && not txEr && cgSent = EndOfPacketNoExt xmit'
    | not txEn && txEr && cgSent = EndOfPacketExt xmit'
    | otherwise = self

  out = (self, OSetV)
