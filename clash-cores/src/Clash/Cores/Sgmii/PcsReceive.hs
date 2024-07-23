{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--   Copyright   :  (C) 2024, QBayLogic B.V.
--   License     :  BSD2 (see the file LICENSE)
--   Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
--
--   PCS receive process, as defined in IEEE 802.3 Figure 36-7a and 36-7b
module Clash.Cores.Sgmii.PcsReceive where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.Common
import Clash.Prelude
import Data.Maybe (fromJust, fromMaybe, isJust)

-- | Defines all possible valid 'checkEnd' results
data CheckEnd
  = K28_5DK28_5
  | K28_5D21_5D00_0
  | K28_5D02_2D00_0
  | TRK28_5
  | TRR
  | RRR
  | RRK28_5
  | RRS
  deriving (Eq, Show)

-- | State type of 'pcsReceive'. This contains all states as they are defined in
--   IEEE 802.3 Clause 36, with with exeception of the states @CARRIER_DETECT@,
--   @RECEIVE@ and @EPD2_CHECK_END@ as these do not act upon a received code
--   group. The transitions of these states are embedded in the states that
--   usually transition to either of these states.
data PcsReceiveState
  = WaitForK {_rx :: Bool, _xmit :: Xmit}
  | RxK {_rx :: Bool, _xmit :: Xmit}
  | RxCB {_rx :: Bool, _xmit :: Xmit}
  | RxCC {_rx :: Bool, _xmit :: Xmit, _hist :: Symbol8b10b}
  | RxCD
      { _rx :: Bool
      , _xmit :: Xmit
      , _hist :: Symbol8b10b
      , _rxConfReg :: ConfReg
      }
  | RxInvalid {_rx :: Bool, _xmit :: Xmit}
  | IdleD {_rx :: Bool, _xmit :: Xmit}
  | FalseCarrier {_rx :: Bool, _xmit :: Xmit}
  | StartOfPacket {_rx :: Bool, _xmit :: Xmit}
  | EarlyEnd {_rx :: Bool, _xmit :: Xmit}
  | TriRri {_rx :: Bool, _xmit :: Xmit}
  | TrrExtend {_rx :: Bool, _xmit :: Xmit}
  | PacketBurstRrs {_rx :: Bool, _xmit :: Xmit}
  | ExtendErr {_rx :: Bool, _xmit :: Xmit}
  | EarlyEndExt {_rx :: Bool, _xmit :: Xmit}
  | RxData {_rx :: Bool, _xmit :: Xmit, _hist :: Symbol8b10b}
  | RxDataError {_rx :: Bool, _xmit :: Xmit, _hist :: Symbol8b10b}
  | LinkFailed {_rx :: Bool, _xmit :: Xmit}
  deriving (Generic, NFDataX, Eq, Show)

-- | Calculate the number of bits that are different in two code groups. For
--   example: the code groups @0b0000@ and @0b0001@ have a difference of 1.
bitDiff ::
  (KnownNat n) =>
  -- | First code group
  BitVector n ->
  -- | Second code group
  BitVector n ->
  -- | Bit difference
  Index (n + 1)
bitDiff cg0 cg1 = foldl f 0 $ map bitCoerce $ bv2v $ xor cg0 cg1
 where
  f a b = if b then a + 1 else a

-- | Take the running disparity, the 'Even' signal and the current data word
--   and determine whether there is a 2-bit or more difference between the code
--   group and both alternative representations of the K28.5 control word or a
--   difference of between 2 and 9 bits between the code group and the expected
--   encoding of the K28.5 control word
carrierDetect ::
  -- | Code group
  Cg ->
  -- | Running disparity
  Bool ->
  -- | 'Even' signal
  Even ->
  -- | The 'carrierDetect' condition
  Bool
carrierDetect cg rd rxEven
  | rxEven /= Even = False
  | bitDiff cgK28_5N cg >= 2 && bitDiff cgK28_5P cg >= 2 = True
  | bitDiff cgK28_5 cg >= 2 && bitDiff cgK28_5 cg <= 9 = True
  | otherwise = False
 where
  cgK28_5 = if rd then cgK28_5P else cgK28_5N

-- | Take the running disparity, the current and next two input data words and
--   check whether they correspond to one of the specified end conditions
checkEnd ::
  -- | Current and next 2 data words
  Vec 3 Symbol8b10b ->
  -- | End condition
  Maybe CheckEnd
checkEnd dws
  | dws == cwK28_5 :> dws !! (1 :: Index 3) :> cwK28_5 :> Nil = Just K28_5DK28_5
  | dws == cwK28_5 :> dwD21_5 :> dwD00_0 :> Nil = Just K28_5D21_5D00_0
  | dws == cwK28_5 :> dwD02_2 :> dwD00_0 :> Nil = Just K28_5D02_2D00_0
  | dws == cwT :> cwR :> cwK28_5 :> Nil = Just TRK28_5
  | dws == cwT :> Nil ++ repeat cwR = Just TRR
  | dws == repeat cwR = Just RRR
  | dws == repeat cwR ++ cwK28_5 :> Nil = Just RRK28_5
  | dws == repeat cwR ++ cwS :> Nil = Just RRS
  | otherwise = Nothing

-- | Function that implements the transitions of the @EPD2_CHECK_END@ state
epd2CheckEnd ::
  Vec 3 Symbol8b10b -> Even -> Bool -> Xmit -> Maybe PcsReceiveState
epd2CheckEnd dws rxEven rx xmit
  | rxEnd == Just RRR = Just (TrrExtend rx xmit)
  | rxEnd == Just RRK28_5 && rxEven == Even = Just (TriRri rx xmit)
  | rxEnd == Just RRS = Just (PacketBurstRrs rx xmit)
  | otherwise = Nothing
 where
  rxEnd = checkEnd dws

-- | Function that implements the transitions of the @RECEIVE@ state
receive :: Vec 3 Symbol8b10b -> Even -> Bool -> Xmit -> Maybe PcsReceiveState
receive dws rxEven rx xmit
  | rxEnd == Just K28_5DK28_5 && rxEven == Even = Just (EarlyEnd rx xmit)
  | rxEnd == Just K28_5D21_5D00_0 && rxEven == Even = Just (EarlyEnd rx xmit)
  | rxEnd == Just K28_5D02_2D00_0 && rxEven == Even = Just (EarlyEnd rx xmit)
  | rxEnd == Just TRK28_5 && rxEven == Even = Just (TriRri rx xmit)
  | rxEnd == Just TRR = Just (TrrExtend rx xmit)
  | rxEnd == Just RRR = Just (EarlyEnd rx xmit)
  | isDw (head dws) = Just (RxData rx xmit dw)
  | otherwise = Nothing
 where
  rxEnd = checkEnd dws
  dw = head dws

-- | State transition function for 'pcsReceive'. Takes the state as defined in
--   'PcsReceiveState' and returns the next state as defined in Clause 36 of
--   IEEE 802.3. In contrast to the specification in Clause 36, here
--   'Sgmii.syncT' is responsible for decoding the code groups instead of this
--   function, to not duplicate any work, but as this function does need to
--   determine the difference in bits ('bitDifference') the code group is set as
--   an input value as well.
--
--   __N.B.__: This function does not implement the optional EEE
--   (Energy-Efficient Ethernet) capability.
pcsReceiveT ::
  -- | Current state
  PcsReceiveState ->
  -- | Input values, where @Vec 3 CodeGroup@ contains the current and next two
  -- | data words
  (Cg, Bool, Vec 3 Symbol8b10b, Even, SyncStatus, Maybe Xmit) ->
  -- | New state
  PcsReceiveState
pcsReceiveT WaitForK{..} (_, _, dws, rxEven, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed False xmit'
  | head dws == cwK28_5 && rxEven == Even = RxK False xmit'
  | otherwise = WaitForK _rx xmit'
 where
  xmit' = fromMaybe _xmit xmit
pcsReceiveT RxK{..} (_, _, dws, _, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed False xmit'
  | dw == dwD21_5 = RxCB False xmit'
  | dw == dwD02_2 = RxCB False xmit'
  | not (isDw dw) && xmit' /= Data = RxInvalid False xmit'
  | xmit' /= Data && isDw dw = IdleD False xmit'
  | xmit' == Data = IdleD False xmit'
  | otherwise = RxK _rx xmit'
 where
  xmit' = fromMaybe _xmit xmit
  dw = head dws
pcsReceiveT RxCB{..} (_, _, dws, _, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed _rx xmit'
  | isDw dw = RxCC _rx xmit' dw
  | otherwise = RxInvalid _rx xmit'
 where
  xmit' = fromMaybe _xmit xmit
  dw = head dws
pcsReceiveT RxCC{..} (_, _, dws, _, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed _rx xmit'
  | isDw dw = RxCD _rx xmit' dw $ resize $ fromSymbol _hist
  | otherwise = RxInvalid _rx xmit'
 where
  xmit' = fromMaybe _xmit xmit
  dw = head dws
pcsReceiveT RxCD{..} (_, _, dws, rxEven, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed _rx xmit'
  | dw == cwK28_5 && rxEven == Even = RxK _rx xmit'
  | dw /= cwK28_5 = RxInvalid _rx xmit'
  | rxEven == Odd = RxInvalid _rx xmit'
  | otherwise = RxCD _rx xmit' _hist _rxConfReg
 where
  xmit' = fromMaybe _xmit xmit
  dw = head dws
pcsReceiveT RxInvalid{..} (_, _, dws, rxEven, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed rx xmit'
  | dw == cwK28_5 && rxEven == Even = RxK rx xmit'
  | dw /= cwK28_5 && rxEven == Even = WaitForK rx xmit'
  | otherwise = RxInvalid _rx xmit'
 where
  rx = xmit' == Data || _rx
  xmit' = fromMaybe _xmit xmit
  dw = head dws
pcsReceiveT IdleD{..} (cg, rd, dws, rxEven, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed False xmit'
  | dw /= cwK28_5 && xmit' /= Data = RxInvalid False xmit'
  | carrierDetected && xmit' == Data && dw /= cwS = FalseCarrier False xmit'
  | carrierDetected && xmit' == Data && dw == cwS = StartOfPacket False xmit'
  | otherwise = RxK False xmit'
 where
  carrierDetected = carrierDetect cg rd rxEven
  xmit' = fromMaybe _xmit xmit
  dw = head dws
pcsReceiveT FalseCarrier{..} (_, _, dws, rxEven, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed True xmit'
  | dw == cwK28_5 && rxEven == Even = RxK True xmit'
  | otherwise = FalseCarrier _rx xmit'
 where
  xmit' = fromMaybe _xmit xmit
  dw = head dws
pcsReceiveT EarlyEnd{..} (_, _, dws, _, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed _rx xmit'
  | dw == dwD02_2 = RxCB _rx xmit'
  | dw == dwD21_5 = RxCB _rx xmit'
  | otherwise = IdleD _rx xmit'
 where
  xmit' = fromMaybe _xmit xmit
  dw = head dws
pcsReceiveT TriRri{..} (_, _, dws, _, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed False xmit'
  | head dws == cwK28_5 = RxK False xmit'
  | otherwise = TriRri _rx xmit'
 where
  xmit' = fromMaybe _xmit xmit
pcsReceiveT PacketBurstRrs{..} (_, _, dws, _, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed _rx xmit'
  | head dws == cwS = StartOfPacket _rx xmit'
  | otherwise = PacketBurstRrs _rx xmit'
 where
  xmit' = fromMaybe _xmit xmit
pcsReceiveT ExtendErr{..} (_, _, dws, rxEven, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed _rx xmit'
  | dw == cwS = StartOfPacket _rx xmit'
  | dw == cwK28_5 && rxEven == Even = RxK _rx xmit'
  | isJust s && rxEven == Even = fromJust s
  | otherwise = ExtendErr _rx xmit'
 where
  s = epd2CheckEnd dws rxEven _rx xmit'
  xmit' = fromMaybe _xmit xmit
  dw = head dws
pcsReceiveT LinkFailed{..} (_, _, _, _, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed False xmit'
  | otherwise = WaitForK False xmit'
 where
  xmit' = fromMaybe _xmit xmit
pcsReceiveT self (_, _, dws, rxEven, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed self._rx xmit'
  | isJust s1 = fromJust s1
  | otherwise = s2
 where
  (s1, s2) = case self of
    TrrExtend{} ->
      (epd2CheckEnd dws rxEven self._rx xmit', ExtendErr self._rx xmit')
    EarlyEndExt{} ->
      (epd2CheckEnd dws rxEven self._rx xmit', ExtendErr self._rx xmit')
    _ -> (receive dws rxEven self._rx xmit', RxDataError self._rx xmit' dw)

  xmit' = fromMaybe self._xmit xmit
  dw = head dws

-- | Output function for 'pcsReceive', that sets the outputs as defined in IEEE
--   802.3 Clause 36.
pcsReceiveO ::
  -- | Current state
  PcsReceiveState ->
  -- | New output values
  ( PcsReceiveState
  , Maybe Bool
  , Maybe Bool
  , Maybe Symbol8b10b
  , Maybe Rudi
  , Maybe ConfReg
  )
pcsReceiveO self = case self of
  WaitForK{} -> (self, Just False, Just False, Nothing, Nothing, Nothing)
  RxK{} -> (self, Just False, Just False, Nothing, Nothing, Nothing)
  RxCB{} -> (self, Just False, Just False, Nothing, Nothing, Nothing)
  RxCD{} -> (self, Nothing, Nothing, Nothing, Just C, Just rxConfReg)
  RxInvalid{} -> (self, Nothing, Nothing, Nothing, rudi1, Nothing)
  IdleD{} -> (self, Just False, Just False, Nothing, Just I, Nothing)
  FalseCarrier{} ->
    (self, Nothing, Just True, Just (Cw 0b00001110), Nothing, Nothing)
  StartOfPacket{} ->
    (self, Just True, Just False, Just (Cw 0b01010101), Nothing, Nothing)
  EarlyEnd{} -> (self, Nothing, Just True, Nothing, Nothing, Nothing)
  TriRri{} -> (self, Just False, Just False, Nothing, Nothing, Nothing)
  TrrExtend{} ->
    (self, Just False, Just True, Just (Cw 0b00001111), Nothing, Nothing)
  PacketBurstRrs{} ->
    (self, Just False, Nothing, Just (Cw 0b00001111), Nothing, Nothing)
  ExtendErr{} ->
    (self, Just False, Nothing, Just (Cw 0b00011111), Nothing, Nothing)
  EarlyEndExt{} -> (self, Nothing, Just True, Nothing, Nothing, Nothing)
  RxData{} -> (self, Nothing, Just False, Just self._hist, Nothing, Nothing)
  RxDataError{} -> (self, Nothing, Just True, Just self._hist, Nothing, Nothing)
  LinkFailed{} -> (self, rxDv, Just self._rx, Nothing, rudi2, Nothing)
  _ -> (self, Nothing, Nothing, Nothing, Nothing, Nothing)
 where
  rxConfReg = (fromSymbol self._hist ++# 0) .|. self._rxConfReg
  rudi1 = if self._xmit == Conf then Just Invalid else Nothing
  rudi2 = if self._xmit /= Data then Just Invalid else Nothing
  rxDv = if self._rx then Nothing else Just False

-- | The 'pcsReceive' block. Takes a tuple with the new input code group,
--   running disparity and data word, 'Even', 'SyncStatus' and 'Xmit' signals
--   and runs the transition function 'pcsReceiveT'. The outputs are a set of
--   'Maybe' values.
pcsReceive ::
  (HiddenClockResetEnable dom) =>
  -- | Current code group from 'Sgmii.sync'
  Signal dom Cg ->
  -- | Current running disparity from 'Sgmii.sync'
  Signal dom Bool ->
  -- | Input 'Symbol8b10b' from 'Sgmii.sync'
  Signal dom (Vec 3 Symbol8b10b) ->
  -- | The 'Even' value from 'Sgmii.sync'
  Signal dom Even ->
  -- | The current 'SyncStatus' from 'Sgmii.sync'
  Signal dom SyncStatus ->
  -- | The 'Xmit' signal from 'Sgmii.autoNeg'
  Signal dom (Maybe Xmit) ->
  -- | Tuple containing the output values
  ( Signal dom (Maybe Bool)
  , Signal dom (Maybe Bool)
  , Signal dom (Maybe Symbol8b10b)
  , Signal dom (Maybe Rudi)
  , Signal dom (Maybe ConfReg)
  )
pcsReceive cg rd dw1 rxEven syncStatus xmit = (rxDv, rxEr, dw2, rudi, rxConfReg)
 where
  (_, rxDv, rxEr, dw2, rudi, rxConfReg) =
    mooreB
      pcsReceiveT
      pcsReceiveO
      (WaitForK False Idle)
      (cg, rd, dw1, rxEven, syncStatus, xmit)

{-# CLASH_OPAQUE pcsReceive #-}
