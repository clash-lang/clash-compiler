{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Sgmii.PcsReceive where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.Common
import Clash.Prelude

-- | Defines all possible valid 'checkEnd' results, as well as @None@ for no
--   valid result
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
--   IEEE 802.3 Clause 36, with the addition of two initialization states that
--   are used to fill the @cw@ input shift register and the exeception of the
--   states @CARRIER_DETECT@, @RECEIVE@ and @EPD2_CHECK_END@ as these do not act
--   upon a received code group. The transitions of these states are embedded in
--   the states that usually transition to either of these states.
data PcsReceiveState
  = Init1
  | Init2
  | WaitForK {_rx :: Bool}
  | RxK {_rx :: Bool}
  | RxCB {_rx :: Bool}
  | RxCC {_rx :: Bool, _hist :: DataWord}
  | RxCD {_rx :: Bool, _hist :: DataWord, _rxConfReg :: ConfReg}
  | RxInvalid {_rx :: Bool}
  | IdleD {_rx :: Bool}
  | FalseCarrier {_rx :: Bool}
  | StartOfPacket {_rx :: Bool}
  | EarlyEnd {_rx :: Bool}
  | TriRri {_rx :: Bool}
  | TrrExtend {_rx :: Bool}
  | PacketBurstRrs {_rx :: Bool}
  | ExtendErr {_rx :: Bool}
  | EarlyEndExt {_rx :: Bool}
  | RxData {_rx :: Bool, _hist :: DataWord}
  | RxDataError {_rx :: Bool, _hist :: DataWord}
  | LinkFailed {_rx :: Bool}
  deriving (Generic, NFDataX, Eq, Show)

-- | Calculate the number of bits that are different in two code groups. For
--   example: the code groups @0b0000@ and @0b0001@ have a difference of 1.
bitDifference ::
  (KnownNat n) =>
  -- | First code group
  BitVector n ->
  -- | Second code group
  BitVector n ->
  -- | Bit difference
  Index (n + 1)
bitDifference cg0 cg1 = foldl f 0 $ map bitCoerce $ bv2v $ xor cg0 cg1
 where
  f a b = if b then a + 1 else a

-- | Take the running disparity, the 'Even' signal and the current data word
--   and determine whether there is a 2-bit or more difference between the code
--   group and both alternative representations of the K28.5 control word or a
--   difference of between 2 and 9 bits between the code group and the expected
--   encoding of the K28.5 control word.
--
--   Remarks:
--   - Currently this function needs to re-encode the received data words to
--     be able to determine the bit difference, this is rather unecessary in
--     practice and could be resolved by determining the carrier detect status
--     at an earlier point, for example at 'Sgmii.sync'
carrierDetect ::
  -- | Code group
  BitVector 10 ->
  -- | Running disparity
  Bool ->
  -- | 'Even' signal
  Even ->
  -- | The 'carrierDetect' condition
  Bool
carrierDetect cg rd rxEven
  | rxEven
      == Even
      && bitDifference cgK28_5N cg
      >= 2
      && bitDifference cgK28_5P cg
      >= 2 =
      True
  | rxEven
      == Even
      && bitDifference cgK28_5 cg
      >= 2
      && bitDifference cgK28_5 cg
      <= 9 =
      True
  | otherwise = False
 where
  cgK28_5N = 0b0011111010
  cgK28_5P = 0b1100000101
  cgK28_5 = if rd then cgK28_5P else cgK28_5N

-- | Take the running disparity, the current and next two input data words and
--   check whether they correspond to one of the specified end conditions
checkEnd ::
  -- | Current and next 2 data words
  Vec 3 DataWord ->
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

-- | State transition function for @pcsReceive. Takes the state as defined in
--   'PcsReceiveState', a tuple containing the inputs of the PCS Receive block
--   and returns a tuple containing the next state and the outputs as defined
--   in Clause 36 of IEEE 802.3. In contrast to the specification in Clause 36,
--   here 'Sgmii.syncT' is responsible for decoding the code groups instead of
--   this function, to not duplicate any work.
pcsReceiveT ::
  -- | Current state
  PcsReceiveState ->
  -- | Input values, where @Vec 3 CodeGroup@ contains the current and next two
  -- | data words
  (BitVector 10, Bool, Vec 3 DataWord, Even, SyncStatus, Xmit) ->
  -- | Tuple with the new state and the output values
  ( PcsReceiveState
  , ( PcsReceiveState
    , Maybe Bool
    , Maybe Bool
    , Maybe DataWord
    , Maybe Rudi
    , Maybe ConfReg
    )
  )
pcsReceiveT self@Init1 _ = (nextState, out)
 where
  nextState = Init2

  out = (self, Nothing, Nothing, Nothing, Nothing, Nothing)
pcsReceiveT self@Init2 _ = (nextState, out)
 where
  nextState = WaitForK False

  out = (self, Nothing, Nothing, Nothing, Nothing, Nothing)
pcsReceiveT self@WaitForK{} (_, _, dws, rxEven, syncStatus, _) =
  (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed rx
    | head dws == cwK28_5 && rxEven == Even = RxK rx
    | otherwise = self

  rx = False
  rxDv = Just False
  rxEr = Just False

  out = (self, rxDv, rxEr, Nothing, Nothing, Nothing)
pcsReceiveT self@RxK{} (_, _, dws, _, syncStatus, xmit) = (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed rx
    | dw == dwD21_5 = RxCB rx
    | dw == dwD02_2 = RxCB rx
    | not (isDw dw) && xmit /= Data = RxInvalid rx
    | xmit /= Data && isDw dw = IdleD rx
    | xmit == Data = IdleD rx
    | otherwise = self

  rx = False
  dw = head dws
  rxDv = Just False
  rxEr = Just False

  out = (self, rxDv, rxEr, Nothing, Nothing, Nothing)
pcsReceiveT self@RxCB{..} (_, _, dws, _, syncStatus, _) = (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed _rx
    | isDw dw = RxCC _rx dw
    | not (isDw dw) = RxInvalid _rx
    | otherwise = self

  dw = head dws
  rxDv = Just False
  rxEr = Just False

  out = (self, rxDv, rxEr, Nothing, Nothing, Nothing)
pcsReceiveT self@RxCC{..} (_, _, dws, _, syncStatus, _) = (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed _rx
    | isDw dw = RxCD _rx dw $ resize $ fromDw _hist
    | not (isDw dw) = RxInvalid _rx
    | otherwise = self

  dw = head dws

  out = (self, Nothing, Nothing, Nothing, Nothing, Nothing)
pcsReceiveT self@RxCD{..} (_, _, dws, rxEven, syncStatus, _) = (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed _rx
    | dw == cwK28_5 && rxEven == Even = RxK _rx
    | dw /= cwK28_5 = RxInvalid _rx
    | rxEven == Odd = RxInvalid _rx
    | otherwise = self

  dw = head dws
  rudi = Just C
  rxConfReg = (fromDw _hist ++# 0) .|. _rxConfReg

  out = (self, Nothing, Nothing, Nothing, rudi, Just rxConfReg)
pcsReceiveT self@RxInvalid{..} (_, _, dws, rxEven, syncStatus, xmit) =
  (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed rx
    | dw == cwK28_5 && rxEven == Even = RxK rx
    | dw /= cwK28_5 && rxEven == Even = WaitForK rx
    | otherwise = self

  rx = xmit == Data || _rx
  dw = head dws

  rudi = if xmit == Conf then Just Invalid else Nothing

  out = (self, Nothing, Nothing, Nothing, rudi, Nothing)
pcsReceiveT self@IdleD{} (cg, rd, dws, rxEven, syncStatus, xmit) =
  (nextState, out)
 where
  carrierDetected = carrierDetect cg rd rxEven

  nextState
    | syncStatus == Fail = LinkFailed rx
    | dw /= cwK28_5 && xmit /= Data = RxInvalid rx
    | carrierDetected && xmit == Data && dw /= cwS =
        FalseCarrier rx
    | carrierDetected && xmit == Data && dw == cwS =
        StartOfPacket rx
    | not carrierDetected && xmit == Data = RxK rx
    | dw == cwK28_5 = RxK rx
    | otherwise = self

  rx = False
  dw = head dws
  rxDv = Just False
  rxEr = Just False
  rudi = Just I

  out = (self, rxDv, rxEr, Nothing, rudi, Nothing)
pcsReceiveT self@FalseCarrier{} (_, _, dws, rxEven, syncStatus, _) =
  (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed rx
    | dw == cwK28_5 && rxEven == Even = RxK rx
    | otherwise = self

  rx = True
  dw = head dws
  rxEr = Just True

  out = (self, Nothing, rxEr, Just $ Cw 0b00001110, Nothing, Nothing)
pcsReceiveT self@StartOfPacket{} (_, _, dws, rxEven, syncStatus, _) =
  (nextState, out)
 where
  rxEnd = checkEnd dws

  nextState
    | syncStatus == Fail = LinkFailed rx
    | rxEnd == Just K28_5DK28_5 && rxEven == Even = EarlyEnd rx
    | rxEnd == Just K28_5D21_5D00_0 && rxEven == Even = EarlyEnd rx
    | rxEnd == Just K28_5D02_2D00_0 && rxEven == Even = EarlyEnd rx
    | rxEnd == Just TRK28_5 && rxEven == Even = TriRri rx
    | rxEnd == Just TRR = TrrExtend rx
    | rxEnd == Just RRR = EarlyEnd rx
    | isDw dw = RxData rx dw
    | otherwise = RxDataError rx dw

  rx = True
  dw = head dws
  rxDv = Just True
  rxEr = Just False

  out = (self, rxDv, rxEr, Just $ Cw 0b01010101, Nothing, Nothing)
pcsReceiveT self@EarlyEnd{..} (_, _, dws, _, syncStatus, _) = (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed _rx
    | dw /= dwD21_5 && dw /= dwD02_2 = IdleD _rx
    | dw == dwD02_2 = RxCB _rx
    | dw == dwD21_5 = RxCB _rx
    | otherwise = self

  dw = head dws

  rxEr = Just True

  out = (self, Nothing, rxEr, Nothing, Nothing, Nothing)
pcsReceiveT self@TriRri{} (_, _, dws, _, syncStatus, _) = (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed rx
    | head dws == cwK28_5 = RxK rx
    | otherwise = self

  rx = False
  rxDv = Just False
  rxEr = Just False

  out = (self, rxDv, rxEr, Nothing, Nothing, Nothing)
pcsReceiveT self@TrrExtend{..} (_, _, dws, rxEven, syncStatus, _) =
  (nextState, out)
 where
  rxEnd = checkEnd dws

  nextState
    | syncStatus == Fail = LinkFailed _rx
    | rxEnd == Just RRR = TrrExtend _rx
    | rxEnd == Just RRK28_5 && rxEven == Even = TriRri _rx
    | rxEnd == Just RRS = PacketBurstRrs _rx
    | otherwise = ExtendErr _rx

  rxDv = Just False
  rxEr = Just True

  out = (self, rxDv, rxEr, Just $ Cw 0b00001111, Nothing, Nothing)
pcsReceiveT self@PacketBurstRrs{..} (_, _, dws, _, syncStatus, _) =
  (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed _rx
    | head dws == cwS = StartOfPacket _rx
    | otherwise = self

  rxDv = Just False

  out = (self, rxDv, Nothing, Just $ Cw 0b00001111, Nothing, Nothing)
pcsReceiveT self@ExtendErr{..} (_, _, dws, rxEven, syncStatus, _) =
  (nextState, out)
 where
  rxEnd = checkEnd dws
  selfCond = dw /= cwS && dw /= cwK28_5 && rxEven == Even

  nextState
    | syncStatus == Fail = LinkFailed _rx
    | dw == cwS = StartOfPacket _rx
    | dw == cwK28_5 && rxEven == Even = RxK _rx
    | selfCond && rxEnd == Just RRR = TrrExtend _rx
    | selfCond && rxEnd == Just RRK28_5 = TriRri _rx
    | selfCond && rxEnd == Just RRS = PacketBurstRrs _rx
    | otherwise = self

  dw = head dws
  rxDv = Just False

  out = (self, rxDv, Nothing, Just $ Cw 0b00011111, Nothing, Nothing)
pcsReceiveT self@EarlyEndExt{..} (_, _, dws, rxEven, syncStatus, _) =
  (nextState, out)
 where
  rxEnd = checkEnd dws

  nextState
    | syncStatus == Fail = LinkFailed _rx
    | rxEnd == Just RRR = TrrExtend _rx
    | rxEnd == Just RRK28_5 && rxEven == Even = TriRri _rx
    | rxEnd == Just RRS = PacketBurstRrs _rx
    | otherwise = ExtendErr _rx

  rxEr = Just True

  out = (self, Nothing, rxEr, Nothing, Nothing, Nothing)
pcsReceiveT self@RxData{..} (_, _, dws, rxEven, syncStatus, _) =
  (nextState, out)
 where
  rxEnd = checkEnd dws

  nextState
    | syncStatus == Fail = LinkFailed _rx
    | rxEnd == Just K28_5DK28_5 && rxEven == Even = EarlyEnd _rx
    | rxEnd == Just K28_5D21_5D00_0 && rxEven == Even = EarlyEnd _rx
    | rxEnd == Just K28_5D02_2D00_0 && rxEven == Even = EarlyEnd _rx
    | rxEnd == Just TRK28_5 && rxEven == Even = TriRri _rx
    | rxEnd == Just TRR = TrrExtend _rx
    | rxEnd == Just RRR = EarlyEnd _rx
    | isDw dw = RxData _rx dw
    | otherwise = RxDataError _rx dw

  dw = head dws
  rxEr = Just False

  out = (self, Nothing, rxEr, Just _hist, Nothing, Nothing)
pcsReceiveT self@RxDataError{..} (_, _, dws, rxEven, syncStatus, _) =
  (nextState, out)
 where
  rxEnd = checkEnd dws

  nextState
    | syncStatus == Fail = LinkFailed _rx
    | rxEnd == Just K28_5DK28_5 && rxEven == Even = EarlyEnd _rx
    | rxEnd == Just K28_5D21_5D00_0 && rxEven == Even = EarlyEnd _rx
    | rxEnd == Just K28_5D02_2D00_0 && rxEven == Even = EarlyEnd _rx
    | rxEnd == Just TRK28_5 && rxEven == Even = TriRri _rx
    | rxEnd == Just TRR = TrrExtend _rx
    | rxEnd == Just RRR = EarlyEnd _rx
    | isDw dw = RxData _rx dw
    | otherwise = RxDataError _rx dw

  dw = head dws
  rxEr = Just True

  out = (self, Nothing, rxEr, Just _hist, Nothing, Nothing)
pcsReceiveT self@LinkFailed{..} (_, _, _, _, syncStatus, xmit) =
  (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed rx
    | otherwise = WaitForK rx

  (rx, rxDv, rxEr) =
    if _rx
      then (False, Nothing, Just True)
      else (_rx, Just False, Just False)
  rudi = if xmit /= Data then Just Invalid else Nothing

  out = (self, rxDv, rxEr, Nothing, rudi, Nothing)

-- | Takes a tuple with the new input code group, 'Even', 'SyncStatus' and
--   'Xmit' signals and runs the transition function 'pcsReceiveT' with the
--   inputs generated by 'sync'. The outputs are a set of Maybe values.
pcsReceive ::
  (HiddenClockResetEnable dom) =>
  -- | Current input code group
  Signal dom (BitVector 10) ->
  -- | Current running disparity from 'Sgmii.sync'
  Signal dom Bool ->
  -- | Input 'DataWord' from 'Sgmii.sync'
  Signal dom (Vec 3 DataWord) ->
  -- | The 'Even' value from 'Sgmii.sync'
  Signal dom Even ->
  -- | The current 'SyncStatus' from 'Sgmii.sync'
  Signal dom SyncStatus ->
  -- | The 'Xmit' signal from 'Sgmii.autoNeg'
  Signal dom Xmit ->
  -- | Tuple containing the output values
  Signal dom (Maybe Bool, Maybe Bool, Maybe DataWord, Maybe Rudi, Maybe ConfReg)
pcsReceive cg rd dw1 rxEven syncStatus xmit =
  bundle (rxDv, rxEr, dw2, rudi, rxConfReg)
 where
  (_, rxDv, rxEr, dw2, rudi, rxConfReg) =
    mealyB pcsReceiveT Init1 (cg, rd, dw1, rxEven, syncStatus, xmit)

{-# CLASH_OPAQUE pcsReceive #-}
