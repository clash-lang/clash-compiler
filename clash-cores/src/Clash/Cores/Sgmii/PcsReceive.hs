{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Sgmii.PcsReceive where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.Common
import Clash.Prelude
import Data.Maybe (fromMaybe)

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
  | WaitForK {_rx :: Bool, _xmit :: Xmit}
  | RxK {_rx :: Bool, _xmit :: Xmit}
  | RxCB {_rx :: Bool, _xmit :: Xmit}
  | RxCC {_rx :: Bool, _xmit :: Xmit, _hist :: DataWord}
  | RxCD {_rx :: Bool, _xmit :: Xmit, _hist :: DataWord, _rxConfReg :: ConfReg}
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
  | RxData {_rx :: Bool, _xmit :: Xmit, _hist :: DataWord}
  | RxDataError {_rx :: Bool, _xmit :: Xmit, _hist :: DataWord}
  | LinkFailed {_rx :: Bool, _xmit :: Xmit}
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
  cgK28_5N = 0b0101111100
  cgK28_5P = 0b1010000011
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
  (BitVector 10, Bool, Vec 3 DataWord, Even, SyncStatus, Maybe Xmit) ->
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
  nextState = WaitForK False Conf

  out = (self, Nothing, Nothing, Nothing, Nothing, Nothing)
pcsReceiveT self@WaitForK{..} (_, _, dws, rxEven, syncStatus, xmit) =
  (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed rx xmit'
    | head dws == cwK28_5 && rxEven == Even = RxK rx xmit'
    | otherwise = self

  rx = False
  xmit' = fromMaybe _xmit xmit
  rxDv = Just False
  rxEr = Just False

  out = (self, rxDv, rxEr, Nothing, Nothing, Nothing)
pcsReceiveT self@RxK{..} (_, _, dws, _, syncStatus, xmit) = (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed rx xmit'
    | dw == dwD21_5 = RxCB rx xmit'
    | dw == dwD02_2 = RxCB rx xmit'
    | not (isDw dw) && xmit' /= Data = RxInvalid rx xmit'
    | xmit' /= Data && isDw dw = IdleD rx xmit'
    | xmit' == Data = IdleD rx xmit'
    | otherwise = self

  rx = False
  xmit' = fromMaybe _xmit xmit
  dw = head dws
  rxDv = Just False
  rxEr = Just False

  out = (self, rxDv, rxEr, Nothing, Nothing, Nothing)
pcsReceiveT self@RxCB{..} (_, _, dws, _, syncStatus, xmit) = (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed _rx xmit'
    | isDw dw = RxCC _rx xmit' dw
    | not (isDw dw) = RxInvalid _rx xmit'
    | otherwise = self

  xmit' = fromMaybe _xmit xmit
  dw = head dws
  rxDv = Just False
  rxEr = Just False

  out = (self, rxDv, rxEr, Nothing, Nothing, Nothing)
pcsReceiveT self@RxCC{..} (_, _, dws, _, syncStatus, xmit) = (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed _rx xmit'
    | isDw dw = RxCD _rx xmit' dw $ resize $ fromDw _hist
    | not (isDw dw) = RxInvalid _rx xmit'
    | otherwise = self

  xmit' = fromMaybe _xmit xmit
  dw = head dws

  out = (self, Nothing, Nothing, Nothing, Nothing, Nothing)
pcsReceiveT self@RxCD{..} (_, _, dws, rxEven, syncStatus, xmit) =
  (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed _rx xmit'
    | dw == cwK28_5 && rxEven == Even = RxK _rx xmit'
    | dw /= cwK28_5 = RxInvalid _rx xmit'
    | rxEven == Odd = RxInvalid _rx xmit'
    | otherwise = self

  xmit' = fromMaybe _xmit xmit
  dw = head dws
  rudi = Just C
  rxConfReg = (fromDw _hist ++# 0) .|. _rxConfReg

  out = (self, Nothing, Nothing, Nothing, rudi, Just rxConfReg)
pcsReceiveT self@RxInvalid{..} (_, _, dws, rxEven, syncStatus, xmit) =
  (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed rx xmit'
    | dw == cwK28_5 && rxEven == Even = RxK rx xmit'
    | dw /= cwK28_5 && rxEven == Even = WaitForK rx xmit'
    | otherwise = self

  rx = xmit' == Data || _rx
  xmit' = fromMaybe _xmit xmit
  dw = head dws

  rudi = if xmit' == Conf then Just Invalid else Nothing

  out = (self, Nothing, Nothing, Nothing, rudi, Nothing)
pcsReceiveT self@IdleD{..} (cg, rd, dws, rxEven, syncStatus, xmit) =
  (nextState, out)
 where
  carrierDetected = carrierDetect cg rd rxEven

  nextState
    | syncStatus == Fail = LinkFailed rx xmit'
    | dw /= cwK28_5 && xmit' /= Data = RxInvalid rx xmit'
    | carrierDetected && xmit' == Data && dw /= cwS =
        FalseCarrier rx xmit'
    | carrierDetected && xmit' == Data && dw == cwS =
        StartOfPacket rx xmit'
    | not carrierDetected && xmit' == Data = RxK rx xmit'
    | dw == cwK28_5 = RxK rx xmit'
    | otherwise = self

  rx = False
  xmit' = fromMaybe _xmit xmit
  dw = head dws
  rxDv = Just False
  rxEr = Just False
  rudi = Just I

  out = (self, rxDv, rxEr, Nothing, rudi, Nothing)
pcsReceiveT self@FalseCarrier{..} (_, _, dws, rxEven, syncStatus, xmit) =
  (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed rx xmit'
    | dw == cwK28_5 && rxEven == Even = RxK rx xmit'
    | otherwise = self

  rx = True
  xmit' = fromMaybe _xmit xmit
  dw = head dws
  rxEr = Just True

  out = (self, Nothing, rxEr, Just $ Cw 0b00001110, Nothing, Nothing)
pcsReceiveT self@StartOfPacket{..} (_, _, dws, rxEven, syncStatus, xmit) =
  (nextState, out)
 where
  rxEnd = checkEnd dws

  nextState
    | syncStatus == Fail = LinkFailed rx xmit'
    | rxEnd == Just K28_5DK28_5 && rxEven == Even = EarlyEnd rx xmit'
    | rxEnd == Just K28_5D21_5D00_0 && rxEven == Even = EarlyEnd rx xmit'
    | rxEnd == Just K28_5D02_2D00_0 && rxEven == Even = EarlyEnd rx xmit'
    | rxEnd == Just TRK28_5 && rxEven == Even = TriRri rx xmit'
    | rxEnd == Just TRR = TrrExtend rx xmit'
    | rxEnd == Just RRR = EarlyEnd rx xmit'
    | isDw dw = RxData rx xmit' dw
    | otherwise = RxDataError rx xmit' dw

  rx = True
  xmit' = fromMaybe _xmit xmit
  dw = head dws
  rxDv = Just True
  rxEr = Just False

  out = (self, rxDv, rxEr, Just $ Cw 0b01010101, Nothing, Nothing)
pcsReceiveT self@EarlyEnd{..} (_, _, dws, _, syncStatus, xmit) =
  (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed _rx xmit'
    | dw /= dwD21_5 && dw /= dwD02_2 = IdleD _rx xmit'
    | dw == dwD02_2 = RxCB _rx xmit'
    | dw == dwD21_5 = RxCB _rx xmit'
    | otherwise = self

  xmit' = fromMaybe _xmit xmit
  dw = head dws
  rxEr = Just True

  out = (self, Nothing, rxEr, Nothing, Nothing, Nothing)
pcsReceiveT self@TriRri{..} (_, _, dws, _, syncStatus, xmit) = (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed rx xmit'
    | head dws == cwK28_5 = RxK rx xmit'
    | otherwise = self

  rx = False
  xmit' = fromMaybe _xmit xmit
  rxDv = Just False
  rxEr = Just False

  out = (self, rxDv, rxEr, Nothing, Nothing, Nothing)
pcsReceiveT self@TrrExtend{..} (_, _, dws, rxEven, syncStatus, xmit) =
  (nextState, out)
 where
  rxEnd = checkEnd dws

  nextState
    | syncStatus == Fail = LinkFailed _rx xmit'
    | rxEnd == Just RRR = TrrExtend _rx xmit'
    | rxEnd == Just RRK28_5 && rxEven == Even = TriRri _rx xmit'
    | rxEnd == Just RRS = PacketBurstRrs _rx xmit'
    | otherwise = ExtendErr _rx xmit'

  xmit' = fromMaybe _xmit xmit
  rxDv = Just False
  rxEr = Just True

  out = (self, rxDv, rxEr, Just $ Cw 0b00001111, Nothing, Nothing)
pcsReceiveT self@PacketBurstRrs{..} (_, _, dws, _, syncStatus, xmit) =
  (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed _rx xmit'
    | head dws == cwS = StartOfPacket _rx xmit'
    | otherwise = self

  xmit' = fromMaybe _xmit xmit
  rxDv = Just False

  out = (self, rxDv, Nothing, Just $ Cw 0b00001111, Nothing, Nothing)
pcsReceiveT self@ExtendErr{..} (_, _, dws, rxEven, syncStatus, xmit) =
  (nextState, out)
 where
  rxEnd = checkEnd dws
  selfCond = dw /= cwS && dw /= cwK28_5 && rxEven == Even

  nextState
    | syncStatus == Fail = LinkFailed _rx xmit'
    | dw == cwS = StartOfPacket _rx xmit'
    | dw == cwK28_5 && rxEven == Even = RxK _rx xmit'
    | selfCond && rxEnd == Just RRR = TrrExtend _rx xmit'
    | selfCond && rxEnd == Just RRK28_5 = TriRri _rx xmit'
    | selfCond && rxEnd == Just RRS = PacketBurstRrs _rx xmit'
    | otherwise = self

  xmit' = fromMaybe _xmit xmit
  dw = head dws
  rxDv = Just False

  out = (self, rxDv, Nothing, Just $ Cw 0b00011111, Nothing, Nothing)
pcsReceiveT self@EarlyEndExt{..} (_, _, dws, rxEven, syncStatus, xmit) =
  (nextState, out)
 where
  rxEnd = checkEnd dws

  nextState
    | syncStatus == Fail = LinkFailed _rx xmit'
    | rxEnd == Just RRR = TrrExtend _rx xmit'
    | rxEnd == Just RRK28_5 && rxEven == Even = TriRri _rx xmit'
    | rxEnd == Just RRS = PacketBurstRrs _rx xmit'
    | otherwise = ExtendErr _rx xmit'

  xmit' = fromMaybe _xmit xmit
  rxEr = Just True

  out = (self, Nothing, rxEr, Nothing, Nothing, Nothing)
pcsReceiveT self@RxData{..} (_, _, dws, rxEven, syncStatus, xmit) =
  (nextState, out)
 where
  rxEnd = checkEnd dws

  nextState
    | syncStatus == Fail = LinkFailed _rx xmit'
    | rxEnd == Just K28_5DK28_5 && rxEven == Even = EarlyEnd _rx xmit'
    | rxEnd == Just K28_5D21_5D00_0 && rxEven == Even = EarlyEnd _rx xmit'
    | rxEnd == Just K28_5D02_2D00_0 && rxEven == Even = EarlyEnd _rx xmit'
    | rxEnd == Just TRK28_5 && rxEven == Even = TriRri _rx xmit'
    | rxEnd == Just TRR = TrrExtend _rx xmit'
    | rxEnd == Just RRR = EarlyEnd _rx xmit'
    | isDw dw = RxData _rx xmit' dw
    | otherwise = RxDataError _rx xmit' dw

  xmit' = fromMaybe _xmit xmit
  dw = head dws
  rxEr = Just False

  out = (self, Nothing, rxEr, Just _hist, Nothing, Nothing)
pcsReceiveT self@RxDataError{..} (_, _, dws, rxEven, syncStatus, xmit) =
  (nextState, out)
 where
  rxEnd = checkEnd dws

  nextState
    | syncStatus == Fail = LinkFailed _rx xmit'
    | rxEnd == Just K28_5DK28_5 && rxEven == Even = EarlyEnd _rx xmit'
    | rxEnd == Just K28_5D21_5D00_0 && rxEven == Even = EarlyEnd _rx xmit'
    | rxEnd == Just K28_5D02_2D00_0 && rxEven == Even = EarlyEnd _rx xmit'
    | rxEnd == Just TRK28_5 && rxEven == Even = TriRri _rx xmit'
    | rxEnd == Just TRR = TrrExtend _rx xmit'
    | rxEnd == Just RRR = EarlyEnd _rx xmit'
    | isDw dw = RxData _rx xmit' dw
    | otherwise = RxDataError _rx xmit' dw

  xmit' = fromMaybe _xmit xmit
  dw = head dws
  rxEr = Just True

  out = (self, Nothing, rxEr, Just _hist, Nothing, Nothing)
pcsReceiveT self@LinkFailed{..} (_, _, _, _, syncStatus, xmit) =
  (nextState, out)
 where
  nextState
    | syncStatus == Fail = LinkFailed rx xmit'
    | otherwise = WaitForK rx xmit'

  (rx, rxDv, rxEr) =
    if _rx
      then (False, Nothing, Just True)
      else (_rx, Just False, Just False)
  xmit' = fromMaybe _xmit xmit
  rudi = if xmit' /= Data then Just Invalid else Nothing

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
  Signal dom (Maybe Xmit) ->
  -- | Tuple containing the output values
  Signal dom (Maybe Bool, Maybe Bool, Maybe DataWord, Maybe Rudi, Maybe ConfReg)
pcsReceive cg rd dw1 rxEven syncStatus xmit =
  bundle (rxDv, rxEr, dw2, rudi, rxConfReg)
 where
  (_, rxDv, rxEr, dw2, rudi, rxConfReg) =
    mealyB pcsReceiveT Init1 (cg, rd, dw1, rxEven, syncStatus, xmit)

{-# CLASH_OPAQUE pcsReceive #-}
