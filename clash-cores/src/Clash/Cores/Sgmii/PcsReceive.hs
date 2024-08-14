{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--   Copyright   :  (C) 2024, QBayLogic B.V.
--   License     :  BSD2 (see the file LICENSE)
--   Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
--
--   PCS receive process, as defined in IEEE 802.3 Figure 36-7a and 36-7b
module Clash.Cores.Sgmii.PcsReceive
  ( PcsReceiveState (..)
  , pcsReceive
  , pcsReceiveO
  , pcsReceiveT
  )
where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.Common
import Clash.Prelude
import Data.Maybe (fromJust, isJust)

-- | Defines all possible valid termination values
data CheckEnd = KDK | KDD | TRK | TRR | RRR | RRK | RRS
  deriving (Eq)

-- | State type of 'pcsReceive'. This contains all states as they are defined in
--   IEEE 802.3 Clause 36, with with exeception of the states @CARRIER_DETECT@,
--   @RECEIVE@ and @EPD2_CHECK_END@ as these do not act upon a received code
--   group. The transitions of these states are embedded in the states that
--   usually transition to either of these states.
data PcsReceiveState
  = WaitForK {_rx :: Bool}
  | RxK {_rx :: Bool}
  | RxCB {_rx :: Bool}
  | RxCC {_rx :: Bool, _hist :: Symbol8b10b}
  | RxCD {_rx :: Bool, _rxConfReg :: ConfReg}
  | RxInvalid {_rx :: Bool, _xmit :: Xmit}
  | IdleD {_rx :: Bool}
  | FalseCarrier {_rx :: Bool}
  | StartOfPacket {_rx :: Bool}
  | EarlyEnd {_rx :: Bool}
  | TriRri {_rx :: Bool}
  | TrrExtend {_rx :: Bool}
  | PacketBurstRrs {_rx :: Bool}
  | ExtendErr {_rx :: Bool}
  | EarlyEndExt {_rx :: Bool}
  | RxData {_rx :: Bool, _hist :: Symbol8b10b}
  | RxDataError {_rx :: Bool, _hist :: Symbol8b10b}
  | LinkFailed {_rx :: Bool, _xmit :: Xmit}
  deriving (Generic, NFDataX, Show)

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
  CodeGroup ->
  -- | Running disparity
  Bool ->
  -- | 'Even' signal
  Even ->
  -- | The carrier detection condition
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
  | dws == cwK28_5 :> dws !! (1 :: Index 3) :> cwK28_5 :> Nil = Just KDK
  | dws == cwK28_5 :> dwD21_5 :> dwD00_0 :> Nil = Just KDD
  | dws == cwK28_5 :> dwD02_2 :> dwD00_0 :> Nil = Just KDD
  | dws == cwT :> cwR :> cwK28_5 :> Nil = Just TRK
  | dws == cwT :> Nil ++ repeat cwR = Just TRR
  | dws == repeat cwR = Just RRR
  | dws == repeat cwR ++ cwK28_5 :> Nil = Just RRK
  | dws == repeat cwR ++ cwS :> Nil = Just RRS
  | otherwise = Nothing

-- | Function that implements the transitions of the @EPD2_CHECK_END@ state
epd2CheckEnd :: Vec 3 Symbol8b10b -> Even -> Bool -> Maybe PcsReceiveState
epd2CheckEnd dws rxEven rx
  | rxEnd == Just RRR = Just (TrrExtend rx)
  | rxEnd == Just RRK && rxEven == Even = Just (TriRri rx)
  | rxEnd == Just RRS = Just (PacketBurstRrs rx)
  | otherwise = Nothing
 where
  rxEnd = checkEnd dws

-- | Function that implements the transitions of the @RECEIVE@ state
receive :: Vec 3 Symbol8b10b -> Even -> Bool -> Maybe PcsReceiveState
receive dws rxEven rx
  | rxEnd == Just KDK && rxEven == Even = Just (EarlyEnd rx)
  | rxEnd == Just KDD && rxEven == Even = Just (EarlyEnd rx)
  | rxEnd == Just TRK && rxEven == Even = Just (TriRri rx)
  | rxEnd == Just TRR = Just (TrrExtend rx)
  | rxEnd == Just RRR = Just (EarlyEnd rx)
  | isDw (head dws) = Just (RxData rx (head dws))
  | otherwise = Nothing
 where
  rxEnd = checkEnd dws

-- | State transition function for 'pcsReceive'. Takes the state as defined in
--   'PcsReceiveState' and returns the next state as defined in Clause 36 of
--   IEEE 802.3. In contrast to the specification in Clause 36, here
--   'Sgmii.syncT' is responsible for decoding the code groups instead of this
--   function, to not duplicate any work, but as this function does need to
--   determine the difference in bits the code group is set as an input value as
--   well.
--
--   __N.B.__: This function does not implement the optional EEE
--   (Energy-Efficient Ethernet) capability.
pcsReceiveT ::
  -- | Current state
  PcsReceiveState ->
  -- | Input values, where @Vec 3 CodeGroup@ contains the current and next two
  -- | data words
  (CodeGroup, Bool, Vec 3 Symbol8b10b, Even, Status, Xmit) ->
  -- | New state
  PcsReceiveState
pcsReceiveT WaitForK{..} (_, _, dws, rxEven, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed False xmit
  | head dws == cwK28_5 && rxEven == Even = RxK False
  | otherwise = WaitForK _rx
pcsReceiveT RxK{} (_, _, dws, _, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed False xmit
  | head dws == dwD21_5 || head dws == dwD02_2 = RxCB False
  | xmit == Data || isDw (head dws) = IdleD False
  | otherwise = RxInvalid False xmit
pcsReceiveT RxCB{..} (_, _, dws, _, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed _rx xmit
  | isDw (head dws) = RxCC _rx (head dws)
  | otherwise = RxInvalid _rx xmit
pcsReceiveT RxCC{..} (_, _, dws, _, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed _rx xmit
  | isDw (head dws) = RxCD _rx rxConfReg
  | otherwise = RxInvalid _rx xmit
 where
  rxConfReg = pack $ map fromSymbol $ head dws :> _hist :> Nil
pcsReceiveT RxCD{..} (_, _, dws, rxEven, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed _rx xmit
  | head dws == cwK28_5 && rxEven == Even = RxK _rx
  | otherwise = RxInvalid _rx xmit
pcsReceiveT RxInvalid{..} (_, _, dws, rxEven, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed rx xmit
  | rxEven == Odd = RxInvalid rx xmit
  | head dws == cwK28_5 = RxK rx
  | otherwise = WaitForK rx
 where
  rx = xmit == Data || _rx
pcsReceiveT IdleD{} (cg, rd, dws, rxEven, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed False xmit
  | head dws /= cwK28_5 && xmit /= Data = RxInvalid False xmit
  | carrierDetected && xmit == Data && head dws /= cwS = FalseCarrier False
  | carrierDetected && xmit == Data && head dws == cwS = StartOfPacket False
  | otherwise = RxK False
 where
  carrierDetected = carrierDetect cg rd rxEven
pcsReceiveT FalseCarrier{..} (_, _, dws, rxEven, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed True xmit
  | head dws == cwK28_5 && rxEven == Even = RxK True
  | otherwise = FalseCarrier _rx
pcsReceiveT EarlyEnd{..} (_, _, dws, _, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed _rx xmit
  | head dws == dwD21_5 || head dws == dwD02_2 = RxCB False
  | otherwise = IdleD _rx
pcsReceiveT TriRri{..} (_, _, dws, _, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed False xmit
  | head dws == cwK28_5 = RxK False
  | otherwise = TriRri _rx
pcsReceiveT PacketBurstRrs{..} (_, _, dws, _, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed _rx xmit
  | head dws == cwS = StartOfPacket _rx
  | otherwise = PacketBurstRrs _rx
pcsReceiveT ExtendErr{..} (_, _, dws, rxEven, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed _rx xmit
  | head dws == cwS = StartOfPacket _rx
  | head dws == cwK28_5 && rxEven == Even = RxK _rx
  | isJust s && rxEven == Even = fromJust s
  | otherwise = ExtendErr _rx
 where
  s = epd2CheckEnd dws rxEven _rx
pcsReceiveT LinkFailed{} (_, _, _, _, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed False xmit
  | otherwise = WaitForK False
pcsReceiveT s (_, _, dws, rxEven, syncStatus, xmit)
  | syncStatus == Fail = LinkFailed (_rx s) xmit
  | isJust s1 = fromJust s1
  | otherwise = s2
 where
  (s1, s2) = case s of
    TrrExtend{} ->
      (epd2CheckEnd dws rxEven (_rx s), ExtendErr (_rx s))
    EarlyEndExt{} ->
      (epd2CheckEnd dws rxEven (_rx s), ExtendErr (_rx s))
    _ -> (receive dws rxEven (_rx s), RxDataError (_rx s) (head dws))

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
  )
pcsReceiveO s = case s of
  WaitForK{} -> (s, Just False, Just False, Nothing, Nothing)
  RxK{} -> (s, Just False, Just False, Nothing, Nothing)
  RxCB{} -> (s, Just False, Just False, Nothing, Nothing)
  RxCD{} -> (s, Nothing, Nothing, Nothing, Just (RudiC (_rxConfReg s)))
  RxInvalid{} ->
    (s, Nothing, Nothing, Nothing, orNothing (_xmit s == Conf) RudiInvalid)
  IdleD{} -> (s, Just False, Just False, Nothing, Just RudiI)
  FalseCarrier{} -> (s, Nothing, Just True, Just (Cw 0b00001110), Nothing)
  StartOfPacket{} -> (s, Just True, Just False, Just (Cw 0b01010101), Nothing)
  EarlyEnd{} -> (s, Nothing, Just True, Nothing, Nothing)
  TriRri{} -> (s, Just False, Just False, Nothing, Nothing)
  TrrExtend{} -> (s, Just False, Just True, Just (Cw 0b00001111), Nothing)
  PacketBurstRrs{} -> (s, Just False, Nothing, Just (Cw 0b00001111), Nothing)
  ExtendErr{} -> (s, Just False, Nothing, Just (Cw 0b00011111), Nothing)
  EarlyEndExt{} -> (s, Nothing, Just True, Nothing, Nothing)
  RxData{} -> (s, Nothing, Just False, Just (_hist s), Nothing)
  RxDataError{} -> (s, Nothing, Just True, Just (_hist s), Nothing)
  LinkFailed{} ->
    ( s
    , orNothing (_rx s) False
    , Just (_rx s)
    , Nothing
    , orNothing (_xmit s /= Data) RudiInvalid
    )
  _ -> (s, Nothing, Nothing, Nothing, Nothing)

-- | The 'pcsReceive' block. Takes a tuple with the new input code group,
--   running disparity and data word, 'Even', 'Status' and 'Xmit' signals
--   and runs the transition function 'pcsReceiveT'. The outputs are a set of
--   'Maybe' values.
pcsReceive ::
  (HiddenClockResetEnable dom) =>
  -- | Current code group from 'Sgmii.sync'
  Signal dom CodeGroup ->
  -- | Current running disparity from 'Sgmii.sync'
  Signal dom Bool ->
  -- | Input 'Symbol8b10b' from 'Sgmii.sync'
  Signal dom (Vec 3 Symbol8b10b) ->
  -- | The 'Even' value from 'Sgmii.sync'
  Signal dom Even ->
  -- | The current 'Status' from 'Sgmii.sync'
  Signal dom Status ->
  -- | The 'Xmit' signal from 'Sgmii.autoNeg'
  Signal dom (Maybe Xmit) ->
  -- | Tuple containing the output values
  ( Signal dom (Maybe Bool)
  , Signal dom (Maybe Bool)
  , Signal dom (Maybe Symbol8b10b)
  , Signal dom (Maybe Rudi)
  )
pcsReceive cg rd dw1 rxEven syncStatus xmit = (rxDv, rxEr, dw2, rudi)
 where
  (_, rxDv, rxEr, dw2, rudi) =
    mooreB
      pcsReceiveT
      pcsReceiveO
      (WaitForK False)
      (cg, rd, dw1, rxEven, syncStatus, xmit')

  xmit' = regMaybe Idle xmit

{-# CLASH_OPAQUE pcsReceive #-}
