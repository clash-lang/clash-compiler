{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Clash.Cores.Sgmii.Sync where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.Common
import Clash.Prelude

-- | State type of the output queue for 'sync'
type OutputQueue = Vec 3 (BitVector 10, Bool, DataWord, Even, SyncStatus)

-- | State type of 'sync'. This contains all states as they are defined in IEEE
--   802.3 Clause 36.
data SyncState
  = LossOfSync
      { _cg :: BitVector 10
      , _rd :: Bool
      , _dw :: DataWord
      , _rxEven :: Even
      }
  | CommaDetect1 {_cg :: BitVector 10, _rd :: Bool, _dw :: DataWord}
  | AcquireSync1
      { _cg :: BitVector 10
      , _rd :: Bool
      , _dw :: DataWord
      , _rxEven :: Even
      }
  | CommaDetect2 {_cg :: BitVector 10, _rd :: Bool, _dw :: DataWord}
  | AcquireSync2
      { _cg :: BitVector 10
      , _rd :: Bool
      , _dw :: DataWord
      , _rxEven :: Even
      }
  | CommaDetect3 {_cg :: BitVector 10, _rd :: Bool, _dw :: DataWord}
  | SyncAcquired1
      { _cg :: BitVector 10
      , _rd :: Bool
      , _dw :: DataWord
      , _rxEven :: Even
      }
  | SyncAcquired2
      { _cg :: BitVector 10
      , _rd :: Bool
      , _dw :: DataWord
      , _rxEven :: Even
      }
  | SyncAcquired2A
      { _cg :: BitVector 10
      , _rd :: Bool
      , _dw :: DataWord
      , _rxEven :: Even
      , _goodCgs :: Index 4
      }
  | SyncAcquired3
      { _cg :: BitVector 10
      , _rd :: Bool
      , _dw :: DataWord
      , _rxEven :: Even
      }
  | SyncAcquired3A
      { _cg :: BitVector 10
      , _rd :: Bool
      , _dw :: DataWord
      , _rxEven :: Even
      , _goodCgs :: Index 4
      }
  | SyncAcquired4
      { _cg :: BitVector 10
      , _rd :: Bool
      , _dw :: DataWord
      , _rxEven :: Even
      }
  | SyncAcquired4A
      { _cg :: BitVector 10
      , _rd :: Bool
      , _dw :: DataWord
      , _rxEven :: Even
      , _goodCgs :: Index 4
      }
  deriving (Generic, NFDataX, Eq, Show)

-- | Vector containing the 3 code groups that contain a comma (K28.1, K28.5 and
--   K28.7) and their alternative forms (with opposite running disparity)
commas :: Vec 6 (BitVector 10)
commas =
  0b0011111001
    :> 0b1100000110
    :> 0b0011111010
    :> 0b1100000101
    :> 0b0011111000
    :> 0b1100000111
    :> Nil

-- | State transition function for 'sync'. Takes the state as defined in
--   'SyncState', a the new incoming code group from the SerDes-block and
--   returns the next state as defined in Clause 36 of IEEE 802.3.
syncT ::
  -- | Current state
  SyncState ->
  -- | New input codegroup
  BitVector 10 ->
  -- | New state and output tuple
  SyncState
syncT LossOfSync{..} cg = nextState
 where
  nextState
    | cg `notElem` commas = LossOfSync cg rd dw rxEven
    | cg `elem` commas = CommaDetect1 cg rd dw
    | otherwise = LossOfSync cg rd dw rxEven

  (rd, dw) = decode8b10b _rd cg
  rxEven = nextEven _rxEven
syncT CommaDetect1{..} cg = nextState
 where
  nextState
    | not (isDw dw) = LossOfSync cg rd dw rxEven
    | isDw dw = AcquireSync1 cg rd dw rxEven
    | otherwise = CommaDetect1 cg rd dw

  (rd, dw) = decode8b10b _rd cg
  rxEven = Even
syncT AcquireSync1{..} cg = nextState
 where
  nextState
    | not (isValidDw dw) = LossOfSync cg rd dw rxEven
    | cg `elem` commas && rxEven == Even = LossOfSync cg rd dw rxEven
    | cg `elem` commas && rxEven == Odd = CommaDetect2 cg rd dw
    | cg `notElem` commas && isValidDw dw = AcquireSync1 cg rd dw rxEven
    | otherwise = AcquireSync1 cg rd dw rxEven

  (rd, dw) = decode8b10b _rd cg
  rxEven = nextEven _rxEven
syncT CommaDetect2{..} cg = nextState
 where
  nextState
    | not (isDw dw) = LossOfSync cg rd dw rxEven
    | isDw dw = AcquireSync2 cg rd dw rxEven
    | otherwise = CommaDetect2 cg rd dw

  (rd, dw) = decode8b10b _rd cg
  rxEven = Even
syncT AcquireSync2{..} cg = nextState
 where
  nextState
    | not (isValidDw dw) = LossOfSync cg rd dw rxEven
    | cg `elem` commas && rxEven == Even = LossOfSync cg rd dw rxEven
    | cg `elem` commas && rxEven == Odd = CommaDetect3 cg rd dw
    | cg `notElem` commas && isValidDw dw = AcquireSync2 cg rd dw rxEven
    | otherwise = AcquireSync2 cg rd dw rxEven

  (rd, dw) = decode8b10b _rd cg
  rxEven = nextEven _rxEven
syncT CommaDetect3{..} cg = nextState
 where
  nextState
    | not (isDw dw) = LossOfSync cg rd dw rxEven
    | isDw dw = SyncAcquired1 cg rd dw rxEven
    | otherwise = CommaDetect3 cg rd dw

  (rd, dw) = decode8b10b _rd cg
  rxEven = Even
syncT SyncAcquired1{..} cg = nextState
 where
  nextState
    | not (isValidDw dw) = SyncAcquired2 cg rd dw rxEven
    | cg `elem` commas && rxEven == Even = SyncAcquired2 cg rd dw rxEven
    | not (not (isValidDw dw) || cg `elem` commas && rxEven == Even) =
        SyncAcquired1 cg rd dw rxEven
    | otherwise = SyncAcquired1 cg rd dw rxEven

  (rd, dw) = decode8b10b _rd cg
  rxEven = nextEven _rxEven
syncT SyncAcquired2{..} cg = nextState
 where
  nextState
    | not (isValidDw dw) = SyncAcquired3 cg rd dw rxEven
    | cg `elem` commas && rxEven == Even = SyncAcquired3 cg rd dw rxEven
    | not (not (isValidDw dw) || cg `elem` commas && rxEven == Even) =
        SyncAcquired2A cg rd dw rxEven goodCgs
    | otherwise = SyncAcquired2 cg rd dw rxEven

  (rd, dw) = decode8b10b _rd cg
  rxEven = nextEven _rxEven
  goodCgs = 0
syncT SyncAcquired2A{..} cg = nextState
 where
  nextState
    | not (isValidDw dw) = SyncAcquired3 cg rd dw rxEven
    | cg `elem` commas && rxEven == Even = SyncAcquired3 cg rd dw rxEven
    | not (not (isValidDw dw) || cg `elem` commas && rxEven == Even)
        && goodCgs
        == 3 =
        SyncAcquired1 cg rd dw rxEven
    | not (not (isValidDw dw) || cg `elem` commas && rxEven == Even) =
        SyncAcquired2A cg rd dw rxEven goodCgs
    | otherwise = SyncAcquired2A cg rd dw rxEven goodCgs

  (rd, dw) = decode8b10b _rd cg
  rxEven = nextEven _rxEven
  goodCgs = _goodCgs + 1
syncT SyncAcquired3{..} cg = nextState
 where
  nextState
    | not (isValidDw dw) = SyncAcquired4 cg rd dw rxEven
    | cg `elem` commas && rxEven == Even = SyncAcquired4 cg rd dw rxEven
    | not (not (isValidDw dw) || cg `elem` commas && rxEven == Even) =
        SyncAcquired3A cg rd dw rxEven goodCgs
    | otherwise = SyncAcquired3 cg rd dw rxEven

  (rd, dw) = decode8b10b _rd cg
  rxEven = nextEven _rxEven
  goodCgs = 0
syncT SyncAcquired3A{..} cg = nextState
 where
  nextState
    | not (isValidDw dw) = SyncAcquired4 cg rd dw rxEven
    | cg `elem` commas && rxEven == Even = SyncAcquired4 cg rd dw rxEven
    | not (not (isValidDw dw) || cg `elem` commas && rxEven == Even)
        && goodCgs
        == 3 =
        SyncAcquired2 cg rd dw rxEven
    | not (not (isValidDw dw) || cg `elem` commas && rxEven == Even) =
        SyncAcquired3A cg rd dw rxEven goodCgs
    | otherwise = SyncAcquired3A cg rd dw rxEven goodCgs

  (rd, dw) = decode8b10b _rd cg
  rxEven = nextEven _rxEven
  goodCgs = _goodCgs + 1
syncT SyncAcquired4{..} cg = nextState
 where
  nextState
    | not (isValidDw dw) = LossOfSync cg rd dw rxEven
    | cg `elem` commas && rxEven == Even = LossOfSync cg rd dw rxEven
    | not (not (isValidDw dw) || cg `elem` commas && rxEven == Even) =
        SyncAcquired4A cg rd dw rxEven goodCgs
    | otherwise = SyncAcquired4 cg rd dw rxEven

  (rd, dw) = decode8b10b _rd cg
  rxEven = nextEven _rxEven
  goodCgs = 0
syncT SyncAcquired4A{..} cg = nextState
 where
  nextState
    | not (isValidDw dw) = LossOfSync cg rd dw rxEven
    | cg `elem` commas && rxEven == Even = LossOfSync cg rd dw rxEven
    | not (not (isValidDw dw) || cg `elem` commas && rxEven == Even)
        && goodCgs
        == 3 =
        SyncAcquired3 cg rd dw rxEven
    | not (not (isValidDw dw) || cg `elem` commas && rxEven == Even) =
        SyncAcquired4A cg rd dw rxEven goodCgs
    | otherwise = SyncAcquired4A cg rd dw rxEven goodCgs

  (rd, dw) = decode8b10b _rd cg
  rxEven = nextEven _rxEven
  goodCgs = _goodCgs + 1

-- | Output function for 'sync'. Takes the state as defined in 'SyncState' and
--   returns a tuple containing the outputs as defined in Clause 36 of IEEE
--   802.3
syncO ::
  -- | Current state
  SyncState ->
  -- | New state and output tuple
  (SyncState, BitVector 10, Bool, DataWord, Even, SyncStatus)
syncO self@LossOfSync{..} = (self, _cg, _rd, _dw, rxEven, Fail)
 where
  rxEven = nextEven _rxEven
syncO self@CommaDetect1{..} = (self, _cg, _rd, _dw, Even, Fail)
syncO self@AcquireSync1{..} = (self, _cg, _rd, _dw, rxEven, Fail)
 where
  rxEven = nextEven _rxEven
syncO self@CommaDetect2{..} = (self, _cg, _rd, _dw, Even, Fail)
syncO self@AcquireSync2{..} = (self, _cg, _rd, _dw, rxEven, Fail)
 where
  rxEven = nextEven _rxEven
syncO self@CommaDetect3{..} = (self, _cg, _rd, _dw, Even, Fail)
syncO self = (self, self._cg, self._rd, self._dw, rxEven, Ok)
 where
  rxEven = nextEven self._rxEven

-- | Transition function for the inputs of 'Sgmii.pcsReceive'. This is used to
--   keep a small list of "future" values for 'DataWord', such that these can
--   be used in 'Sgmii.checkEnd'.
outputQueueT ::
  -- | Current state with three values for all inputs
  OutputQueue ->
  -- | New input values for the code group, running disparity, data word, 'Even'
  --   signal and 'SyncStatus;
  (BitVector 10, Bool, DataWord, Even, SyncStatus) ->
  -- | New state
  OutputQueue
outputQueueT s i = s <<+ i

-- | Output function for the output queue, where the values are taken from the
--   current state
outputQueueO ::
  -- Current state with three values for all inputs
  OutputQueue ->
  -- | New output with one value for everything except 'DataWord' for the
  --   prescient 'Sgmii.checkEnd' function.
  (BitVector 10, Bool, Vec 3 DataWord, Even, SyncStatus)
outputQueueO s = (cg, rd, dw, rxEven, syncStatus)
 where
  (head -> cg, head -> rd, dw, head -> rxEven, head -> syncStatus) = unzip5 s

-- | Takes a code group and runs it through the state machine as defined in
--   IEEE 802.3 Clause 36 to check whether the signal is synchronized. If it is
--   not, output 'SyncStatus' @Fail@ and try to re-aquire synchronization, else
--   simply pass through the new running disparity and 'DataWord' from the
--   decoded code group as well as the 'Even' signal. The current code word is
--   also propagated as it is required by 'Sgmii.pcsReceive'.
sync ::
  (HiddenClockResetEnable dom) =>
  -- | New code group from the PHY
  Signal dom (BitVector 10) ->
  -- | A tuple containing the running disparity, a new 'DataWord', the new value
  --   for 'Even' and the current synchronization status
  Signal dom (BitVector 10, Bool, Vec 3 DataWord, Even, SyncStatus)
sync cg1 = out
 where
  out =
    moore outputQueueT outputQueueO (repeat (0, False, Dw 0, Odd, Fail))
      $ bundle (cg2, rd, dw, rxEven, syncStatus)

  (_, cg2, rd, dw, rxEven, syncStatus) =
    mooreB syncT syncO (LossOfSync 0 False (Dw 0) Even) $ register 0 cg1

{-# CLASH_OPAQUE sync #-}
