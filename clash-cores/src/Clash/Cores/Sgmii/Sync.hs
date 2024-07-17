{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- |
--   Copyright   :  (C) 2024, QBayLogic B.V.
--   License     :  BSD2 (see the file LICENSE)
--   Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
--
--   Synchronization process, as defined in IEEE 802.3 Figure 36-9
module Clash.Cores.Sgmii.Sync where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.Common
import Clash.Prelude
import Data.Maybe (isNothing)

-- | State type of the output queue for 'sync'
type OutputQueue = Vec 3 (Cg, Bool, Symbol8b10b, Even, SyncStatus)

-- | State type of 'sync'. This contains all states as they are defined in IEEE
--   802.3 Clause 36.
data SyncState
  = LossOfSync {_cg :: Cg, _rd :: Bool, _dw :: Symbol8b10b, _rxEven :: Even}
  | CommaDetect1 {_cg :: Cg, _rd :: Bool, _dw :: Symbol8b10b}
  | AcquireSync1 {_cg :: Cg, _rd :: Bool, _dw :: Symbol8b10b, _rxEven :: Even}
  | CommaDetect2 {_cg :: Cg, _rd :: Bool, _dw :: Symbol8b10b}
  | AcquireSync2 {_cg :: Cg, _rd :: Bool, _dw :: Symbol8b10b, _rxEven :: Even}
  | CommaDetect3 {_cg :: Cg, _rd :: Bool, _dw :: Symbol8b10b}
  | SyncAcquired1 {_cg :: Cg, _rd :: Bool, _dw :: Symbol8b10b, _rxEven :: Even}
  | SyncAcquired2 {_cg :: Cg, _rd :: Bool, _dw :: Symbol8b10b, _rxEven :: Even}
  | SyncAcquired2A
      { _cg :: Cg
      , _rd :: Bool
      , _dw :: Symbol8b10b
      , _rxEven :: Even
      , _goodCgs :: Index 4
      }
  | SyncAcquired3 {_cg :: Cg, _rd :: Bool, _dw :: Symbol8b10b, _rxEven :: Even}
  | SyncAcquired3A
      { _cg :: Cg
      , _rd :: Bool
      , _dw :: Symbol8b10b
      , _rxEven :: Even
      , _goodCgs :: Index 4
      }
  | SyncAcquired4 {_cg :: Cg, _rd :: Bool, _dw :: Symbol8b10b, _rxEven :: Even}
  | SyncAcquired4A
      { _cg :: Cg
      , _rd :: Bool
      , _dw :: Symbol8b10b
      , _rxEven :: Even
      , _goodCgs :: Index 4
      }
  deriving (Generic, NFDataX, Eq, Show)

-- | Vector containing the two alternative forms (with opposite running
--   disparity) of K28.5. This is the only relevant comma, as the other commas
--   are set as "reserved" in the list of control words. The order of the commas
--   in this is important, as the first comma returns the negative running
--   disparity when it is decoded and the second comma returns the positive
--   running disparity when it is decoded. This is used in 'LossOfSync' to
--   recover the correct running disparity from a received comma.
commas :: Vec 2 Cg
commas = cgK28_5N :> cgK28_5P :> Nil

-- | State transition function for 'sync'. Takes the state as defined in
--   'SyncState', a the new incoming code group from the deserialization block
--   and returns the next state as defined in Clause 36 of IEEE 802.3. As is
--   described in the documentation for 'Sgmii.pcsReceive', this function also
--   does the decoding of 10-bit code groups (which is usually done by
--   'Sgmii.pcsReceive') as it needs the information provided by the decode
--   function to determine whether a code group corresponds to a valid data
--   word.
syncT ::
  -- | Current state
  SyncState ->
  -- | New input codegroup
  Cg ->
  -- | New state and output tuple
  SyncState
syncT LossOfSync{..} cg
  | isNothing comma = LossOfSync cg rd dw rxEven
  | otherwise = CommaDetect1 cg rd dw
 where
  -- As written in the documentation for 'commas', this is used to recover the
  -- running disparity in case there has been a reset
  comma = elemIndex cg commas
  rdNew = maybe _rd bitCoerce comma

  (rd, dw) = decode8b10b rdNew cg
  rxEven = nextEven _rxEven
syncT CommaDetect1{..} cg
  | not (isDw dw) = LossOfSync cg rd dw Even
  | otherwise = AcquireSync1 cg rd dw Even
 where
  (rd, dw) = decode8b10b _rd cg
syncT AcquireSync1{..} cg
  | not (isValidSymbol dw) = LossOfSync cg rd dw rxEven
  | cg `elem` commas && rxEven == Even = LossOfSync cg rd dw rxEven
  | cg `elem` commas && rxEven == Odd = CommaDetect2 cg rd dw
  | otherwise = AcquireSync1 cg rd dw rxEven
 where
  (rd, dw) = decode8b10b _rd cg
  rxEven = nextEven _rxEven
syncT CommaDetect2{..} cg
  | not (isDw dw) = LossOfSync cg rd dw Even
  | otherwise = AcquireSync2 cg rd dw Even
 where
  (rd, dw) = decode8b10b _rd cg
syncT AcquireSync2{..} cg
  | not (isValidSymbol dw) = LossOfSync cg rd dw rxEven
  | cg `elem` commas && rxEven == Even = LossOfSync cg rd dw rxEven
  | cg `elem` commas && rxEven == Odd = CommaDetect3 cg rd dw
  | otherwise = AcquireSync2 cg rd dw rxEven
 where
  (rd, dw) = decode8b10b _rd cg
  rxEven = nextEven _rxEven
syncT CommaDetect3{..} cg
  | not (isDw dw) = LossOfSync cg rd dw Even
  | otherwise = SyncAcquired1 cg rd dw Even
 where
  (rd, dw) = decode8b10b _rd cg
syncT SyncAcquired1{..} cg
  | not (isValidSymbol dw) = SyncAcquired2 cg rd dw rxEven
  | cg `elem` commas && rxEven == Even = SyncAcquired2 cg rd dw rxEven
  | otherwise = SyncAcquired1 cg rd dw rxEven
 where
  (rd, dw) = decode8b10b _rd cg
  rxEven = nextEven _rxEven
syncT self cg
  | not (isValidSymbol dw) = s1 cg rd dw rxEven
  | cg `elem` commas && rxEven == Even = s1 cg rd dw rxEven
  | goodCgs == maxBound = s2 cg rd dw rxEven
  | otherwise = s3 cg rd dw rxEven goodCgs
 where
  (s1, s2, s3, goodCgs) = case self of
    SyncAcquired2{} -> (SyncAcquired3, undefined, SyncAcquired2A, 0)
    SyncAcquired2A{} ->
      (SyncAcquired3, SyncAcquired1, SyncAcquired2A, self._goodCgs + 1)
    SyncAcquired3{} -> (SyncAcquired4, undefined, SyncAcquired3A, 0)
    SyncAcquired3A{} ->
      (SyncAcquired4, SyncAcquired2, SyncAcquired3A, self._goodCgs + 1)
    SyncAcquired4{} -> (LossOfSync, undefined, SyncAcquired4A, 0)
    SyncAcquired4A{} ->
      (LossOfSync, SyncAcquired3, SyncAcquired4A, self._goodCgs + 1)

  (rd, dw) = decode8b10b self._rd cg
  rxEven = nextEven self._rxEven

-- | Output function for 'sync'. Takes the state as defined in 'SyncState' and
--   returns a tuple containing the outputs as defined in Clause 36 of IEEE
--   802.3
syncO ::
  -- | Current state
  SyncState ->
  -- | New state and output tuple
  (SyncState, Cg, Bool, Symbol8b10b, Even, SyncStatus)
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
--   keep a small list of "future" values for 'Symbol8b10b', such that these can
--   be used in 'Sgmii.checkEnd'.
outputQueueT ::
  -- | Current state with three values for all inputs
  OutputQueue ->
  -- | New input values for the code group, running disparity, data word, 'Even'
  --   signal and 'SyncStatus;
  (Cg, Bool, Symbol8b10b, Even, SyncStatus) ->
  -- | New state
  OutputQueue
outputQueueT s i = s <<+ i

-- | Output function for the output queue, where the values are taken from the
--   current state
outputQueueO ::
  -- Current state with three values for all inputs
  OutputQueue ->
  -- | New output with one value for everything except 'Symbol8b10b' for the
  --   prescient 'Sgmii.checkEnd' function.
  (Cg, Bool, Vec 3 Symbol8b10b, Even, SyncStatus)
outputQueueO s = (cg, rd, dw, rxEven, syncStatus)
 where
  (head -> cg, head -> rd, dw, head -> rxEven, head -> syncStatus) = unzip5 s

-- | Takes a code group and runs it through the state machine as defined in
--   IEEE 802.3 Clause 36 to check whether the signal is synchronized. If it is
--   not, output 'SyncStatus' @Fail@ and try to re-aquire synchronization, else
--   simply pass through the new running disparity and 'Symbol8b10b' from the
--   decoded code group as well as the 'Even' signal. The current code word is
--   also propagated as it is required by 'Sgmii.pcsReceive'. This function
--   contains a list of data words as these need to be used by the prescient
--   'Sgmii.checkEnd' function.
sync ::
  (HiddenClockResetEnable dom) =>
  -- | New code group from the PHY
  Signal dom Cg ->
  -- | A tuple containing the input code group, running disparity, a new
  --   'Symbol8b10b', the new value for 'Even' and the current synchronization
  --   status
  ( Signal dom Cg
  , Signal dom Bool
  , Signal dom (Vec 3 Symbol8b10b)
  , Signal dom Even
  , Signal dom SyncStatus
  )
sync rxCg =
  mooreB
    outputQueueT
    outputQueueO
    (repeat (0, False, Dw 0, Odd, Fail))
    (cg, rd, dw, rxEven, syncStatus)
 where
  (_, cg, rd, dw, rxEven, syncStatus) =
    mooreB syncT syncO (LossOfSync 0 False (Dw 0) Even) rxCg

{-# CLASH_OPAQUE sync #-}
