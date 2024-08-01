{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

-- |
--   Copyright   :  (C) 2024, QBayLogic B.V.
--   License     :  BSD2 (see the file LICENSE)
--   Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
--
--   Synchronization process, as defined in IEEE 802.3 Figure 36-9
module Clash.Cores.Sgmii.Sync
  ( OutputQueue
  , SyncState (..)
  , outputQueueO
  , outputQueueT
  , sync
  , syncO
  , syncT
  )
where

import Clash.Cores.LineCoding8b10b
import Clash.Cores.Sgmii.Common
import Clash.Prelude
import Data.Maybe (isNothing)

-- | State type of the output queue for 'sync'
type OutputQueue = Vec 3 (Cg, Bool, Symbol8b10b, Even, Status)

-- | State type of 'sync'. This contains all states as they are defined in IEEE
--   802.3 Clause 36.
data SyncState
  = LossOfSync {_cg :: Cg, _rd :: Bool, _dw :: Symbol8b10b, _rxEven :: Even}
  | CommaDetect {_cg :: Cg, _rd :: Bool, _dw :: Symbol8b10b, _i :: Index 3}
  | AcquireSync
      { _cg :: Cg
      , _rd :: Bool
      , _dw :: Symbol8b10b
      , _rxEven :: Even
      , _i :: Index 3
      }
  | SyncAcquired
      { _cg :: Cg
      , _rd :: Bool
      , _dw :: Symbol8b10b
      , _rxEven :: Even
      , _i :: Index 3
      }
  | SyncAcquiredA
      { _cg :: Cg
      , _rd :: Bool
      , _dw :: Symbol8b10b
      , _rxEven :: Even
      , _goodCgs :: Index 4
      , _i :: Index 3
      }
  deriving (Generic, NFDataX, Show)

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
syncT s cg = case s of
  LossOfSync{}
    | isNothing comma -> LossOfSync cg rd dw rxEven
    | otherwise -> CommaDetect cg rd dw 0
  CommaDetect{}
    | not (isDw dw) -> LossOfSync cg rd dw Even
    | _i s == 0 -> AcquireSync cg rd dw Even (_i s)
    | otherwise -> SyncAcquired cg rd dw Even 0
  AcquireSync{}
    | not (isValidSymbol dw) -> LossOfSync cg rd dw rxEven
    | cg `elem` commas && rxEven == Even -> LossOfSync cg rd dw rxEven
    | cg `elem` commas && rxEven == Odd -> CommaDetect cg rd dw 1
    | otherwise -> AcquireSync cg rd dw rxEven 0
  SyncAcquired{}
    | _i s == maxBound && not (isValidSymbol dw) -> LossOfSync cg rd dw rxEven
    | _i s == maxBound && cg `elem` commas && rxEven == Even ->
        LossOfSync cg rd dw rxEven
    | not (isValidSymbol dw) -> SyncAcquired cg rd dw rxEven (_i s + 1)
    | cg `elem` commas && rxEven == Even ->
        SyncAcquired cg rd dw rxEven (_i s + 1)
    | _i s == 0 -> SyncAcquired cg rd dw rxEven 0
    | otherwise -> SyncAcquiredA cg rd dw rxEven goodCgs (_i s)
  SyncAcquiredA{}
    | _i s == maxBound && not (isValidSymbol dw) -> LossOfSync cg rd dw rxEven
    | _i s == maxBound && cg `elem` commas && rxEven == Even ->
        LossOfSync cg rd dw rxEven
    | not (isValidSymbol dw) -> SyncAcquired cg rd dw rxEven (_i s + 1)
    | cg `elem` commas && rxEven == Even ->
        SyncAcquired cg rd dw rxEven (_i s + 1)
    | _i s == 0 && goodCgs == maxBound -> SyncAcquired cg rd dw rxEven 0
    | goodCgs == maxBound -> SyncAcquired cg rd dw rxEven (_i s - 1)
    | otherwise -> SyncAcquiredA cg rd dw rxEven goodCgs (_i s)
 where
  comma = elemIndex cg commas
  rdNew = case s of
    LossOfSync{} -> maybe (_rd s) bitCoerce comma
    _ -> _rd s
  (rd, dw) = decode8b10b rdNew cg
  rxEven = nextEven (_rxEven s)
  goodCgs = case s of
    SyncAcquiredA{} -> _goodCgs s + 1
    _ -> 0

-- | Output function for 'sync'. Takes the state as defined in 'SyncState' and
--   returns a tuple containing the outputs as defined in Clause 36 of IEEE
--   802.3
syncO ::
  -- | Current state
  SyncState ->
  -- | New state and output tuple
  (SyncState, Cg, Bool, Symbol8b10b, Even, Status)
syncO s = case s of
  LossOfSync{} -> (s, _cg s, _rd s, _dw s, rxEven, Fail)
  CommaDetect{} -> (s, _cg s, _rd s, _dw s, Even, Fail)
  AcquireSync{} -> (s, _cg s, _rd s, _dw s, rxEven, Fail)
  _ -> (s, _cg s, _rd s, _dw s, rxEven, Ok)
 where
  rxEven = nextEven (_rxEven s)

-- | Transition function for the inputs of 'Sgmii.pcsReceive'. This is used to
--   keep a small list of "future" values for 'Symbol8b10b', such that these can
--   be used in 'Sgmii.checkEnd'.
outputQueueT ::
  -- | Current state with three values for all inputs
  OutputQueue ->
  -- | New input values for the code group, running disparity, data word, 'Even'
  --   signal and 'Status;
  (Cg, Bool, Symbol8b10b, Even, Status) ->
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
  (Cg, Bool, Vec 3 Symbol8b10b, Even, Status)
outputQueueO s = (cg, rd, dw, rxEven, syncStatus)
 where
  (head -> cg, head -> rd, dw, head -> rxEven, head -> syncStatus) = unzip5 s

-- | Takes a code group and runs it through the state machine as defined in
--   IEEE 802.3 Clause 36 to check whether the signal is synchronized. If it is
--   not, output 'Status' @Fail@ and try to re-aquire synchronization, else
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
  , Signal dom Status
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
