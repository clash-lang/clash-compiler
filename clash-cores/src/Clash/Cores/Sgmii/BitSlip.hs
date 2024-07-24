{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--   Copyright   :  (C) 2024, QBayLogic B.V.
--   License     :  BSD2 (see the file LICENSE)
--   Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
--
--   Bit slip function that word-aligns a stream of bits based on received
--   comma values
module Clash.Cores.Sgmii.BitSlip
  ( BitSlipState (..)
  , bitSlip
  , bitSlipO
  , bitSlipT
  )
where

import Clash.Cores.Sgmii.Common
import Clash.Prelude
import Data.Maybe (fromJust, isNothing)

-- | State variable for 'bitSlip', with the two states as described in
--   'bitSlipT'. Due to timing constraints, not all functions can be executed in
--   the same cycle, which is why intermediate values are saved in the record
--   for 'BSFail'.
data BitSlipState
  = BSFail
      { _s :: BitVector 20
      , _ns :: Vec 8 (Index 10)
      , _hist :: Vec 10 (BitVector 10)
      }
  | BSOk {_s :: BitVector 20, _n :: Index 10}
  deriving (Generic, NFDataX, Eq, Show)

-- | State transition function for 'bitSlip', where the initial state is the
--   training state, and after 8 consecutive commas have been detected at the
--   same index in the status register it moves into the 'BSOk' state where the
--   recovered index is used to shift the output 'BitVector'
bitSlipT ::
  -- | Current state
  BitSlipState ->
  -- | New input values
  (Cg, SyncStatus) ->
  -- | New state
  BitSlipState
bitSlipT BSFail{..} (cg, _)
  | isNothing n = BSFail s ns hist
  | _ns == repeat (fromJust n) = BSOk s (fromJust n)
  | otherwise = BSFail s ns hist
 where
  s = resize $ _s ++# reverseBV cg
  ns = maybe _ns (_ns <<+) n
  hist = map pack $ take d10 $ windows1d d10 $ bv2v s

  n = elemIndex True $ map f _hist
   where
    f a = a == reverseBV cgK28_5N || a == reverseBV cgK28_5P
bitSlipT BSOk{..} (cg, syncStatus)
  | syncStatus == Fail = BSFail s (repeat _n) (repeat 0)
  | otherwise = BSOk s _n
 where
  s = resize $ _s ++# reverseBV cg

-- | Output function for 'bitSlip' that takes the calculated index value and
--   rotates the state vector to create the new output value, or outputs the
--   input directly when no such index value has been found yet.
bitSlipO ::
  -- | Current state
  BitSlipState ->
  -- | New output value
  (BitSlipState, Cg, Bool)
bitSlipO self =
  (self, reverseBV $ resize $ rotateR self._s (10 - fromEnum n), bsOk)
 where
  (n, bsOk) = case self of
    BSFail{} -> (last self._ns, False)
    BSOk{} -> (self._n, True)

-- | Function that takes a code word and returns the same code word, but if a
--   comma is detected the code words is shifted such that the comma is at the
--   beginning of the next code word to achieve word-alignment.
bitSlip ::
  forall dom.
  (HiddenClockResetEnable dom) =>
  -- | Input code group
  Signal dom Cg ->
  -- | Current sync status from 'Sgmii.sync'
  Signal dom SyncStatus ->
  -- | Output code group
  (Signal dom Cg, Signal dom Bool)
bitSlip cg1 syncStatus = (register 0 cg2, register False bsOk)
 where
  (_, cg2, bsOk) =
    mooreB
      bitSlipT
      bitSlipO
      (BSFail 0 (repeat 0) (repeat 0))
      (cg1, syncStatus)

{-# CLASH_OPAQUE bitSlip #-}
