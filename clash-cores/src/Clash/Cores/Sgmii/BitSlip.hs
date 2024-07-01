{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Sgmii.BitSlip where

import Clash.Prelude
import Data.Maybe (fromJust, isNothing)

-- | State variable for 'bitSlip'
data BitSlipState
  = BSFail
      { _s :: BitVector 20
      , _ns :: Vec 8 (Index 14)
      , _hist :: Vec 14 (BitVector 7)
      }
  | BSOk {_s :: BitVector 20, _n :: Index 14}
  deriving (Generic, NFDataX, Eq, Show)

-- | State transition function for 'bitSlip', where the initial state is the
--   training state, and after a comma has been detected it moves into the
--   'BSOk' state where the recovered index is used to shift the output
--   'BitVector'
bitSlipT ::
  -- | Current state
  BitSlipState ->
  -- | New input value
  BitVector 10 ->
  -- | New state
  BitSlipState
bitSlipT BSFail{..} cg = nextState
 where
  nextState
    | isNothing n = BSFail s ns hist
    | ns == repeat (fromJust n) = BSOk s (fromJust n)
    | otherwise = BSFail s ns hist

  s = resize $ _s ++# cg
  ns = maybe _ns (_ns <<+) n
  hist = map pack $ windows1d d7 $ bv2v s

  n = elemIndex True $ map f _hist
   where
    f a = a == 0b1111100 || a == 0b0000011
bitSlipT BSOk{..} cg = nextState
 where
  nextState = BSOk s _n

  s = resize $ _s ++# cg

-- | Output function for 'bitSlip' that takes the calculated index value and
--   rotates the state vector to create the new output value
bitSlipO ::
  -- | Current state
  BitSlipState ->
  -- | New output value
  (BitSlipState, BitVector 10, Bool)
bitSlipO self@BSFail{..} = out
 where
  out = (self, resize $ rotateR _s 10, False)
bitSlipO self@BSOk{..} = out
 where
  out = (self, resize $ rotateR _s (13 - fromEnum _n), True)

-- | Function that takes a code word and returns the same code word, but if a
--   comma is detected the code words is shifted such that the comma is at the
--   beginning of the next code word
bitSlip ::
  (HiddenClockResetEnable dom) =>
  -- Input code group
  Signal dom (BitVector 10) ->
  -- Output code group
  Signal dom (BitVector 10, Bool)
bitSlip cg1 = bundle (cg2, ok)
 where
  (_, cg2, ok) = mooreB bitSlipT bitSlipO (BSFail 0 (repeat 0) (repeat 0)) cg1

{-# CLASH_OPAQUE bitSlip #-}
