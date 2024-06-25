{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Cores.Sgmii.BitSlip where

import Clash.Prelude
import Data.Maybe (fromJust, isNothing)

-- | State variable for 'bitSlip'
data BitSlipState n
  = BSFail
      { _s :: BitVector (2 * n)
      , _ns :: Vec 8 (Index n)
      , _hist :: Vec 10 (BitVector 7)
      }
  | BSOk {_s :: BitVector (2 * n), _n :: Index n}
  deriving (Generic, NFDataX, Eq, Show)

-- | State transition function for 'bitSlip', where the initial state is the
--   training state, and after a comma has been detected it moves into the
--   'BSOk' state where the recovered index is used to shift the output
--   'BitVector'
bitSlipT ::
  -- | Current state
  BitSlipState 10 ->
  -- | New input value
  BitVector 10 ->
  -- | New state
  BitSlipState 10
bitSlipT BSFail{..} cg = nextState
 where
  nextState
    | isNothing n = BSFail s ns hist
    | ns == repeat (fromJust n) = BSOk s (fromJust n)
    | otherwise = BSFail s ns hist

  s = resize $ _s ++# cg
  ns = maybe _ns (_ns <<+) n
  hist = map pack $ take d10 $ windows1d d7 $ bv2v s

  n = elemIndex True $ map f _hist
   where
    f a = a == 0b0011111 || a == 0b1100000
bitSlipT BSOk{..} cg = nextState
 where
  nextState = BSOk s _n

  s = resize $ _s ++# cg

-- | Output function for 'bitSlip' that takes the calculated index value and
--   rotates the state vector to create the new output value
bitSlipO ::
  -- | Current state
  BitSlipState 10 ->
  -- | New output value
  (BitSlipState 10, BitVector 10, Bool)
bitSlipO self@BSFail{..} = out
 where
  out = (self, resize $ rotateR _s (10 - fromEnum (last _ns)), False)
bitSlipO self@BSOk{..} = out
 where
  out = (self, resize $ rotateR _s (10 - fromEnum _n), True)

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
