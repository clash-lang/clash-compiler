{-# LANGUAGE RecordWildCards #-}
module I2C.MasterByte.ShiftRegister
  ( ShiftRegister(..)
  , shiftStartState
  , shiftRegister
  )
where

import CLaSH.Prelude
import Control.Lens hiding (Index)
import Control.Monad.State

data ShiftRegister
  = ShiftRegister
  { _sr   :: BitVector 8
  , _dCnt :: Index 8
  }

makeLenses ''ShiftRegister

shiftStartState
  = ShiftRegister
  { _sr = 0
  , _dCnt = 0
  }

{-# NOINLINE shiftRegister #-}
shiftRegister :: Bool
              -> Bool
              -> BitVector 8
              -> Bit
              -> State ShiftRegister Bool
shiftRegister ld shift dIn coreRxd = do
  (ShiftRegister {..}) <- get

  -- shift register
  if ld then
    sr .= dIn
  else when shift $
    sr .= _sr <<# coreRxd

  -- data-counter
  if ld then
    dCnt .= 7
  else when shift $
    dCnt -= 1

  return (_dCnt == 0)

(<<#) :: KnownNat n => BitVector (1 + n) -> Bit -> BitVector (n + 1)
bv <<# b = snd (split bv) ++# b
