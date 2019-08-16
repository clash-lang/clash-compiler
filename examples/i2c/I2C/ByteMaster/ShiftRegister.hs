{-# LANGUAGE RecordWildCards #-}
module I2C.ByteMaster.ShiftRegister where

import Clash.Prelude

import Control.Lens hiding (Index)
import Control.Monad.State

data ShiftRegister
  = ShiftRegister
  { _sr   :: Vec 8 Bit
  , _dcnt :: SatIndex 'SatError 8
  } deriving (Generic, NFDataX)

makeLenses ''ShiftRegister

{-# INLINE shiftStartState #-}
shiftStartState
  = ShiftRegister
  { _sr   = repeat low
  , _dcnt = 0
  }

shiftRegister :: Bool
              -> Bool
              -> Bool
              -> Vec 8 Bit
              -> Bit
              -> State ShiftRegister Bool
shiftRegister rst ld shiftsr din coreRxd = do
  (ShiftRegister {..}) <- get

  if rst then do
    sr   .= repeat low
    dcnt .= 0
  else if ld then do
    sr   .= din
    dcnt .= 7
  else when shiftsr $ do
    sr   .= (_sr <<+ coreRxd)
    dcnt -= 1

  return (_dcnt == 0)
