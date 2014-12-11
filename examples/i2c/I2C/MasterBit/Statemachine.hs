{-# LANGUAGE RecordWildCards #-}
module I2C.MasterBit.Statemachine
  ( BitStateMachine (..)
  , StateMachine (..)
  , nextStateDecoder
  , stateMachineStart
  )
where

import CLaSH.Prelude
import Control.Lens
import Control.Monad.State

import I2C.Types

data BitStateMachine
  = Idle |
    StartA | StartB | StartC | StartD | StartE |
    StopA | StopB | StopC | StopD |
    ReadA | ReadB | ReadC | ReadD |
    WriteA | WriteB | WriteC | WriteD
  deriving Eq

data StateMachine
  = StateMachine
  { _isclOen, _isdaOen   :: Bool        -- Internal I2C lines
  , _sdaChk              :: Bool        -- check SDA status (multi-master arbiter)
  , _cmdAck              :: Bool        -- command completed
  , _cState              :: BitStateMachine
  }

makeLenses ''StateMachine

{-# INLINE stateMachineStart #-}
stateMachineStart
  = StateMachine
  { _isclOen = True
  , _isdaOen = True
  , _sdaChk  = False
  , _cmdAck  = False
  , _cState  = Idle
  }

-- topEntity a b c s = runState (nextStateDecoder a b c) s

{-# NOINLINE nextStateDecoder #-}
nextStateDecoder :: I2Ccommand
                 -> Bool
                 -> Bit
                 -> State StateMachine ()
nextStateDecoder cmd clkEn dIn = do
  (StateMachine {..}) <- get

  -- default no acknowledge
  cmdAck .= False

  when clkEn $ do
    case _cState of
      -- idle
      Idle -> do
          case cmd of
            I2C_Start -> cState .= StartA
            I2C_Stop  -> cState .= StopA
            I2C_Write -> cState .= WriteA
            I2C_Read  -> cState .= ReadA
            _         -> cState .= Idle -- NOP command

          sdaChk .= False

      -- start
      StartA -> do
          cState  .= StartB
          isdaOen .= True  -- set SDA high
          sdaChk  .= False -- don't check SDA

      StartB -> do
          cState  .= StartC
          isclOen .= True  -- set SCL high
          isdaOen .= True  -- keep SDA high
          sdaChk  .= False -- don't check SDA

      StartC -> do
          cState  .= StartD
          isclOen .= True  -- keep SCL high
          isdaOen .= False -- set SDA low
          sdaChk  .= False -- don't check SDA

      StartD -> do
          cState  .= StartE
          isclOen .= True  -- keep SCL high
          isdaOen .= False -- keep SDA low
          sdaChk  .= False -- don't check SDA

      StartE -> do
          cState  .= Idle
          cmdAck  .= True  -- command completed
          isclOen .= False -- set SCL low
          sdaChk  .= False -- don't check SDA

      -- stop
      StopA -> do
          cState  .= StopB
          isclOen .= False -- keep SCL low
          isdaOen .= False -- set SDA low
          sdaChk  .= False -- don't check SDA

      StopB -> do
          cState  .= StopC
          isclOen .= True -- set SCL high
          isdaOen .= False -- keep SDA low
          sdaChk  .= False -- don't check SDA

      StopC -> do
          cState  .= StopD
          isclOen .= True -- keep SCL high
          isdaOen .= False -- keep SDA low
          sdaChk  .= False -- don't check SDA

      StopD -> do
          cState  .= Idle
          cmdAck .= True -- command completed
          isclOen .= True -- keep SCL high
          isdaOen .= True -- set SDA high
          sdaChk  .= False -- don't check SDA

      -- read
      ReadA -> do
          cState  .= ReadB
          isclOen .= False -- keep SCL low
          isdaOen .= True -- tri-state SDA
          sdaChk  .= False -- don't check SDA

      ReadB -> do
          cState  .= ReadC
          isclOen .= True -- set SCL high
          isdaOen .= True -- tri-state SDA
          sdaChk  .= False -- don't check SDA

      ReadC -> do
          cState  .= ReadD
          isclOen .= True -- keep SCL high
          isdaOen .= True -- tri-state SDA
          sdaChk  .= False -- don't check SDA

      ReadD -> do
          cState  .= Idle
          cmdAck  .= True -- command completed
          isclOen .= False -- set SCL low
          isdaOen .= True -- tri-state SDA
          sdaChk  .= False -- don't check SDA

      -- write
      WriteA -> do
          cState  .= WriteB
          isclOen .= False -- keep SCL low
          isdaOen .= (dIn == 1) -- set SDA
          sdaChk  .= False -- don't check SDA (SCL low)

      WriteB -> do
          cState  .= WriteC
          isclOen .= True -- set SCL high
          isdaOen .= (dIn == 1) -- keep SDA
          sdaChk  .= False -- don't check SDA yet
                           -- Allow some more time for SDA and SCL to settle

      WriteC -> do
          cState  .= WriteD
          isclOen .= True -- keep SCL high
          isdaOen .= (dIn == 1) -- keep SDA
          sdaChk  .= True -- check SDA

      WriteD -> do
          cState  .= Idle
          cmdAck  .= True -- command completed
          isclOen .= False -- set SCL low
          isdaOen .= (dIn == 1) -- keep SDA
          sdaChk  .= False -- don't check SDA (SCL low)
