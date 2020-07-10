{-# LANGUAGE RecordWildCards #-}
module I2C.BitMaster.StateMachine where

import Clash.Prelude
import Control.Lens hiding (Index)
import Control.Monad.State

import I2C.Types

data BitStateMachine
  = Idle
  | Start (SatIndex 'SatError 5)
  | Stop  (SatIndex 'SatError 4)
  | Read  (SatIndex 'SatError 4)
  | Write (SatIndex 'SatError 4)
  deriving (Eq, Generic, NFDataX)

data StateMachine
  = StateMachine
  { _sclOen    :: Bool            -- i2c clock output enable register
  , _sdaOen    :: Bool            -- i2c data output enable register
  , _sdaChk    :: Bool            -- check SDA status (multi-master arbiter)
  , _cmdAck    :: Bool            -- command completed
  , _bitStateM :: BitStateMachine -- State Machine
  } deriving (Generic, NFDataX)

makeLenses ''StateMachine

{-# INLINE stateMachineStart #-}
stateMachineStart
  = StateMachine
  { _sclOen    = True
  , _sdaOen    = True
  , _sdaChk    = False
  , _cmdAck    = False
  , _bitStateM = Idle
  }

{-# NOINLINE bitStateMachine #-}
bitStateMachine :: Bool
                -> Bool
                -> Bool
                -> I2CCommand
                -> Bit
                -> State StateMachine ()
bitStateMachine rst al clkEn cmd din = do
  (StateMachine {..}) <- get
  cmdAck .= False

  if rst || al then do
    bitStateM .= Idle
    cmdAck    .= False
    sclOen    .= True
    sdaOen    .= True
    sdaChk    .= False
  else do
    when clkEn $ case _bitStateM of
      -- start
      Start 0 -> do
        bitStateM .= Start 1
        sdaOen    .= True   -- set SDA high
        sdaChk    .= False  -- don't check SDA

      Start 1 -> do
        bitStateM .= Start 2
        sclOen    .= True   -- set SCL high
        sdaOen    .= True   -- keep SDA high
        sdaChk    .= False  -- don't check SDA

      Start 2 -> do
        bitStateM .= Start 3
        sclOen    .= True   -- keep SCL high
        sdaOen    .= False  -- set SDA low
        sdaChk    .= False  -- don't check SDA

      Start 3 -> do
        bitStateM .= Start 4
        sclOen    .= True   -- keep SCL high
        sdaOen    .= False  -- keep SDA low
        sdaChk    .= False  -- don't check SDA

      Start 4 -> do
        bitStateM .= Idle
        cmdAck    .= True   -- command completed
        sclOen    .= False  -- set SCL low
        sdaOen    .= False  -- keep SDA low
        sdaChk    .= False  -- don't check SDA

      -- stop
      Stop 0 -> do
        bitStateM .= Stop 1
        sclOen    .= False  -- keep SCL low
        sdaOen    .= False  -- set SDA low
        sdaChk    .= False  -- don't check SDA

      Stop 1 -> do
        bitStateM .= Stop 2
        sclOen    .= True   -- set SCL high
        sdaOen    .= False  -- keep SDA low
        sdaChk    .= False  -- don't check SDA

      Stop 2 -> do
        bitStateM .= Stop 3
        sclOen    .= True   -- keep SCL high
        sdaOen    .= False  -- keep SDA low
        sdaChk    .= False  -- don't check SDA

      Stop 3 -> do
        bitStateM .= Idle
        cmdAck    .= True   -- command completed
        sclOen    .= True   -- keep SCL high
        sdaOen    .= True   -- set SDA high
        sdaChk    .= False  -- don't check SDA


      -- read
      Read 0 -> do
        bitStateM .= Read 1
        sclOen    .= False  -- keep SCL low
        sdaOen    .= True   -- tri-state SDA
        sdaChk    .= False  -- don't check SDA

      Read 1 ->
        do bitStateM .= Read 2
           sclOen    .= True   -- set SCL high
           sdaOen    .= True   -- tri-state SDA
           sdaChk    .= False  -- don't check SDA

      Read 2 -> do
        bitStateM .= Read 3
        sclOen    .= True   -- keep SCL high
        sdaOen    .= True   -- tri-state SDA
        sdaChk    .= False  -- don't check SDA

      Read 3 -> do
         bitStateM .= Idle
         cmdAck    .= True   -- command completed
         sclOen    .= False  -- set SCL low
         sdaOen    .= True   -- tri-state SDA
         sdaChk    .= False  -- don't check SDA

      -- write
      Write 0 -> do
        bitStateM .= Write 1
        sclOen    .= False         -- keep SCL low
        sdaOen    .= (din == high) -- set SDA
        sdaChk    .= False         -- don't check SDA (SCL low)

      Write 1 -> do
        bitStateM .= Write 2
        sclOen    .= True          -- set SCL high
        sdaOen    .= (din == high) -- keep SDA
        sdaChk    .= False         -- don't check SDA yet
                                   -- Allow some more time for SDA and SCL to settle
      Write 2 -> do
        bitStateM .= Write 3
        sclOen    .= True          -- keep SCL high
        sdaOen    .= (din == high) -- keep SDA
        sdaChk    .= True          -- check SDA

      Write 3 -> do
        bitStateM .= Idle
        cmdAck    .= True          -- command completed
        sclOen    .= False         -- set SCL low
        sdaOen    .= (din == high) -- keep SDA
        sdaChk    .= False         -- don't check SDA (SCL low)

      -- idle
      _ -> do
        bitStateM .= case cmd of
                       I2Cstart  -> Start 0
                       I2Cstop   -> Stop 0
                       I2Cwrite  -> Write 0
                       I2Cread   -> Read 0
                       otherwise -> Idle
        sdaChk    .= False
