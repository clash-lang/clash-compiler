{-|
  Copyright   :  (C) 2014, University of Twente
                     2024, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Clash.Cores.I2C.BitMaster.StateMachine where

import Clash.Prelude
import Control.Lens hiding (Index)
import Control.Monad
import Control.Monad.State

import Clash.Cores.I2C.Types

-- | States for bit-level I2C operations.
data BitStateMachine
  = Idle          -- ^ Idle state
  | Start (Index 5) -- ^ Start condition state
  | Stop  (Index 4) -- ^ Stop condition state
  | Read  (Index 4) -- ^ Read operation state
  | Write (Index 4) -- ^ Write operation state
  deriving (Eq, Generic, NFDataX)

-- | Defines the state machine with control and status registers.
data StateMachine
  = StateMachine
  { _sclOen    :: Bool            -- ^ Inverted SCL output enable, False pulls the scl low.
  , _sdaOen    :: Bool            -- ^ Inverted SDA output enable, False pulls the sda low.
  , _sdaChk    :: Bool            -- ^ Checks SDA status
  , _cmdAck    :: Bool            -- ^ Acknowledges command completion
  , _bitStateM :: BitStateMachine -- ^ Current state of the bit-level state machine
  } deriving (Generic, NFDataX)

makeLenses ''StateMachine

-- | Initial state of the state machine.
stateMachineStart :: StateMachine
stateMachineStart
  = StateMachine
  { _sclOen    = True  -- ^ SCL output enabled by default
  , _sdaOen    = True  -- ^ SDA output enabled by default
  , _sdaChk    = False -- ^ SDA status check disabled by default
  , _cmdAck    = False -- ^ Command acknowledgment flag set to false
  , _bitStateM = Idle  -- ^ Initial state set to Idle
  }

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE bitStateMachine #-}
-- | Bit level I2C state machine that manages transitions between various states from
-- 'StateMachine' based on the input parameters and the current state.
--
-- * In the 'Start' state, the function initiates the start condition on the I2C bus.
-- * In the 'Stop' state, it initiates the stop condition, releasing the bus.
-- * In the 'Read' state, it reads a bit from the slave device.
-- * In the 'Write' state, it writes a bit to the slave device.
--
-- The function ensures that the state transitions are compliant with the I2C protocol.
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
                       _         -> Idle
        sdaChk    .= False
