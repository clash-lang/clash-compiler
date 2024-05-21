{-|
  Copyright   :  (C) 2014, University of Twente
                     2024, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Clash.Cores.Experimental.I2C.BitMaster.BusCtrl
  ( busStatusCtrl
  , BusStatusCtrl(..)
  , busStartState
  ) where

import Clash.Prelude
import Control.Lens
import Control.Monad
import Control.Monad.State

import Clash.Cores.Experimental.I2C.BitMaster.StateMachine
import Clash.Cores.Experimental.I2C.Types

-- | Bus status control state.
data BusStatusCtrl
  = BusStatusCtrl
  { _sI2C           :: I2CIn       -- ^ Synchronized SCL and SDA
  , _dI2C           :: I2CIn       -- ^ Delayed sI2C
  , _al             :: Bool        -- ^ Internal arbitration lost signal
  , _cI2C           :: Vec 2 I2CIn -- ^ Capture SCL and SDA
  , _fI2C           :: Vec 3 I2CIn -- ^ Filter input for SCL and SDA
  , _filterCnt      :: Unsigned 14 -- ^ Clock divider for filter
  , _startCondition :: Bool        -- ^ Start detected
  , _stopCondition  :: Bool        -- ^ Stop detected
  , _busy           :: Bool        -- ^ Internal busy signal
  , _cmdStop        :: Bool        -- ^ Stop command
  } deriving (Generic, NFDataX)

makeLenses ''BusStatusCtrl

{-# INLINE busStartState #-}
busStartState :: BusStatusCtrl
busStartState
  = BusStatusCtrl
  { _sI2C           = (high,high)
  , _dI2C           = (high,high)
  , _al             = False
  , _cI2C           = repeat (high,high)
  , _fI2C           = repeat (high,high)
  , _filterCnt      = 0
  , _startCondition = False
  , _stopCondition  = False
  , _busy           = False
  , _cmdStop        = False
  }

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE busStatusCtrl #-}
-- | Low level bus status controller that monitors the state of the bus and performs
-- glitch filtering. It detects start conditions, stop conditions and arbitration loss.
busStatusCtrl
  :: Bool
  -- ^ Reset
  -> Bool
  -- ^ Enable
  -> Unsigned 16
  -- ^ Clock counter used for clock division
  -> I2CCommand
  -- ^ I2C command
  -> Bool
  -- ^ Clock enable
  -> I2CIn
  -- ^ SCL and SDA
  -> BitStateMachine
  -- ^ Current state of the bit-level state machine
  -> Bool
  -- ^ Checks SDA status
  -> Bool
  -- ^ Inverted SDA output enable, False pulls the sda low.
  -> State BusStatusCtrl ()
  -- ^ Bus status control state
busStatusCtrl rst ena clkCnt cmd clkEn i2cI bitStateM0 sdaChk0 sdaOen0 = do
  BusStatusCtrl {..} <- get

  -- capture SCL and SDA
  if rst then do
     cI2C .= repeat (low,low)
  else do
     cI2C .= (_cI2C <<+ i2cI)

  -- filter SCL and SDA; (attempt to remove glitches)
  filterCnt .= if rst || not ena then
                  0
               else if _filterCnt == 0 then
                  resize (shiftR clkCnt 2)
               else
                  _filterCnt - 1

  if rst then do
     fI2C .= repeat (high,high)
  else when (_filterCnt == 0) $ do
     fI2C .= (_fI2C <<+ head _cI2C)

  -- filtered SCL and SDA signal
  if rst then do
    sI2C .= (high,high)
  else do
    sI2C._1 .= filterT (map fst _fI2C)
    sI2C._2 .= filterT (map snd _fI2C)

  dI2C .= _sI2C

  let (sSCL,sSDA) = _sI2C
      dSDA        = snd _dI2C

  -- detect start condition => detect falling edge on SDA while SCL is high
  -- detect stop condition  => detect rising edge on SDA wile SCL is high
  if rst then do
    startCondition .= False
    stopCondition  .= False
  else do
    startCondition .= ((sSDA == low  && dSDA == high) && (sSCL == high))
    stopCondition  .= ((sSDA == high && dSDA == low ) && (sSCL == high))

  -- i2c busy signal
  busy .= if rst then False else (_startCondition || _busy) && (not _stopCondition)

  -- generate arbitration lost signal
  -- arbitration lost when:
  -- 1) master drives SDA high, but the i2c bus is low
  -- 2) stop detected while not requested (detect during 'idle' state)
  let masterHighBusLow = sdaChk0 && sSDA == low && sdaOen0
  if rst then do
    cmdStop .= False
    al      .= False
  else do
    when clkEn $
      cmdStop .= (cmd == I2Cstop)
    if bitStateM0 == Idle then
      al .= (masterHighBusLow || (_stopCondition && (not _cmdStop)))
    else
      al .= masterHighBusLow
  where
    filterT f = (f !! (2 :: Integer) .&. f !! (1 :: Integer)) .|.
                (f !! (2 :: Integer) .&. f !! (0 :: Integer)) .|.
                (f !! (1 :: Integer) .&. f !! (0 :: Integer))
