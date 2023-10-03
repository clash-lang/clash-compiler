{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Clash.Cores.I2C.BitMaster.BusCtrl where

import Clash.Prelude
import Control.Lens
import Control.Monad
import Control.Monad.State

import Clash.Cores.I2C.BitMaster.StateMachine
import Clash.Cores.I2C.Types

data BusStatusCtrl
  = BusStatusCtrl
  { _sI2C           :: I2CIn       -- synchronized SCL and SDA
  , _dI2C           :: I2CIn       -- delayed sI2C
  , _al             :: Bool        -- internal arbitration lost signal
  , _cI2C           :: Vec 2 I2CIn -- capture SCL and SDA
  , _fI2C           :: Vec 3 I2CIn -- filter input for SCL and SDA
  , _filterCnt      :: Unsigned 14 -- clock divider for filter
  , _startCondition :: Bool        -- start detected
  , _stopCondition  :: Bool        -- stop detected
  , _busy           :: Bool        -- internal busy signal
  , _cmdStop        :: Bool        -- STOP command
  } deriving (Generic, NFDataX)

makeLenses ''BusStatusCtrl

{-# INLINE busStartState #-}
busStartState
  = BusStatusCtrl
  { _sI2C           = (high,high)        -- synchronized SCL and SDA input
  , _dI2C           = (high,high)        -- delayed sI2C
  , _al             = False              -- internal arbitration lost signal
  , _cI2C           = repeat (high,high) -- capture SCL and SDA
  , _fI2C           = repeat (high,high) -- filter input for SCL and SDA
  , _filterCnt      = 0                  -- clock divider for filter
  , _startCondition = False              -- start detected
  , _stopCondition  = False              -- stop detected
  , _busy           = False              -- internal busy signal
  , _cmdStop        = False              -- STOP command
  }

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE busStatusCtrl #-}
busStatusCtrl :: Bool
              -> Bool
              -> Unsigned 16
              -> I2CCommand
              -> Bool
              -> I2CIn
              -> BitStateMachine
              -> Bool
              -> Bool
              -> State BusStatusCtrl ()
busStatusCtrl rst ena clkCnt cmd clkEn i2cI bitStateM sdaChk sdaOen = do
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
  let masterHighBusLow = sdaChk && sSDA == low && sdaOen
  if rst then do
    cmdStop .= False
    al      .= False
  else do
    when clkEn $
      cmdStop .= (cmd == I2Cstop)
    if bitStateM == Idle then
      al .= (masterHighBusLow || (_stopCondition && (not _cmdStop)))
    else
      al .= masterHighBusLow
  where
    filterT f = (f!!2 .&. f!!1) .|.
                (f!!2 .&. f!!0) .|.
                (f!!1 .&. f!!0)
