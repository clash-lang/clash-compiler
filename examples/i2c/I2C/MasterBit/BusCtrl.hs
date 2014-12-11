{-# LANGUAGE RecordWildCards #-}
module I2C.MasterBit.BusCtrl
  ( BusStatusCtrl (..)
  , busStatusCtrl
  , busStartState
  )
where

import CLaSH.Prelude
import Control.Lens
import Control.Monad.State

import I2C.MasterBit.Statemachine
import I2C.Types

data BusStatusCtrl
  = BusStatusCtrl
  { _cI2C           :: Vec 2 I2CIPair -- capture SDA and SCL
  , _filterCnt      :: Unsigned 14    -- clock divider for filter
  , _fI2C           :: Vec 3 I2CIPair -- filtered inputs for SCL and SDA
  , _sI2C           :: I2CIPair       -- synchronised SCL and SDA
  , _dI2C           :: I2CIPair       -- delayed version of 'sI2C'
  , _startCondition :: Bool           -- start detected
  , _stopCondition  :: Bool           -- stop detected
  , _ibusy          :: Bool           -- internal busy signal
  , _cmdStop        :: Bool           -- STOP command
  , _ial            :: Bool           -- internal arbitration lost signal
  }

makeLenses ''BusStatusCtrl

{-# INLINE busStartState #-}
busStartState
  = BusStatusCtrl
  { _cI2C           = repeat (I2CIPair 0 0)
  , _filterCnt      = 0
  , _fI2C           = repeat (I2CIPair 1 1)
  , _sI2C           = (I2CIPair 1 1)
  , _dI2C           = (I2CIPair 1 1)
  , _startCondition = False
  , _stopCondition  = False
  , _ibusy          = False
  , _cmdStop        = False
  , _ial            = False
  }

{-# NOINLINE busStatusCtrl #-}
busStatusCtrl :: Unsigned 16
              -> I2Ccommand
              -> Bool
              -> Bool
              -> Bool
              -> BitStateMachine
              -> I2CIPair
              -> State BusStatusCtrl ()
busStatusCtrl clkCnt cmd sdaChk isdaOen clkEn cState i2ci  = do
    (BusStatusCtrl {..}) <- get

    -- capture SCL and SDA
    cI2C .= (i2ci +>> _cI2C)

    -- filter SCL and SDA (attempt to) remove glitches
    if _filterCnt == 0 then do
       filterCnt .= unpack (fst (split clkCnt))
    else do
       filterCnt -= 1

    when (_filterCnt == 0) $
      fI2C .= (last _cI2C +>> _fI2C)

    -- generated filtered SCL and SDA signals
    (sI2C.scli) .= filterT (map _scli _fI2C)
    (sI2C.sdai) .= filterT (map _sdai _fI2C)

    -- Delayed version of sI2C
    dI2C .= _sI2C

    let sSDA = _sdai _sI2C
        dSDA = _sdai _dI2C
        sSCL = _scli _sI2C

    -- zoom busState $ do
    -- detect start condition -> do detect falling edge on SDA while SCL is high
    -- detect stop condition -> do detect rising edge on SDA while SCL is high
    startCondition .= ((sSDA == 0 && dSDA == 1) && (sSCL == 1))
    stopCondition  .= ((sSDA == 1 && dSDA == 0) && (sSCL == 1))

    -- generate i2c-bus busy signal
    ibusy .= ((_startCondition || _ibusy) && not _stopCondition)

    -- generate arbitration lost signal
    -- arbitration lost when:
    -- 1) master drives SDA high, but the i2c bus is low
    -- 2) stop detected while not requested (detect during 'idle' state)
    when clkEn (cmdStop .= (cmd == I2C_Stop))

    let masterHighBusLow = sdaChk && sSDA == 0 && isdaOen
    if cState == Idle then
       ial .= (masterHighBusLow || (_stopCondition && not _cmdStop))
    else
       ial .= masterHighBusLow
  where
    filterT f = (f!!2 .&. f!!1) .|.
                (f!!2 .&. f!!0) .|.
                (f!!1 .&. f!!0)
