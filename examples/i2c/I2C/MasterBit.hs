{-# LANGUAGE RecordWildCards #-}
module I2C.MasterBit
  (masterBitCtrl
  )
where

import CLaSH.Prelude
import Control.Lens
import Control.Lens.Zoom
import Control.Monad.State

import I2C.MasterBit.BusCtrl
import I2C.MasterBit.Statemachine
import I2C.Types

data MasterBitS
  = MasterBitS
  { _dsclOen             :: Bool        -- delayed sclOen signal
  , _slaveWait           :: Bool        -- clock generation signals
  , _cnt                 :: Unsigned 16 -- clock devider counter
  , _clkEn               :: Bool        -- statemachine clock enable
  , _dout                :: Bit
  , _busState            :: BusStatusCtrl
  , _stateMachine        :: StateMachine
  }

makeLenses ''MasterBitS

{-# NOINLINE masterBitCtrl #-}
masterBitCtrl :: Unsigned 16
              -> Signal I2Ccommand
              -> Signal Bit
              -> Signal I2CIPair
              -> (Signal (Bool,Bool,Bool,Bit), Signal I2COPair)
masterBitCtrl clkCnt cmd dIn i2ci = unbundle o
  where
    s      = register masterStartState s'
    (o,s') = unbundle (masterBitCtrlT clkCnt <$> cmd <*> dIn <*> i2ci <*> s)


{-# INLINE masterStartState #-}
masterStartState :: MasterBitS
masterStartState
  = MasterBitS
  { _dsclOen      = False
  , _slaveWait    = False
  , _cnt          = 0
  , _clkEn        = True
  , _dout         = 0
  , _busState     = busStartState
  , _stateMachine = stateMachineStart
  }


masterBitCtrlT :: Unsigned 16
               -> I2Ccommand
               -> Bit
               -> I2CIPair
               -> MasterBitS
               -> ( ((Bool,Bool,Bool,Bit),I2COPair)
                  , MasterBitS)
masterBitCtrlT clkCnt cmd dIn i2ci = runState $ do
  (MasterBitS {..}) <- get
  -- Extract substate information
  let sSCL      = _scli (_sI2C _busState)
      dSCL      = _scli (_dI2C _busState)
      sSDA      = _sdai (_sI2C _busState)
      al        = _ial _busState
      busy      = _ibusy _busState
      (StateMachine isclOen isdaOen sdaChk cmdAck cState) = _stateMachine

  -- whenever the slave is not ready it can delay the cycle by pulling SCL low
  -- delay scl_oen
  dsclOen .= isclOen

  -- slave_wait is asserted when master wants to drive SCL high, but the slave pulls it low
  -- slave_wait remains asserted until the slave releases SCL
  let masterSclHigh = isclOen && not _dsclOen
  slaveWait .= ((masterSclHigh || _slaveWait) && sSCL == 0)

  -- master drives scl high, but another master pulls it low
  -- master start counting down its low cycle now (clock synchronisation)
  let sclSync = sSCL == 1 &&
                dSCL == 0 &&
                isclOen

  -- generate clk enable signal
  if sclSync then do
     cnt .= clkCnt
     clkEn .= True
  else if _slaveWait then do
     clkEn .= False
  else do
     cnt -= 1
     clkEn .= False

  -- generate bus status controller
  zoom busState (busStatusCtrl clkCnt cmd sdaChk isdaOen _clkEn cState i2ci)

  -- generate dout signal, store out on rising edge of SCL
  when (sSCL == 1 && dSCL == 0) (dout .= sSDA)

  -- generate statemachine
  zoom stateMachine (nextStateDecoder cmd _clkEn dIn)

  -- assign outputs
  return ( (cmdAck,al,busy,_dout)
         , I2COPair 0 isclOen 0 isdaOen
         )
