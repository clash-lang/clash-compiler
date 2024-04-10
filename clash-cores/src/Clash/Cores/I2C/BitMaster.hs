{-|
  Copyright   :  (C) 2014, University of Twente
                     2024, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Clash.Cores.I2C.BitMaster
  ( bitMaster
  , BitMasterI
  , BitMasterO
  ) where

import Clash.Prelude

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Tuple

import Clash.Cores.I2C.BitMaster.BusCtrl
import Clash.Cores.I2C.BitMaster.StateMachine
import Clash.Cores.I2C.Types

-- | Internal state of the I2C BitMaster.
data BitMasterS
  = BitS
  { _busState       :: BusStatusCtrl -- ^ Manage overall status of the I2C bus.
  , _stateMachine   :: StateMachine  -- ^ Handles the bit-level I2C operations
  , _dout           :: Bit           -- ^  Data to be sent out on the I2C bus
  , _dsclOen        :: Bool          -- ^ Delayed version of the SCL output enable signal
  , _clkEn          :: Bool          -- ^ Enable the clock for the state machine
  , _slaveWait      :: Bool          -- ^ Whether the slave is pulling the SCL line low, causing the master to wait
  , _cnt            :: Unsigned 16   -- ^ Counter used for clock division
  }
  deriving (Generic, NFDataX)

makeLenses ''BitMasterS


-- | 5-tuple containing the input interface for the BitMaster.
--
--  1. Resets the internal state when asserted
--  2. Enables or disables the BitMaster
--  3. Used for clock division
--  4. Carries command and data in signals
--  5. Contains the SCL and SDA input signals
type BitMasterI = (Bool,Bool,Unsigned 16,BitCtrlSig,I2CIn)

-- | 3-tuple containing the output interface for the BitMaster.
--
-- 1. Carries command acknowledgment and other flags
-- 2. Indicates if the BitMaster is currently busy
-- 3. Contains the SCL and SDA output signals
type BitMasterO = (BitRespSig,Bool,I2COut)

-- | Bit level I2C controller that contains a statemachine to properly:
--
-- * Monitor the bus for activity and arbitration.
-- * Read singular bits from the bus.
-- * Write singular bits to the bus.
-- * Return bits read from the bus.
bitMaster
  :: KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Unbundled dom BitMasterI
  -> Unbundled dom BitMasterO
bitMaster = exposeClockResetEnable (mealyB bitMasterT bitMasterInit)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE bitMaster #-}

bitMasterInit :: BitMasterS
bitMasterInit = BitS { _stateMachine   = stateMachineStart
                     , _busState       = busStartState
                     , _dout           = high
                     , _dsclOen        = False
                     , _clkEn          = True
                     , _slaveWait      = False
                     , _cnt            = 0
                     }


bitMasterT :: BitMasterS -> BitMasterI -> (BitMasterS, BitMasterO)
bitMasterT s@(BitS { _stateMachine = StateMachine  {..}
                   , _busState     = BusStatusCtrl {..}
                   , ..
                   })
                  (rst,ena,clkCnt,(cmd,din),i2cI@(_sclI,_sdaI)) =
                     swap $ flip runState s $ do
  -- Whenever the slave is not ready it can delay the cycle by pulling SCL low
  -- delay scloEn
  dsclOen .= _sclOen

  -- slaveWait is asserted when the master wants to drive SCL high, but the slave pulls it low
  -- slaveWait remains asserted until the slave releases SCL
  let masterSclHigh = _sclOen && not _dsclOen
      (sSCL,sSDA)   = _sI2C
  slaveWait .= ((masterSclHigh || _slaveWait) && sSCL == 0)

  -- master drives SCL high, but another master pulls it low
  -- master start counting down it low cycle now (clock synchronization)
  let dSCL    = fst _dI2C
      sclSync = dSCL == high && sSCL == low && _sclOen

  -- generate clk enable signal
  if rst || _cnt == 0 || not ena || sclSync then do
     cnt   .= clkCnt
     clkEn .= True
  else if _slaveWait then do
     clkEn .= False
  else do
     cnt   -= 1
     clkEn .= False

  -- bus status controller
  zoom busState (busStatusCtrl rst ena clkCnt cmd _clkEn i2cI _bitStateM _sdaChk _sdaOen)

  -- generate dout signal, store dout on rising edge of SCL
  when (sSCL == high && dSCL == low) $
    dout .= sSDA

  -- state machine
  zoom stateMachine (bitStateMachine rst _al _clkEn cmd din)

  -- assign outputs
  let
    i2cO =  (if _sclOen then Nothing else Just 0, if _sdaOen then Nothing else Just 0)
    outp = ((_cmdAck,_al,_dout),_busy,i2cO)

  return outp
