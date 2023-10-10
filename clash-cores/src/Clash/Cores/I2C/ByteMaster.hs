{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Clash.Cores.I2C.ByteMaster (byteMaster) where

import Clash.Prelude hiding (read)

import Control.Lens hiding (Index)
import Control.Monad
import Control.Monad.Trans.State
import Data.Tuple

import Clash.Cores.I2C.ByteMaster.ShiftRegister
import Clash.Cores.I2C.Types

data ByteStateMachine = Idle | Start | Read | Write | Ack | Stop
  deriving (Show, Generic, NFDataX)

data ByteMasterS
  = ByteS
  { _srState    :: ShiftRegister
  , _byteStateM :: ByteStateMachine -- State machine
  , _coreCmd    :: I2CCommand       -- coreCmd register
  , _coreTxd    :: Bit              -- coreTxd register
  , _shiftsr    :: Bool             -- shift sr
  , _ld         :: Bool             -- load values in to sr
  , _hostAck    :: Bool             -- host cmd acknowlegde register
  , _ackOut     :: Bool             -- slave ack register
  }
  deriving (Generic, NFDataX)

makeLenses ''ByteMasterS

-- |
-- 1. Statemachine reset
-- 2. Start
-- 3. Stop
-- 4. Read
-- 5. Write
-- 6. Acknowledge
-- 7. Data in
-- 8. Bitmaster response
type ByteMasterI = (Bool,Bool,Bool,Bool,Bool,Bool,BitVector 8,BitRespSig)

-- |
-- 1. Acknowledge for I2C controller
-- 2. I2C acknowledgement
-- 3. Data output
-- 4  Bitmaster control signals
type ByteMasterO = (Bool,Bool,BitVector 8,BitCtrlSig)

{-# ANN byteMaster
  (Synthesize
    { t_name     = "bytemaster"
    , t_inputs   = [ PortName "clk"
                   , PortName "arst"
                   , PortName "gen"
                   , PortProduct ""
                      [ PortName "rst"
                      , PortName "start"
                      , PortName "stop"
                      , PortName "read"
                      , PortName "write"
                      , PortName "ackIn"
                      , PortName "din"
                      , PortName "bitResp" ]
                   ]
    , t_output   = PortProduct ""
                     [ PortName "hostAck"
                     , PortName "ackOut"
                     , PortName "dout"
                     , PortName "bitCtrl"
                     ]
    }) #-}
-- | Byte level controller, takes care of correctly executing i2c communication
-- based on the supplied control signals. It should be instantiated alongside 'bitMaster'.
-- The outgoing bitCtrl' controls the 'bitMaster' whose 'bitResp' should be supplied
-- as last input.
byteMaster
  :: Clock System
  -> Reset System
  -> Enable System
  -> Unbundled System ByteMasterI
  -> Unbundled System ByteMasterO
byteMaster = exposeClockResetEnable (mealyB byteMasterT byteMasterInit)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE byteMaster #-}

{-# INLINE byteMasterInit #-}
byteMasterInit :: ByteMasterS
byteMasterInit
  = ByteS
  { _srState    = shiftStartState
  , _byteStateM = Idle
  , _coreCmd    = I2Cnop
  , _coreTxd    = low
  , _shiftsr    = False
  , _ld         = False
  , _hostAck    = False
  , _ackOut     = True
  }

byteMasterT :: ByteMasterS -> ByteMasterI -> (ByteMasterS, ByteMasterO)
byteMasterT s@(ByteS {_srState = ShiftRegister {..}, ..})
            (rst,start,stop,read,write,ackIn,din,~(coreAck,al,coreRxd)) = swap $ flip runState s $ do
      -- generate go-signal
  let go = (read || write || stop) && (not _hostAck)

      -- assign dOut the output of the shift-register
      dout = _sr

  cntDone <- zoom srState (shiftRegister rst _ld _shiftsr (bv2v din) coreRxd)

  -- state machine
  coreTxd .= head dout
  shiftsr .= False
  ld      .= False
  hostAck .= False

  if rst || al then do
    coreCmd    .= I2Cnop
    coreTxd    .= low
    byteStateM .= Idle
    ackOut     .= True
  else case _byteStateM of
    Idle -> when go $ do
      ld .= True
      if start then do
        byteStateM .= Start
        coreCmd    .= I2Cstart
      else if read then do
        byteStateM .= Read
        coreCmd    .= I2Cread
      else if write then do
        byteStateM .= Write
        coreCmd    .= I2Cwrite
      else do-- stop
        byteStateM .= Stop
        coreCmd    .= I2Cstop
    Start -> when coreAck $ do
      ld .= True
      if read then do
        byteStateM .= Read
        coreCmd    .= I2Cread
      else do
        byteStateM .= Write
        coreCmd    .= I2Cwrite
    Write -> when coreAck $ do
      if cntDone then do
        byteStateM .= Ack
        coreCmd    .= I2Cread
      else do
        coreCmd    .= I2Cwrite
        shiftsr    .= True
    Read -> when coreAck $ do
      shiftsr .= True
      coreTxd .= bitCoerce ackIn
      if cntDone then do
        byteStateM .= Ack
        coreCmd    .= I2Cwrite
      else do
        coreCmd    .= I2Cread
    Ack -> if coreAck then do
        ackOut  .= bitCoerce coreRxd
        coreTxd .= high
        -- check for stop; Should a STOP command be generated?
        if stop then do
          byteStateM .= Stop
          coreCmd    .= I2Cstop
        else do
          byteStateM .= Idle
          coreCmd    .= I2Cnop
          -- generate command acknowledge signal
          hostAck    .= True
      else
        coreTxd .= bitCoerce ackIn
    Stop -> when coreAck $ do
      byteStateM .= Idle
      coreCmd    .= I2Cnop
      hostAck    .= True

  let bitCtrl = (_coreCmd,_coreTxd)
      outp    = (_hostAck,_ackOut,v2bv dout,bitCtrl)

  return outp
