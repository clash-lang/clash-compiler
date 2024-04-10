{-|
  Copyright   :  (C) 2014, University of Twente
                     2024, Google LLC
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
module Clash.Cores.I2C.ByteMaster
  ( byteMaster
  , ByteMasterI
  , ByteMasterO
  , I2COperation(..)
  ) where

import Clash.Prelude

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Tuple

import Clash.Cores.I2C.ByteMaster.ShiftRegister
import Clash.Cores.I2C.Types
import Data.Maybe (fromJust)

data ByteStateMachine = Idle | Active | Start | Read | Write | Ack | Stop
  deriving (Show, Generic, NFDataX, Eq)

data I2COperation = ReadData | WriteData (BitVector 8)
  deriving (Generic, NFDataX, BitPack)

getWriteData :: I2COperation -> BitVector 8
getWriteData ReadData = 0
getWriteData (WriteData d) = d

data ByteMasterS
  = ByteS
  { _srState    :: ShiftRegister
  , _byteStateM :: ByteStateMachine
  , _coreCmd    :: I2CCommand
  , _coreTxd    :: Bit
  , _shiftsr    :: Bool
  , _ld         :: Bool
  }
  deriving (Generic, NFDataX, Eq)

makeLenses ''ByteMasterS

-- |
-- 1. Statemachine reset
-- 2. Claim bus
-- 3. Bus operation
-- 4. Acknowledge signal to be transmitted from master to slave on read operations.
--    True means SDA is low.
-- 5. Bitmaster response
type ByteMasterI = (Bool,Bool,Maybe I2COperation,Bool,BitRespSig)

-- |
-- 1. Acknowledge for `I2COperation`
-- 2. Received acknowledge signal from slave to master on write operations. True
--    means SDA is low.
-- 3. Data output
-- 4  Bitmaster control signals
type ByteMasterO = (Bool,Bool,BitVector 8,BitCtrlSig)

-- | Byte level controller, takes care of correctly executing i2c communication
-- based on the supplied control signals. It should be instantiated alongside
-- 'Clash.Cores.I2C.BitMaster.bitMaster'. The outgoing 'BitCtrlSig' controls the
-- 'Clash.Cores.I2C.BitMaster.bitMaster. whose 'BitRespSig' should be supplied as last
-- input.
byteMaster
  :: KnownDomain dom
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Unbundled dom ByteMasterI
  -> Unbundled dom ByteMasterO
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
  }

byteMasterT :: ByteMasterS -> ByteMasterI -> (ByteMasterS, ByteMasterO)
byteMasterT s@(ByteS {_srState = ShiftRegister {..}, ..})
            (rst,claimBus,maybeI2COp,ackRead,~(coreAck,al,coreRxd)) = swap $ flip runState s $ do

      -- assign dOut the output of the shift-register
  let dout = _sr

  cntDone <- zoom srState (shiftRegister rst _ld _shiftsr (bv2v (getWriteData $ fromJust maybeI2COp )) coreRxd)

  -- state machine
  coreTxd   .= head dout
  shiftsr   .= False
  ld        .= False

  if rst || al then do
    coreCmd    .= I2Cnop
    coreTxd    .= low
    byteStateM .= Idle
  else case (_byteStateM, maybeI2COp) of
    (Idle, _) -> when claimBus $ do
      ld .= True
      byteStateM .= Start
      coreCmd    .= I2Cstart
    (Active, Just ReadData) -> do
      ld .= True
      byteStateM .= Read
      coreCmd    .= I2Cread
    (Active, Just (WriteData _)) -> do
      ld .= True
      byteStateM .= Write
      coreCmd    .= I2Cwrite
    (Active ,Nothing) ->
      if claimBus then do
        byteStateM .= Active
        coreCmd    .= I2Cnop
      else do
        byteStateM .= Stop
        coreCmd    .= I2Cstop
    (Start, Nothing) -> when coreAck $ do
      byteStateM .= Active
      coreCmd    .= I2Cnop
    (Start, Just ReadData) -> when coreAck $ do
      byteStateM .= Read
      coreCmd    .= I2Cread
    (Start, Just (WriteData _)) -> when coreAck $ do
      ld .= True
      byteStateM .= Write
      coreCmd    .= I2Cwrite
    (Write, _) -> when coreAck $ do
      if cntDone then do
        byteStateM .= Ack
        coreCmd    .= I2Cread
      else do
        coreCmd    .= I2Cwrite
        shiftsr    .= True

    (Read, _) -> when coreAck $ do
      shiftsr .= True
      coreTxd .= bitCoerce (not ackRead)
      if cntDone then do
        byteStateM .= Ack
        coreCmd    .= I2Cwrite
      else do
        coreCmd    .= I2Cread

    (Ack, _) ->
      if coreAck then do
        coreTxd    .= high
        -- check for stop; Should a STOP command be generated?
        if claimBus then do
          byteStateM .= Active
          coreCmd    .= I2Cnop
        else do
          byteStateM .= Stop
          coreCmd    .= I2Cstop
      else
        coreTxd .= bitCoerce (not ackRead)

    (Stop, _) -> when coreAck $ do
      byteStateM .= Idle
      coreCmd    .= I2Cnop

  let
    bitCtrl  = (_coreCmd,_coreTxd)
    i2cOpAck = (_byteStateM == Ack) && coreAck
    ackWrite = i2cOpAck && not (bitCoerce coreRxd)
    outp     = (i2cOpAck,ackWrite,v2bv dout,bitCtrl)

  return outp
