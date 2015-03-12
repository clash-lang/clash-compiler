{-# LANGUAGE RecordWildCards #-}
module I2C.MasterByte
  (masterByteCtrl)
where

import CLaSH.Prelude hiding (shift)
import Control.Lens
import Control.Monad.State

import I2C.MasterByte.ShiftRegister
import I2C.Types

data ByteStateMachine = Idle | Start | Read | Write | Ack | Stop
  deriving Show

data MachineState
  = MachineState
  { _cState   :: ByteStateMachine -- State machine
  , _coreCmd  :: I2Ccommand       -- coreCmd register
  , _coreTxd  :: Bit              -- coreTxd register
  , _shift    :: Bool             -- shift sr
  , _ld       :: Bool             -- load values in to sr
  , _hostAck  :: Bool             -- host cmd acknowlegde register
  , _ackOut   :: Bool             -- slave ack register
  , _srState  :: ShiftRegister
  }

makeLenses ''MachineState

{-# NOINLINE masterByteCtrl #-}
masterByteCtrl :: Signal Bool
               -> Signal Bool
               -> Signal Bool
               -> Signal Bool
               -> Signal Bool
               -> Signal (BitVector 8)
               -> Signal Bool
               -> Signal Bit
               -> ( Signal I2Ccommand
                  , Signal Bit
                  , Signal Bool
                  , Signal Bool
                  , Signal (BitVector 8)
                  )
masterByteCtrl startCmd stopCmd readCmd writeCmd ackIn dIn coreAck coreRxd =
    unbundle o
  where
    s      = register startState s'
    (o,s') = unbundle (masterByteCtrlT <$> startCmd <*> stopCmd <*> readCmd
                                       <*> writeCmd <*> ackIn   <*> dIn
                                       <*> coreAck  <*> coreRxd <*> s)

{-# INLINE startState #-}
startState :: MachineState
startState = MachineState
           { _cState    = Idle
           , _coreCmd   = I2C_NOP
           , _coreTxd   = 0
           , _shift     = False
           , _ld        = False
           , _hostAck   = False
           , _ackOut    = False
           , _srState = shiftStartState
           }

masterByteCtrlT :: Bool
                -> Bool
                -> Bool
                -> Bool
                -> Bool
                -> BitVector 8
                -> Bool
                -> Bit
                -> MachineState
                -> ((I2Ccommand,Bit,Bool,Bool,BitVector 8),MachineState)
masterByteCtrlT startCmd stopCmd readCmd writeCmd ackIn dIn coreAck coreRxd = runState $ do
  (MachineState {..}) <- get

  let cmdAck = _hostAck
      go     = (readCmd || writeCmd || stopCmd) && not _hostAck
      dOut   = _sr _srState

  cntDone <- zoom srState (shiftRegister _ld _shift dIn coreRxd)

  coreTxd .= msb dOut
  shift   .= False
  ld      .= False
  hostAck .= False

  case _cState of
    Idle -> when go $ do
                  if startCmd then do
                      cState  .= Start
                      coreCmd .= I2C_Start
                  else if readCmd then do
                      cState  .= Read
                      coreCmd .= I2C_Read
                  else if writeCmd then do
                      cState  .= Write
                      coreCmd .= I2C_Write
                  else do
                      cState  .= Stop
                      coreCmd .= I2C_Stop
                  ld .= True
    Start -> when coreAck $ do
                   if readCmd then do
                      cState  .= Read
                      coreCmd .= I2C_Read
                   else do
                      cState  .= Write
                      coreCmd .= I2C_Write
                   ld .= True
    Write -> when coreAck $ do
                   if cntDone then do
                      cState  .= Ack
                      coreCmd .= I2C_Read
                   else do
                      cState  .= Write
                      coreCmd .= I2C_Write
                      shift   .= True
    Read  -> when coreAck $ do
                   if cntDone then do
                      cState  .= Ack
                      coreCmd .= I2C_Write
                   else do
                      cState  .= Read
                      coreCmd .= I2C_Read

                   shift   .= True
                   coreTxd .= pack ackIn
    Ack -> if coreAck then do
                  -- check for stop; Should a STOP command be generated?
                  if stopCmd then do
                     cState  .= Stop
                     coreCmd .= I2C_Stop
                  else do
                     cState  .= Idle
                     coreCmd .= I2C_NOP

                     -- generate command acknowledge signal
                     hostAck .= True

                  -- assign ackOut output to coreRxd (contains last received bit)
                  ackOut  .= unpack coreRxd
                  coreTxd .= 1
               else
                  coreTxd .= pack ackIn
    Stop -> when coreAck $ do
                  cState  .= Idle
                  coreCmd .= I2C_NOP

                  -- generate command acknowledge signal
                  hostAck .= True

  return (_coreCmd,_coreTxd,cmdAck,_ackOut,dOut)
