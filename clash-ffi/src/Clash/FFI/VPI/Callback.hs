{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Clash.FFI.VPI.Callback
  ( module Clash.FFI.VPI.Callback.Reason
  ) where

import qualified Control.Monad.IO.Class as IO (liftIO)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import           Foreign.Ptr (FunPtr, Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import           GHC.TypeNats (KnownNat, type (<=))

import qualified Clash.FFI.Monad as Sim (heapPtr, stackPtr)
import           Clash.FFI.View
import           Clash.FFI.VPI.Callback.Reason
import           Clash.FFI.VPI.Object
import           Clash.FFI.VPI.Time
import           Clash.FFI.VPI.Value

data CCallback = forall n. (KnownNat n, 1 <= n) => CCallback
  { ccbReason   :: CInt
  , ccbRoutine  :: FunPtr (Ptr CCallback -> IO CInt)
  , ccbObject   :: Handle
  , ccbTime     :: Ptr CTime
  , ccbValue    :: Ptr (CValue n)
  , ccbIndex    :: CInt
  , ccbData     :: CString
  }

data Callback extra = forall n. (KnownNat n, 1 <= n) => Callback
  { cbReason  :: CallbackReason
  , cbRoutine :: Ptr CCallback -> IO CInt
  , cbObject  :: Handle
  , cbTime    :: Maybe Time
  , cbValue   :: Maybe (Value n)
  , cbIndex   :: Int
  , cbData    :: extra
  }

-- TODO CallbackReason should contain the handle, the time format and the
-- value format when needed for that particular type of reason. Based on the
-- reason we can say whether these are needed or just set to NULL.

foreign import ccall "wrapper"
  sendRoutine :: (Ptr CCallback -> IO CInt) -> IO (FunPtr (Ptr CCallback -> IO CInt))

instance (UnsafeSend extra, Sent extra ~ CString) => UnsafeSend (Callback extra) where
  type Sent (Callback extra) = CCallback

  unsafeSend Callback{..} = do
    (creason, chandle, ctime, cfmt) <- unsafeSend cbReason
    croutine <- IO.liftIO (sendRoutine cbRoutine)
    cvalue <- (\v -> v) <$> Sim.stackPtr
    let cindex = fromIntegral cbIndex
    bytes <- unsafeSend cbData

    pure (CCallback creason croutine chandle ctime cvalue cindex bytes)

instance (Send extra, Sent extra ~ CString) => Send (Callback extra) where
  send Callback{..} = do
    (creason, chandle, ctime, cfmt) <- send cbReason
    croutine <- IO.liftIO (sendRoutine cbRoutine)
    value <- (\v -> v) <$> Sim.heapPtr
    let cindex = fromIntegral cbIndex
    bytes <- send cbData

    pure (CCallback creason croutine chandle ctime cvalue cindex bytes)

foreign import ccall "dynamic"
  receiveRoutine :: FunPtr (Ptr CCallback -> IO CInt) -> Ptr CCallback -> IO CInt

instance (UnsafeReceive extra, Received extra ~ CString) => UnsafeReceive (Callback extra) where
  type Received (Callback extra) = CCallback

  unsafeReceive CCallback{..} = do
    reason <- unsafeReceive ccbReason
    let routine = receiveRoutine ccbRoutine
    time <- unsafeReceive ccbTime
    value <- unsafeReceive ccbValue
    let index = fromIntegral ccbIndex
    extra <- unsafeReceive ccbData

    pure (Callback reason routine ccbObject time value index extra)

instance (Receive extra, Received extra ~ CString) => Receive (Callback extra) where
  receive CCallback{..} = do
    reason <- receive ccbReason
    let routine = receiveRoutine ccbRoutine
    time <- receive ccbTime
    value <- receive ccbValue
    let index = fromIntegral ccbIndex
    extra <- receive ccbData

    pure (Callback reason routine ccbObject time value index extra)

foreign import ccall "vpi_user.h vpi_register_cb"
  c_vpi_register_cb :: Ptr CCallback -> IO Handle

foreign import ccall "vpi_user.h vpi_remove_cb"
  c_vpi_remove_cb :: Handle -> IO Bool

#ifndef IVERILOG
foreign import ccall "vpi_user.h vpi_get_cb_info"
  c_vpi_get_cb_info :: Handle -> Ptr CCallback -> IO ()
#endif

