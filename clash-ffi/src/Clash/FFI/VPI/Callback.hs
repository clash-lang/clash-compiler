{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Clash.FFI.VPI.Callback
  ( CCallbackInfo(..)
  , CallbackInfo(..)
  , Callback(..)
  , registerCallback
  , removeCallback
#ifndef IVERILOG
  , callbackInfo
#endif
  , module Clash.FFI.VPI.Callback.Reason
  ) where

import           Control.Exception (Exception)
import qualified Control.Monad as Monad (unless)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import           Foreign.Ptr (FunPtr, Ptr)
import           Foreign.Storable (Storable)
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim
import           Clash.FFI.View
import           Clash.FFI.VPI.Callback.Reason
import           Clash.FFI.VPI.Handle
import           Clash.FFI.VPI.Object
import           Clash.FFI.VPI.Value

data CCallbackInfo = CCallbackInfo
  { ccbReason   :: CInt
  , ccbRoutine  :: FunPtr (Ptr CCallbackInfo -> IO CInt)
  , ccbObject   :: Object
  , ccbTime     :: Ptr CTime
  , ccbValue    :: Ptr CValue
  , ccbIndex    :: CInt
  , ccbData     :: CString
  }
  deriving stock (Generic)
  deriving anyclass (GStorable)

data CallbackInfo extra = CallbackInfo
  { cbReason  :: CallbackReason
  , cbRoutine :: Ptr CCallbackInfo -> IO CInt
  , cbIndex   :: Int
  , cbData    :: extra
  }

foreign import ccall "wrapper"
  sendRoutine
    :: (Ptr CCallbackInfo -> IO CInt)
    -> IO (FunPtr (Ptr CCallbackInfo -> IO CInt))

instance (UnsafeSend extra, Sent extra ~ CString) => UnsafeSend (CallbackInfo extra) where
  type Sent (CallbackInfo extra) = CCallbackInfo

  unsafeSend CallbackInfo{..} = do
    (creason, chandle, ctime, cvalue) <- unsafeSend cbReason
    croutine <- IO.liftIO (sendRoutine cbRoutine)
    let cindex = fromIntegral cbIndex
    bytes <- unsafeSend cbData

    pure (CCallbackInfo creason croutine chandle ctime cvalue cindex bytes)

instance (Send extra, Sent extra ~ CString) => Send (CallbackInfo extra) where
  send CallbackInfo{..} = do
    (creason, chandle, ctime, cvalue) <- send cbReason
    croutine <- IO.liftIO (sendRoutine cbRoutine)
    let cindex = fromIntegral cbIndex
    bytes <- send cbData

    pure (CCallbackInfo creason croutine chandle ctime cvalue cindex bytes)

foreign import ccall "dynamic"
  receiveRoutine
    :: FunPtr (Ptr CCallbackInfo -> IO CInt)
    -> (Ptr CCallbackInfo -> IO CInt)

instance (UnsafeReceive extra, Received extra ~ CString) => UnsafeReceive (CallbackInfo extra) where
  type Received (CallbackInfo extra) = CCallbackInfo

  unsafeReceive CCallbackInfo{..} = do
    reason <- unsafeReceive (ccbReason, ccbObject, ccbTime, ccbValue)
    let routine = receiveRoutine ccbRoutine
    let index = fromIntegral ccbIndex
    extra <- unsafeReceive ccbData

    pure (CallbackInfo reason routine index extra)

instance (Receive extra, Received extra ~ CString) => Receive (CallbackInfo extra) where
  receive CCallbackInfo{..} = do
    reason <- receive (ccbReason, ccbObject, ccbTime, ccbValue)
    let routine = receiveRoutine ccbRoutine
    let index = fromIntegral ccbIndex
    extra <- receive ccbData

    pure (CallbackInfo reason routine index extra)

foreign import ccall "vpi_user.h vpi_register_cb"
  c_vpi_register_cb :: Ptr CCallbackInfo -> IO Callback

newtype Callback
  = Callback { callbackObject :: Object }
  deriving stock (Show)
  deriving newtype (Handle, Storable)

registerCallback
  :: forall extra o
   . UnsafeSend extra
  => Sent extra ~ CString
  => CallbackInfo extra
  -> SimCont o Callback
registerCallback cb = do
  ptr <- unsafePokeSend cb
  IO.liftIO (c_vpi_register_cb ptr)

data CouldNotUnregisterCallback
  = CouldNotUnregisterCallback Callback CallStack
  deriving anyclass (Exception)

instance Show CouldNotUnregisterCallback where
  show (CouldNotUnregisterCallback h c) =
    mconcat
      [ "Could not unregister callback: "
      , show h
      , "\n"
      , prettyCallStack c
      ]

foreign import ccall "vpi_user.h vpi_remove_cb"
  c_vpi_remove_cb :: Callback -> IO Bool

removeCallback :: forall o. HasCallStack => Callback -> SimCont o ()
removeCallback cb = do
  status <- IO.liftIO (c_vpi_remove_cb cb)

  Monad.unless status $
    Sim.throw (CouldNotUnregisterCallback cb callStack)

  pure ()

#ifndef IVERILOG
foreign import ccall "vpi_user.h vpi_get_cb_info"
  c_vpi_get_cb_info :: Callback -> Ptr CCallbackInfo -> IO ()

callbackInfo
  :: forall extra o
   . Receive extra
  => Received extra ~ CString
  => Callback
  -> SimCont o (CallbackInfo extra)
callbackInfo (Callback handle) =
  Sim.withNewPtr Sim.stackPtr $ \ptr -> do
    IO.liftIO (c_vpi_get_cb_info handle ptr)
    peekReceive ptr
#endif

