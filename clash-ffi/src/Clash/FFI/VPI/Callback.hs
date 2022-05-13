{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- Used to improve the performance of derived instances.
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v0 #-}

module Clash.FFI.VPI.Callback
  ( CCallbackInfo(..)
  , CallbackInfo(..)
  , Callback(..)
  , registerCallback
  , removeCallback
#ifndef IVERILOG
  , getCallbackInfo
#endif
  , module Clash.FFI.VPI.Callback.Reason
  ) where

import           Control.Exception (Exception)
import qualified Control.Monad as Monad (unless)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import           Foreign.Ptr (FunPtr, Ptr)
import qualified Foreign.Ptr as FFI (castPtr)
import           Foreign.Storable (Storable)
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim
import           Clash.FFI.View
import           Clash.FFI.VPI.Callback.Reason
import           Clash.FFI.VPI.Object

-- | The low-level representation of a VPI callback, as sent to the
-- @vpi_register_cb@ function. This can be converted to and from 'CallbackInfo'
-- using 'Send' and 'Receive'.
--
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

-- | Information about a callback which is used to register new callbacks, or
-- returned when requesting information about a callack (in compatible
-- simulators).
--
-- For the low-level representation of callbacks that are sent / received by
-- VPI calls, see 'CCallbackInfo'.
--
data CallbackInfo extra = CallbackInfo
  { cbReason  :: CallbackReason
  , cbRoutine :: Ptr CCallbackInfo -> IO CInt -- TODO CallbackInfo -> IO CInt
  , cbIndex   :: Int
  , cbData    :: extra
  }

foreign import ccall "wrapper"
  sendRoutine
    :: (Ptr CCallbackInfo -> IO CInt)
    -> IO (FunPtr (Ptr CCallbackInfo -> IO CInt))

type instance CRepr (CallbackInfo _) = CCallbackInfo

instance (UnsafeSend extra, CRepr extra ~ Ptr a) => UnsafeSend (CallbackInfo extra) where
  unsafeSend CallbackInfo{..} = do
    (creason, cobject, ctime, cvalue) <- unsafeSend cbReason
    croutine <- IO.liftIO (sendRoutine cbRoutine)
    let cindex = fromIntegral cbIndex
    bytes <- FFI.castPtr <$> unsafeSend cbData

    pure (CCallbackInfo creason croutine cobject ctime cvalue cindex bytes)

instance (Send extra, CRepr extra ~ Ptr a) => Send (CallbackInfo extra) where
  send CallbackInfo{..} = do
    (creason, cobject, ctime, cvalue) <- send cbReason
    croutine <- IO.liftIO (sendRoutine cbRoutine)
    let cindex = fromIntegral cbIndex
    bytes <- FFI.castPtr <$> send cbData

    pure (CCallbackInfo creason croutine cobject ctime cvalue cindex bytes)

foreign import ccall "dynamic"
  receiveRoutine
    :: FunPtr (Ptr CCallbackInfo -> IO CInt)
    -> (Ptr CCallbackInfo -> IO CInt)

instance (UnsafeReceive extra, CRepr extra ~ Ptr a) => UnsafeReceive (CallbackInfo extra) where
  unsafeReceive CCallbackInfo{..} = do
    reason <- unsafeReceive (ccbReason, ccbObject, ccbTime, ccbValue)
    let routine = receiveRoutine ccbRoutine
    let index = fromIntegral ccbIndex
    extra <- unsafeReceive (FFI.castPtr ccbData)

    pure (CallbackInfo reason routine index extra)

instance (Receive extra, CRepr extra ~ Ptr a) => Receive (CallbackInfo extra) where
  receive CCallbackInfo{..} = do
    reason <- receive (ccbReason, ccbObject, ccbTime, ccbValue)
    let routine = receiveRoutine ccbRoutine
    let index = fromIntegral ccbIndex
    extra <- receive (FFI.castPtr ccbData)

    pure (CallbackInfo reason routine index extra)

foreign import ccall "vpi_user.h vpi_register_cb"
  c_vpi_register_cb :: Ptr CCallbackInfo -> IO Callback

-- | A callback is a VPI object that is known to refer to something with the
-- @vpiCallback@ object type. This wrapper exposes operations which are known
-- to be safe on callbacks specifically.
--
-- Callbacks can be unsafely created from 'Object', although this is only safe
-- if you first check the @vpiType@ property to confirm it is a callback.
-- Callbacks can be safely converted to the base object type using
-- 'callbackObject'.
--
newtype Callback
  = Callback { callbackObject :: Object }
  deriving stock (Show)
  deriving newtype (IsObject, Storable)

-- | Register a new callback with the simulator from info which specifies when
-- the callback is triggered, and what data it has available.
--
registerCallback
  :: forall extra o
   . UnsafeSend extra
  => CRepr extra ~ CString
  => CallbackInfo extra
  -> SimCont o Callback
registerCallback cb = do
  ptr <- unsafePokeSend cb
  IO.liftIO (c_vpi_register_cb ptr)

-- | An exception thrown when VPI could not remove a callback from the running
-- simulator. If this is thrown the callback may still be active in simulation.
--
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

-- | Remove a callback from the simulator. Removing a callback also frees the
-- callback object, so 'freeObject' does not need to be called.
--
removeCallback :: forall o. HasCallStack => Callback -> SimCont o ()
removeCallback cb = do
  status <- IO.liftIO (c_vpi_remove_cb cb)

  Monad.unless status $
    Sim.throw (CouldNotUnregisterCallback cb callStack)

  pure ()

-- iverilog just decided not to implement this VPI call...
#ifndef IVERILOG
foreign import ccall "vpi_user.h vpi_get_cb_info"
  c_vpi_get_cb_info :: Callback -> Ptr CCallbackInfo -> IO ()

-- | Get the low-level representation of the information for the given callback
-- object. This can be converted to the high-level representation using
-- 'Receive'. If only the high-level representation is needed then consider
-- using 'receiveCallbackInfo' or 'unsafeReceiveCallbackInfo' instead.
--
getCallbackInfo
  :: forall o
   . SimCont o (Ptr CCallbackInfo)
  -> Callback
  -> SimCont o (Ptr CCallbackInfo)
getCallbackInfo alloc callback =
  Sim.withNewPtr alloc (c_vpi_get_cb_info callback)

-- | Get the high-level representation of the information for the given
-- callback object. The value is unsafely read, meaning it may be corrupted if
-- the low-level representation is deallocated.
--
-- The low-level representation is allocated on the stack, meaning it will not
-- survive past the end of the current callback.
--
-- For more information about safety, see 'Receive' and 'UnsafeReceive'.
--
unsafeReceiveCallbackInfo
  :: forall extra a o
   . UnsafeReceive extra
  => CRepr extra ~ Ptr a
  => Callback
  -> SimCont o (CallbackInfo extra)
unsafeReceiveCallbackInfo callback =
  getCallbackInfo Sim.stackPtr callback >>= unsafeReceive

-- | Get the high-level representation of the information for the given
-- callback object. The value is safely read meaning it will not become
-- corrupted if the low-level representation is deallocated.
--
-- For more information about safety, see 'Receive' and 'UnsafeReceive'.
--
receiveCallbackInfo
  :: forall extra a o
   . Receive extra
  => CRepr extra ~ Ptr a
  => Callback
  -> SimCont o (CallbackInfo extra)
receiveCallbackInfo callback =
  getCallbackInfo Sim.stackPtr callback >>= receive
#endif
