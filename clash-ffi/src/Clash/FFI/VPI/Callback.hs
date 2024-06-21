{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- Used to improve the performance of derived instances.
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v0 #-}

#ifndef IVERILOG
module Clash.FFI.VPI.Callback
  ( CCallbackInfo(..)
  , CallbackInfo(..)
  , Callback(..)
  , registerCallback
  , removeCallback
  , getCallbackInfo
  , withCallbackInfo
  , unsafeReceiveCallbackInfo
  , receiveCallbackInfo
  , module Clash.FFI.VPI.Callback.Reason
  ) where
#endif

module Clash.FFI.VPI.Callback
  ( CCallbackInfo(..)
  , CallbackInfo(..)
  , Callback(..)
  , registerCallback
  , removeCallback
  , module Clash.FFI.VPI.Callback.Reason
  ) where

import           Control.Exception (Exception, throwIO)
import qualified Control.Monad as Monad (unless)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
#ifndef IVERILOG
import qualified Foreign.Marshal.Alloc as FFI (alloca, malloc)
#endif
import           Foreign.Ptr (FunPtr, Ptr)
import qualified Foreign.Ptr as FFI (castPtr)
import           Foreign.Storable (Storable)
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

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
  unsafeSend CallbackInfo{..} f =
    unsafeSend cbReason $ \(ccbReason, ccbObject, ccbTime, ccbValue) -> do
      ccbRoutine <- sendRoutine cbRoutine
      let ccbIndex = fromIntegral cbIndex
      unsafeSend cbData $ \ptr -> do
        let ccbData = FFI.castPtr ptr
        f CCallbackInfo{..}

instance (Send extra, CRepr extra ~ Ptr a) => Send (CallbackInfo extra) where
  send CallbackInfo{..} = do
    (ccbReason, ccbObject, ccbTime, ccbValue) <- send cbReason
    ccbRoutine <- sendRoutine cbRoutine
    let ccbIndex = fromIntegral cbIndex
    ccbData <- FFI.castPtr <$> send cbData
    pure CCallbackInfo{..}

foreign import ccall "dynamic"
  receiveRoutine
    :: FunPtr (Ptr CCallbackInfo -> IO CInt)
    -> (Ptr CCallbackInfo -> IO CInt)

instance (UnsafeReceive extra, CRepr extra ~ Ptr a) => UnsafeReceive (CallbackInfo extra) where
  unsafeReceive CCallbackInfo{..} = do
    cbReason <- unsafeReceive (ccbReason, ccbObject, ccbTime, ccbValue)
    let cbRoutine = receiveRoutine ccbRoutine
    let cbIndex = fromIntegral ccbIndex
    cbData <- unsafeReceive $ FFI.castPtr ccbData
    pure CallbackInfo{..}

instance (Receive extra, CRepr extra ~ Ptr a) => Receive (CallbackInfo extra) where
  receive CCallbackInfo{..} = do
    cbReason <- receive (ccbReason, ccbObject, ccbTime, ccbValue)
    let cbRoutine = receiveRoutine ccbRoutine
    let cbIndex = fromIntegral ccbIndex
    cbData <- receive $ FFI.castPtr ccbData
    pure CallbackInfo{..}

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
  :: forall extra
   . UnsafeSend extra
  => CRepr extra ~ CString
  => CallbackInfo extra
  -> IO Callback
registerCallback =
  (`unsafePokeSend` c_vpi_register_cb)

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
removeCallback :: HasCallStack => Callback -> IO ()
removeCallback cb = do
  status <- c_vpi_remove_cb cb
  Monad.unless status $
    throwIO $ CouldNotUnregisterCallback cb callStack

-- iverilog just decided not to implement this VPI call...
#ifndef IVERILOG
foreign import ccall "vpi_user.h vpi_get_cb_info"
  c_vpi_get_cb_info :: Callback -> Ptr CCallbackInfo -> IO ()

-- | Get the low-level representation of the information for the given callback
-- object, which is allocated on the heap. This can be converted to the high-level
-- representation using 'Receive'. If only the high-level representation is needed
-- then consider using 'receiveCallbackInfo' or 'unsafeReceiveCallbackInfo' instead.
--
getCallbackInfo :: Callback -> IO (Ptr CCallbackInfo)
getCallbackInfo callback =
  FFI.malloc >>= \ptr -> c_vpi_get_cb_info callback ptr >> return ptr

-- | Get the low-level representation of the information for the given callback
-- object, which is allocated on the stack. This can be converted to the high-level
-- representation using 'Receive'. If only the high-level representation is needed
-- then consider using 'receiveCallbackInfo' or 'unsafeReceiveCallbackInfo' instead.
--
withCallbackInfo :: Callback -> (Ptr CCallbackInfo -> IO a) -> IO a
withCallbackInfo callback f =
  FFI.alloca $ \ptr -> c_vpi_get_cb_info callback ptr >> f ptr

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
  :: forall extra a
   . UnsafeReceive extra
  => CRepr extra ~ Ptr a
  => Callback
  -> IO (CallbackInfo extra)
unsafeReceiveCallbackInfo callback =
  withCallbackInfo callback unsafePeekReceive

-- | Get the high-level representation of the information for the given
-- callback object. The value is safely read meaning it will not become
-- corrupted if the low-level representation is deallocated.
--
-- For more information about safety, see 'Receive' and 'UnsafeReceive'.
--
receiveCallbackInfo
  :: forall extra a
   . Receive extra
  => CRepr extra ~ Ptr a
  => Callback
  -> IO (CallbackInfo extra)
receiveCallbackInfo callback =
  getCallbackInfo callback >>= peekReceive
#endif
