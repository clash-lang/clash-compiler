{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Clash.FFI.VPI.Object
  ( Object(..)
  , module Clash.FFI.VPI.Object.Type
    -- * Child Objects
  , IsChildRef(..)
  , sendChildRef
  , unsafeSendChildRef
    -- * Properties
  , module Clash.FFI.VPI.Object.Property
  , IsProperty(..)
  , coerceProperty
  , receiveProperty
  , unsafeReceiveProperty
    -- * Time
  , module Clash.FFI.VPI.Object.Time
  , getTime
  , receiveTime
  , unsafeReceiveTime
    -- * Value
  , module Clash.FFI.VPI.Object.Value
  , getValue
  , receiveValue
  , unsafeReceiveValue
  , sendValue
  , unsafeSendValue
  ) where

import           Control.Exception (Exception)
import qualified Control.Monad as Monad (unless, void, when)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.Coerce

#if defined(SYSTEMVERILOG)
import           Data.Int (Int64)
#endif

import qualified Data.List as List (genericLength)
import           Data.Typeable (Typeable)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import qualified Foreign.Marshal.Utils as FFI (toBool)
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import           Foreign.Storable (Storable)
import qualified Foreign.Storable as FFI
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (heapPtr, stackPtr, throw, withNewPtr)
import           Clash.FFI.View
import           Clash.FFI.VPI.Handle
import           Clash.FFI.VPI.Object.Property
import           Clash.FFI.VPI.Object.Time
import           Clash.FFI.VPI.Object.Type
import           Clash.FFI.VPI.Object.Value

newtype Object
  = Object { objectPtr :: Ptr Object }
  deriving stock (Show)
  deriving newtype (Storable)

#if defined(VERILOG)
foreign import ccall "vpi_user.h vpi_free_object"
  c_vpi_free_object :: Object -> IO CInt
#elif defined(SYSTEMVERILOG)
foreign import ccall "vpi_user.h vpi_release_handle"
  c_vpi_release_handle :: Object -> IO CInt
#else
#error "Neither VERILOG or SYSTEMVERILOG is defined in VPI implementation"
#endif

foreign import ccall "vpi_user.h vpi_compare_objects"
  c_vpi_compare_objects :: Object -> Object -> IO Bool

instance Handle Object where
  nullHandle = Object FFI.nullPtr

  isNullHandle obj =
    objectPtr obj == FFI.nullPtr

  freeHandle obj =
    Monad.unless (isNullHandle obj)
      . IO.liftIO
      . Monad.void
#if defined(VERILOG)
      $ c_vpi_free_object obj
#elif defined(SYSTEMVERILOG)
      $ c_vpi_release_handle obj
#else
#error "Neither VERILOG or SYSTEMVERILOG is defined in VPI implementation"
#endif

  compareHandles x y =
    IO.liftIO (c_vpi_compare_objects x y)

class IsChildRef i where
  getChild
    :: forall a b o
     . HasCallStack
    => Coercible a Object
    => Show a
    => Typeable a
    => Coercible Object b
    => i
    -> a
    -> SimCont o b

data UnknownChild i a
  = UnknownChild i a CallStack
  deriving anyclass (Exception)

instance (Show i, Show a) => Show (UnknownChild i a) where
  show (UnknownChild i a c) =
    mconcat
      [ "Unknown child "
      , show i
      , " for handle "
      , show a
      , "\n"
      , prettyCallStack c
      ]

foreign import ccall "vpi_user.h vpi_handle"
  c_vpi_handle :: CInt -> Object -> IO Object

instance IsChildRef ObjectType where
  getChild objTy parent = do
    cobjTy <- unsafeSend objTy
    child <- IO.liftIO (c_vpi_handle cobjTy (coerce parent))

    Monad.when (isNullHandle child) $
      Sim.throw (UnknownChild objTy parent callStack)

    pure (coerce child)

foreign import ccall "vpi_user.h vpi_handle_by_name"
  c_vpi_handle_by_name :: CString -> Object -> IO Object

instance IsChildRef CString where
  getChild str parent = do
    child <- IO.liftIO (c_vpi_handle_by_name str (coerce parent))

    Monad.when (isNullHandle child) $
      Sim.throw (UnknownChild str parent callStack)

    pure (coerce child)

foreign import ccall "vpi_user.h vpi_handle_by_index"
  c_vpi_handle_by_index :: Object -> CInt -> IO Object

instance IsChildRef CInt where
  getChild ix parent = do
    child <- IO.liftIO (c_vpi_handle_by_index (coerce parent) ix)

    Monad.when (isNullHandle child) $
      Sim.throw (UnknownChild ix parent callStack)

    pure (coerce child)

foreign import ccall "vpi_user.h vpi_handle_by_multi_index"
  c_vpi_handle_by_multi_index :: Object -> CInt -> Ptr CInt -> IO Object

instance IsChildRef [CInt] where
  getChild ixs parent = do
    let len = List.genericLength ixs
    ptr <- unsafeSend ixs
    child <- IO.liftIO (c_vpi_handle_by_multi_index (coerce parent) len ptr)

    Monad.when (isNullHandle child) $
      Sim.throw (UnknownChild ixs parent callStack)

    pure (coerce child)

unsafeSendChildRef
  :: forall i a b o
   . HasCallStack
  => UnsafeSend i
  => IsChildRef (Sent i)
  => Coercible a Object
  => Show a
  => Typeable a
  => Coercible Object b
  => i
  -> a
  -> SimCont o b
unsafeSendChildRef ref handle =
  unsafeSend ref >>= (`getChild` handle)

sendChildRef
  :: forall i a b o
   . HasCallStack
  => Send i
  => IsChildRef (Sent i)
  => Coercible a Object
  => Show a
  => Typeable a
  => Coercible Object b
  => i
  -> a
  -> SimCont o b
sendChildRef ref handle =
  send ref >>= (`getChild` handle)

{-
NOTE [vpi_handle_multi]
~~~~~~~~~~~~~~~~~~~~~~~
When traversing VPI relationships, there are different functions to use
depending on how many objects are on the LHS or RHS:

  * 1-N: vpi_iterate is used to create an iterator over the N objects, and
         vpi_scan is used to advance the iterator

  * 1-1: vpi_handle or vpi_handle_by_* are used to access the related handle
         by something which uniquely determines it (type, name, index)

  * N-1: vpi_handle_multi is used, taking all LHS handles as varargs, and
         returning the single RHS handle

Since vpi_handle_multi uses varargs instead of an array for input, it cannot
be conveniently imported. We would need to add imports such as

    foreign import ccall ...
      c_vpi_handle_multi_2 :: VpiHandle -> VpiHandle -> IO VpiHandle

    foreign import ccall ...
      c_vpi_handle_multi_3 :: VpiHandle -> VpiHandle -> VpiHandle -> IO VpiHandle

This is somewhat of a burden, so instead for the time being we leave these
relationships inaccessible. Most relationships are 1-N or 1-1 in practice, so
this is not a particularly large limitation.
-}

class IsProperty p where
  getProperty
    :: HasCallStack
    => Coercible a Object
    => Show a
    => Typeable a
    => Property p
    -> a
    -> SimCont o p

foreign import ccall "vpi_user.h vpi_get"
  c_vpi_get :: CInt -> Object -> IO CInt

instance IsProperty CInt where
  getProperty prop handle = do
    cprop <- unsafeSend prop
    value <- IO.liftIO (c_vpi_get cprop (coerce handle))

    Monad.when (value == -1) $
      Sim.throw (UndefinedProperty prop handle callStack)

    pure value

instance IsProperty Bool where
  getProperty prop handle = do
    cprop <- unsafeSend prop
    value <- IO.liftIO (c_vpi_get cprop (coerce handle))

    Monad.when (value == -1) $
      Sim.throw (UndefinedProperty prop handle callStack)

    pure (FFI.toBool value)

#if defined(SYSTEMVERILOG)
foreign import ccall "vpi_user.h vpi_get64"
  c_vpi_get64 :: CInt -> Object -> IO Int64

instance IsProperty Int64 where
  getProperty prop handle = do
    cprop <- unsafeSend prop
    value <- IO.liftIO (c_vpi_get64 cprop (coerce handle))

    Monad.when (value == -1) $
      Sim.throw (UndefinedProperty prop handle callStack)

    pure value
#endif

foreign import ccall "vpi_user.h vpi_get_str"
  c_vpi_get_str :: CInt -> Object -> IO CString

instance IsProperty CString where
  getProperty prop handle = do
    cprop <- unsafeSend prop
    value <- IO.liftIO (c_vpi_get_str cprop (coerce handle))

    Monad.when (value == FFI.nullPtr) $
      Sim.throw (UndefinedProperty prop handle callStack)

    pure value

coerceProperty
  :: forall p q a o
   . HasCallStack
  => IsProperty p
  => Coercible p q
  => Coercible a Object
  => Show a
  => Typeable a
  => Property p
  -> a
  -> SimCont o q
coerceProperty prop =
  fmap coerce . getProperty prop

unsafeReceiveProperty
  :: forall p a o
   . HasCallStack
  => UnsafeReceive p
  => IsProperty (Received p)
  => Coercible a Object
  => Show a
  => Typeable a
  => Property (Received p)
  -> a
  -> SimCont o p
unsafeReceiveProperty prop handle =
  getProperty prop handle >>= unsafeReceive

receiveProperty
  :: forall p a o
   . HasCallStack
  => Receive p
  => IsProperty (Received p)
  => Coercible a Object
  => Show a
  => Typeable a
  => Property (Received p)
  -> a
  -> SimCont o p
receiveProperty prop handle =
  getProperty prop handle >>= receive

foreign import ccall "vpi_user.h vpi_get_time"
  c_vpi_get_time :: Object -> Ptr CTime -> IO ()

getTime
  :: forall a o
   . HasCallStack
  => Coercible a Object
  => SimCont o (Ptr CTime)
  -> TimeType
  -> Maybe a
  -> SimCont o (Ptr CTime)
getTime alloc ty mHandle = do
  Monad.when (ty == Suppress) $
    Sim.throw (InvalidTimeType ty callStack)

  cty <- unsafeSend ty

  fmap fst . Sim.withNewPtr alloc $ \ptr -> do
    let object = maybe nullHandle coerce mHandle
    FFI.poke ptr (CTime cty 0 0 0.0)
    c_vpi_get_time object ptr

unsafeReceiveTime
  :: forall a o
   . HasCallStack
  => Coercible a Object
  => TimeType
  -> Maybe a
  -> SimCont o Time
unsafeReceiveTime timeTy mHandle =
  getTime Sim.stackPtr timeTy mHandle >>= unsafePeekReceive

receiveTime
  :: forall a o
   . HasCallStack
  => Coercible a Object
  => TimeType
  -> Maybe a
  -> SimCont o Time
receiveTime timeTy mHandle =
  getTime Sim.stackPtr timeTy mHandle >>= peekReceive

foreign import ccall "vpi_user.h vpi_get_value"
  c_vpi_get_value :: Object -> Ptr CValue -> IO ()

getValue
  :: HasCallStack
  => Coercible handle Object
  => SimCont o (Ptr CValue)
  -> ValueFormat
  -> handle
  -> SimCont o (Ptr CValue)
getValue alloc fmt handle = do
  cfmt <- unsafeSend fmt

  fmap fst . Sim.withNewPtr alloc $ \ptr -> do
    FFI.pokeByteOff ptr 0 cfmt
    c_vpi_get_value (coerce handle) ptr

    pure ()

unsafeReceiveValue
  :: forall handle o
   . HasCallStack
  => Coercible handle Object
  => Show handle
  => Typeable handle
  => ValueFormat
  -> handle
  -> SimCont o Value
unsafeReceiveValue fmt handle = do
  ptr <- getValue Sim.stackPtr fmt (coerce @handle @Object handle)
  cvalue <- IO.liftIO (FFI.peek ptr)
  size <- getProperty Size handle

  unsafeReceive (cvalue, size)

receiveValue
  :: forall handle o
   . HasCallStack
  => Coercible handle Object
  => Show handle
  => Typeable handle
  => ValueFormat
  -> handle
  -> SimCont o Value
receiveValue fmt handle = do
  ptr <- getValue Sim.heapPtr fmt (coerce @handle @Object handle)
  cvalue <- IO.liftIO (FFI.peek ptr)
  size <- getProperty Size handle

  receive (cvalue, size)

foreign import ccall "vpi_user.h vpi_put_value"
  c_vpi_put_value :: Object -> Ptr CValue -> Ptr CTime -> CInt -> IO Object

{-
NOTE [vpi_put_value and events]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In IEEE 1364, it mentions that the return value from vpi_put_value is a handle
to the event scheduled by the FFI call (i.e. an event to perform the value
change). This is returned when the vpiReturnEvent flag is set in the call,
otherwise it always returns NULL.

Currently, clash-ffi has no need to be able to cancel events before they are
executed. Instead of returning a handle from putValue, we silently discard the
result, and do not allow the high level API to set the vpiReturnEvent flag, so
a valid handle would never be returned anyway.
-}

unsafeSendValue
  :: HasCallStack
  => Coercible handle Object
  => handle
  -> Value
  -> DelayMode
  -> SimCont o ()
unsafeSendValue handle value delay = do
  valuePtr <- unsafePokeSend value
  (timePtr, flags) <- unsafeSend delay

  Monad.void . IO.liftIO $
    c_vpi_put_value (coerce handle) valuePtr timePtr flags

sendValue
  :: HasCallStack
  => Coercible handle Object
  => handle
  -> Value
  -> DelayMode
  -> SimCont o ()
sendValue handle value delay = do
  valuePtr <- pokeSend value
  (timePtr, flags) <- send delay

  Monad.void . IO.liftIO $
    c_vpi_put_value (coerce handle) valuePtr timePtr flags

