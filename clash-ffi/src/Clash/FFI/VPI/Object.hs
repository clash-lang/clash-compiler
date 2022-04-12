{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Clash.FFI.VPI.Object
  ( Object(..)
  , IsObject(..)
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

class IsObject a where
  nullObject :: a
  isNullObject :: a -> Bool

  freeObject :: a -> SimCont o ()
  compareObjects :: a -> a -> SimCont o Bool

instance IsObject Object where
  nullObject = Object FFI.nullPtr

  isNullObject obj =
    objectPtr obj == FFI.nullPtr

  freeObject obj =
    Monad.unless (isNullObject obj)
      . IO.liftIO
      . Monad.void
#if defined(VERILOG)
      $ c_vpi_free_object obj
#elif defined(SYSTEMVERILOG)
      $ c_vpi_release_handle obj
#else
#error "Neither VERILOG or SYSTEMVERILOG is defined in VPI implementation"
#endif

  compareObjects x y =
    IO.liftIO (c_vpi_compare_objects x y)

{-
NOTE [use of Coercible in public API]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While VPI has an object model, objects are always represented internally as a
`vpiHandle`, which is a pointer to the object provided by the simulator. This
means in `clash-ffi`, the base object type is a pointer, and any more specific
object types are newtypes around this base object.

Rather than duplicate the API for each object, we take a more lenaint approach.
Any operation can be used on an object, provided it can be coerced into the
base object type, and any object can be returned as a result of an FFI call
provided it can be coerced into from the base object type. The advantage of
this is that any call can be made, provided the input and output object types
are given by the user.
-}

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
      , " for object "
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

    Monad.when (isNullObject child) $
      Sim.throw (UnknownChild objTy parent callStack)

    pure (coerce child)

foreign import ccall "vpi_user.h vpi_handle_by_name"
  c_vpi_handle_by_name :: CString -> Object -> IO Object

instance IsChildRef CString where
  getChild str parent = do
    child <- IO.liftIO (c_vpi_handle_by_name str (coerce parent))

    Monad.when (isNullObject child) $
      Sim.throw (UnknownChild str parent callStack)

    pure (coerce child)

foreign import ccall "vpi_user.h vpi_handle_by_index"
  c_vpi_handle_by_index :: Object -> CInt -> IO Object

instance IsChildRef CInt where
  getChild ix parent = do
    child <- IO.liftIO (c_vpi_handle_by_index (coerce parent) ix)

    Monad.when (isNullObject child) $
      Sim.throw (UnknownChild ix parent callStack)

    pure (coerce child)

foreign import ccall "vpi_user.h vpi_handle_by_multi_index"
  c_vpi_handle_by_multi_index :: Object -> CInt -> Ptr CInt -> IO Object

instance IsChildRef [CInt] where
  getChild ixs parent = do
    let len = List.genericLength ixs
    ptr <- unsafeSend ixs
    child <- IO.liftIO (c_vpi_handle_by_multi_index (coerce parent) len ptr)

    Monad.when (isNullObject child) $
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
unsafeSendChildRef ref object =
  unsafeSend ref >>= (`getChild` object)

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
sendChildRef ref object =
  send ref >>= (`getChild` object)

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
      c_vpi_handle_multi_2 :: Object -> Object -> IO Object

    foreign import ccall ...
      c_vpi_handle_multi_3 :: Object -> Object -> Object -> IO Object

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
  getProperty prop object = do
    cprop <- unsafeSend prop
    value <- IO.liftIO (c_vpi_get cprop (coerce object))

    Monad.when (value == -1) $
      Sim.throw (UndefinedProperty prop object callStack)

    pure value

instance IsProperty Bool where
  getProperty prop object = do
    cprop <- unsafeSend prop
    value <- IO.liftIO (c_vpi_get cprop (coerce object))

    Monad.when (value == -1) $
      Sim.throw (UndefinedProperty prop object callStack)

    pure (FFI.toBool value)

#if defined(SYSTEMVERILOG)
foreign import ccall "vpi_user.h vpi_get64"
  c_vpi_get64 :: CInt -> Object -> IO Int64

instance IsProperty Int64 where
  getProperty prop object = do
    cprop <- unsafeSend prop
    value <- IO.liftIO (c_vpi_get64 cprop (coerce object))

    Monad.when (value == -1) $
      Sim.throw (UndefinedProperty prop object callStack)

    pure value
#endif

foreign import ccall "vpi_user.h vpi_get_str"
  c_vpi_get_str :: CInt -> Object -> IO CString

instance IsProperty CString where
  getProperty prop object = do
    cprop <- unsafeSend prop
    value <- IO.liftIO (c_vpi_get_str cprop (coerce object))

    Monad.when (value == FFI.nullPtr) $
      Sim.throw (UndefinedProperty prop object callStack)

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
  => Typeable o
  => Property (Received p)
  -> a
  -> SimCont o p
unsafeReceiveProperty prop object =
  getProperty prop object >>= unsafeReceive

receiveProperty
  :: forall p a o
   . HasCallStack
  => Receive p
  => IsProperty (Received p)
  => Coercible a Object
  => Show a
  => Typeable a
  => Typeable o
  => Property (Received p)
  -> a
  -> SimCont o p
receiveProperty prop object =
  getProperty prop object >>= receive

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
getTime alloc ty mObject = do
  Monad.when (ty == Suppress) $
    Sim.throw (InvalidTimeType ty callStack)

  cty <- unsafeSend ty

  fmap fst . Sim.withNewPtr alloc $ \ptr -> do
    let object = maybe nullObject coerce mObject
    FFI.poke ptr (CTime cty 0 0 0.0)
    c_vpi_get_time object ptr

unsafeReceiveTime
  :: forall a o
   . HasCallStack
  => Coercible a Object
  => Typeable o
  => TimeType
  -> Maybe a
  -> SimCont o Time
unsafeReceiveTime timeTy mObject =
  getTime Sim.stackPtr timeTy mObject >>= unsafePeekReceive

receiveTime
  :: forall a o
   . HasCallStack
  => Coercible a Object
  => Typeable o
  => TimeType
  -> Maybe a
  -> SimCont o Time
receiveTime timeTy mObject =
  getTime Sim.stackPtr timeTy mObject >>= peekReceive

foreign import ccall "vpi_user.h vpi_get_value"
  c_vpi_get_value :: Object -> Ptr CValue -> IO ()

getValue
  :: HasCallStack
  => Coercible a Object
  => SimCont o (Ptr CValue)
  -> ValueFormat
  -> a
  -> SimCont o (Ptr CValue)
getValue alloc fmt object = do
  cfmt <- unsafeSend fmt

  fmap fst . Sim.withNewPtr alloc $ \ptr -> do
    FFI.pokeByteOff ptr 0 cfmt
    c_vpi_get_value (coerce object) ptr

    pure ()

unsafeReceiveValue
  :: forall a o
   . HasCallStack
  => Coercible a Object
  => Show a
  => Typeable a
  => Typeable o
  => ValueFormat
  -> a
  -> SimCont o Value
unsafeReceiveValue fmt object = do
  ptr <- getValue Sim.stackPtr fmt (coerce @a @Object object)
  cvalue <- IO.liftIO (FFI.peek ptr)
  size <- getProperty Size object

  unsafeReceive (cvalue, size)

receiveValue
  :: forall a o
   . HasCallStack
  => Coercible a Object
  => Show a
  => Typeable a
  => Typeable o
  => ValueFormat
  -> a
  -> SimCont o Value
receiveValue fmt object = do
  ptr <- getValue Sim.heapPtr fmt (coerce @a @Object object)
  cvalue <- IO.liftIO (FFI.peek ptr)
  size <- getProperty Size object

  receive (cvalue, size)

foreign import ccall "vpi_user.h vpi_put_value"
  c_vpi_put_value :: Object -> Ptr CValue -> Ptr CTime -> CInt -> IO Object

{-
NOTE [vpi_put_value and events]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In IEEE 1364, it mentions that the return value from vpi_put_value is the event
scheduled by the FFI call (i.e. an event to perform the value change). This is
returned when the vpiReturnEvent flag is set in the call, otherwise it always
returns NULL.

Currently, clash-ffi has no need to be able to cancel events before they are
executed. Instead of returning an object from putValue, we silently discard the
result, and do not allow the high level API to set the vpiReturnEvent flag, so
a valid object would never be returned anyway.
-}

unsafeSendValue
  :: HasCallStack
  => Coercible a Object
  => a
  -> Value
  -> DelayMode
  -> SimCont o ()
  -- No return object, see NOTE [vpi_put_value and events]
unsafeSendValue object value delay = do
  valuePtr <- unsafePokeSend value
  (timePtr, flags) <- unsafeSend delay

  Monad.void . IO.liftIO $
    c_vpi_put_value (coerce object) valuePtr timePtr flags

sendValue
  :: HasCallStack
  => Coercible a Object
  => a
  -> Value
  -> DelayMode
  -> SimCont o ()
  -- No return object, see NOTE [vpi_put_value and events]
sendValue object value delay = do
  valuePtr <- pokeSend value
  (timePtr, flags) <- send delay

  Monad.void . IO.liftIO $
    c_vpi_put_value (coerce object) valuePtr timePtr flags

