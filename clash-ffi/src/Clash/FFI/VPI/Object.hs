{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Object
  ( Object(..)
  , IsObject(..)
  , module Clash.FFI.VPI.Object.Type
    -- * Child Objects
  , IsChildRef(..)
  , UnknownChild(..)
  , sendChildRef
  , unsafeSendChildRef
    -- * Properties
  , module Clash.FFI.VPI.Object.Property
  , IsProperty(..)
  , receiveProperty
    -- * Time
  , module Clash.FFI.VPI.Object.Time
  , getTime
  , withTime
  , receiveTime
    -- * Value
  , module Clash.FFI.VPI.Object.Value
  , getValue
  , withValue
  , receiveValue
  , unsafeReceiveValue
  , sendValue
  , unsafeSendValue
  ) where

import           Control.DeepSeq (NFData)
import           Control.Exception (Exception, throwIO)
import qualified Control.Monad as Monad (unless, void, when)
import           Data.Coerce

#if defined(SYSTEMVERILOG)
import           Data.Int (Int64)
#endif

import qualified Data.List as List (genericLength)
import           Data.Typeable (Typeable)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import qualified Foreign.Marshal.Alloc as FFI (alloca, malloc)
import qualified Foreign.Marshal.Utils as FFI (toBool)
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (castPtr, nullPtr)
import           Foreign.Storable (Storable)
import qualified Foreign.Storable as FFI
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import           Clash.FFI.View
import           Clash.FFI.VPI.Object.Property
import           Clash.FFI.VPI.Object.Time
import           Clash.FFI.VPI.Object.Type
import           Clash.FFI.VPI.Object.Value

-- | The base object type. Every VPI function is defined on this type, although
-- the supported functions are determined by the @vpiType@ property of an
-- object.
--
-- For some object types this library provides more meaningful newtypes which
-- may have more specific operations implemented for them. See
-- 'Clash.FFI.VPI.Module.Module', 'Clash.FFI.VPI.Iterator.Iterator', and
-- 'Clash.FFI.VPI.Port.Port' for examples of this.
--
-- Operations on objects are implemented by accepting any type which _coerces_
-- to @Object@. This means that the full VPI API can always be used, but users
-- are responsible for making sure calls are safe.
--
newtype Object
  = Object { objectPtr :: Ptr Object }
  deriving stock (Show)
  deriving newtype (NFData, Storable)

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

-- | All VPI objects support a common set of operations, which are used when
-- implementing other VPI functions. Any new VPI object type should derive
-- this class using @-XGeneralizedNewtypeDeriving@, as there is only one
-- implementation which is guaranteed to be consistent with the simulator.
--
class IsObject a where
  -- | The null object of a given object type. All object types have the same
  -- null object.
  --
  nullObject :: a

  -- | Check if the object is the null object. This does not need to be used
  -- directly for most applications, as the VPI API functions in this library
  -- will use it where appropriate (e.g. to determine when a result is
  -- @Nothing@ or @Just in functions that return @Maybe@, or to identify when
  -- to throw exceptions).
  --
  isNullObject :: a -> Bool

  -- | Deallocate the object. The object should not be used for any calls after
  -- this is called, as the object is no longer valid.
  --
  freeObject :: a -> IO ()

  -- | Equality on VPI objects. This function is not pure, as the current
  -- assignment of identifiers to objects in the simulator may change over time
  -- (so a deallocated object's identifier may be used for a new object).
  --
  compareObjects :: a -> a -> IO Bool

instance IsObject Object where
  nullObject = Object FFI.nullPtr

  isNullObject obj =
    objectPtr obj == FFI.nullPtr

  freeObject obj =
    Monad.unless (isNullObject obj)
      . Monad.void
#if defined(VERILOG)
      $ c_vpi_free_object obj
#elif defined(SYSTEMVERILOG)
      $ c_vpi_release_handle obj
#else
#error "Neither VERILOG or SYSTEMVERILOG is defined in VPI implementation"
#endif

  compareObjects =
    c_vpi_compare_objects

{-
NOTE [use of Coercible in public API]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
While VPI has an object model, objects are always represented internally as a
`vpiHandle`, which is a pointer to the object provided by the simulator. This
means in `clash-ffi`, the base object type is a pointer, and any more specific
object types are newtypes around this base object.

Rather than duplicate the API for each object, we take a more lenient approach.
Any operation can be used on an object, provided it can be coerced into the
base object type, and any object can be returned as a result of an FFI call
provided it can be coerced into from the base object type. The advantage of
this is that any call can be made, provided the input and output object types
are given by the user.
-}

-- | In VPI, there are different ways to access child objects of some parent
-- object:
--
--   * @vpi_handle@, which takes the object type of the child
--   * @vpi_handle_by_name@, which takes the name (or full name) of the child
--   * @vpi_handle_by_index@, which takes the integer index of the child
--   * @vpi_handle_by_multi_index@, which takes a list of indices to the child
--
-- While the C API is forced to provide multiple functions, @clash-ffi@
-- abstracts this to a class of child reference types, which allows a uniform
-- API for accessing child objects.
--
class IsChildRef i where
  -- | Get the child object for the given parent using some reference type.
  -- If no parent object is given, the child is a child of the top-level of
  -- the design. This allows specific top-level objects to be accessed by
  -- reference, i.e. accessing a module by its name.
  --
  -- If a high-level representation of the reference can be used then consider
  -- using 'sendChildRef' or 'unsafeSendChildRef' instead.
  --
  -- If no child with the given reference exists under the parent object, then
  -- an 'UnknownChild' exception is thrown.
  --
  getChild
    :: forall a b
     . HasCallStack
    => Coercible a Object
    => Show a
    => Typeable a
    => Coercible Object b
    => i
    -> Maybe a
    -> IO b

-- | An exception thrown when attempting to access a child object using a
-- reference of type @i@ which does not exist under the parent object @a@.
--
data UnknownChild i a
  = UnknownChild i a CallStack
  deriving anyclass (Exception)

instance (Show i, Show a) => Show (UnknownChild i a) where
  show = \case
    UnknownChild i a c -> mconcat
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
  getChild objTy mParent = do
    cobjTy <- send objTy
    let parent = maybe nullObject coerce mParent
    child <- c_vpi_handle cobjTy parent

    Monad.when (isNullObject child) $
      throwIO $ UnknownChild objTy parent callStack

    pure $ coerce child

foreign import ccall "vpi_user.h vpi_handle_by_name"
  c_vpi_handle_by_name :: CString -> Object -> IO Object

instance IsChildRef CString where
  getChild str mParent = do
    let parent = maybe nullObject coerce mParent
    child <- c_vpi_handle_by_name str parent

    Monad.when (isNullObject child) $
      throwIO $ UnknownChild str parent callStack

    pure $ coerce child

foreign import ccall "vpi_user.h vpi_handle_by_index"
  c_vpi_handle_by_index :: Object -> CInt -> IO Object

instance IsChildRef CInt where
  getChild ix mParent = do
    let parent = maybe nullObject coerce mParent
    child <- c_vpi_handle_by_index parent ix

    Monad.when (isNullObject child) $
      throwIO $ UnknownChild ix parent callStack

    pure $ coerce child

foreign import ccall "vpi_user.h vpi_handle_by_multi_index"
  c_vpi_handle_by_multi_index :: Object -> CInt -> Ptr CInt -> IO Object

instance IsChildRef [CInt] where
  getChild ixs mParent = do
    let len = List.genericLength ixs
    unsafeSend ixs $ \ptr ->  do
      let parent = maybe nullObject coerce mParent
      child <- c_vpi_handle_by_multi_index parent len ptr

      Monad.when (isNullObject child) $
        throwIO $ UnknownChild ixs parent callStack

      pure $ coerce child

-- | Get a child object by a reference of type @i@ from an optional parent
-- object of type @a@. The reference given is a high-level representation which
-- is unsafely converted into a low-level representation suitable for the VPI
-- call used. As the call is unsafe, changes to the high-level representation
-- may lead to an error if they occur before the VPI call is made.
--
-- If no child with the given reference exists under the parent object, then
-- an 'UnknownChild' exception is thrown.
--
-- For more information about safety, see 'Send' and 'UnsafeSend'.
--
unsafeSendChildRef
  :: forall i a b
   . HasCallStack
  => UnsafeSend i
  => IsChildRef (CRepr i)
  => Coercible a Object
  => Show a
  => Typeable a
  => Coercible Object b
  => i
  -> Maybe a
  -> IO b
unsafeSendChildRef ref parent =
  unsafeSend ref (`getChild` parent)

-- | Get a child object by reference of type @i@ from an optional parent object
-- of type @a@. The reference given is a high-level representation which is
-- safely converted into a low-level representation. This means the high-level
-- type can be deallocated before the FFI call is made without causing errors.
--
-- If no child with the given reference exists under the parent object, then
-- an 'UnknownChild' exception is thrown.
--
-- For more information about safety, see 'Send' and 'UnsafeSend'.
--
sendChildRef
  :: forall i a b
   . HasCallStack
  => Send i
  => IsChildRef (CRepr i)
  => Coercible a Object
  => Show a
  => Typeable a
  => Coercible Object b
  => i
  -> Maybe a
  -> IO b
sendChildRef ref parent =
  send ref >>= (`getChild` parent)

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

-- | In VPI, there are different ways to access properties of objects:
--
--   * @vpi_get@, which returns an @int@ (which may be encoding a boolean)
--   * @vpi_get_str@, which returns a string
--   * @vpi_get64@ (SystemVerilog only), which returns a @int64_t@
--
-- While the C API is forced to provide multiple functions, @clash-ffi@
-- abstracts this to a class of property value types, which allows a uniform
-- API for accessing properties of an object.
--
class IsProperty p where
  -- | Get the property associated with the given object. The properties which
  -- exist for an object depend on the type of the object, although the
  -- 'Clash.FFI.VPI.Object.Property.Type' property can be used to get the
  -- 'ObjectType' to ensure that invalid properties are not accessed for an
  -- object.
  --
  -- If a high-level representation of the property value is desired then
  -- consider using 'receiveProperty'.
  --
  -- If the property does not exist for the given object, then an
  -- 'InvalidProperty' exception is thrown.
  --
  getProperty
    :: HasCallStack
    => Coercible a Object
    => Show a
    => Typeable a
    => Property p
    -> a
    -> IO p

foreign import ccall "vpi_user.h vpi_get"
  c_vpi_get :: CInt -> Object -> IO CInt

instance IsProperty CInt where
  getProperty prop object = do
    cprop <- send prop
    value <- c_vpi_get cprop $ coerce object

    Monad.when (value == -1) $
      throwIO $ InvalidProperty prop object callStack

    pure value

instance IsProperty Bool where
  getProperty prop object = do
    cprop <- send prop
    value <- c_vpi_get cprop $ coerce object

    Monad.when (value == -1) $
      throwIO $ InvalidProperty prop object callStack

    pure $ FFI.toBool value

#if defined(SYSTEMVERILOG)
foreign import ccall "vpi_user.h vpi_get64"
  c_vpi_get64 :: CInt -> Object -> IO Int64

instance IsProperty Int64 where
  getProperty prop object = do
    cprop <- send prop
    value <- c_vpi_get64 cprop $ coerce object

    Monad.when (value == -1) $
      throwIO $ InvalidProperty prop object callStack

    pure value
#endif

foreign import ccall "vpi_user.h vpi_get_str"
  c_vpi_get_str :: CInt -> Object -> IO CString

instance IsProperty CString where
  getProperty prop object = do
    cprop <- send prop
    value <- c_vpi_get_str cprop $ coerce object

    Monad.when (value == FFI.nullPtr) $
      throwIO $ InvalidProperty prop object callStack

    pure value

-- | Get the value of a property on the given object. The value is safely read,
-- as the VPI specification says that the result of calls to @vpi_get_str@ are
-- stored in a static buffer which is overwritten on each call. Due to this
-- there is no @unsafeReceiveProperty@.
--
-- For more information about safety, see 'Receive' and 'UnsafeReceive'.
--
receiveProperty
  :: forall p a
   . HasCallStack
  => Receive p
  => IsProperty (CRepr p)
  => Coercible a Object
  => Show a
  => Typeable a
  => Property (CRepr p)
  -> a
  -> IO p
receiveProperty prop object =
  getProperty prop object >>= receive

foreign import ccall "vpi_user.h vpi_get_time"
  c_vpi_get_time :: Object -> Ptr CTime -> IO ()

-- | Get a pointer to the low-level representation of the current simulation
-- time, where the pointer is allocated on the heap. The time returned is
-- given in the specified format, and is either the global simulation time or
-- the time of a particular object in the simulation.
--
-- The 'SuppressTime' format cannot be used for this function. Requesting this
-- as the format will throw an 'InvalidTimeType' exception.
--
-- The retuned value can be converted to the high-level representation using
-- 'Receive'. If only the high-level representation is needed then consider
-- using 'receiveTime' instead.
--
getTime
  :: forall a
   . HasCallStack
  => Coercible a Object
  => TimeType
  -> Maybe a
  -> IO (Ptr CTime)
getTime ty mObject = do
  Monad.when (ty == SuppressTime) $
    throwIO $ InvalidTimeType ty callStack

  cty <- send ty

  FFI.malloc >>= \ptr -> do
    let object = maybe nullObject coerce mObject
    FFI.poke ptr $ CTime cty 0 0 0.0
    c_vpi_get_time object ptr
    return ptr

-- | Get a pointer to the low-level representation of the current simulation
-- time, where the pointer is allocated on the stack. The time returned is
-- given in the specified format, and is either the global simulation time or
-- the time of a particular object in the simulation.
--
-- The 'SuppressTime' format cannot be used for this function. Requesting this
-- as the format will throw an 'InvalidTimeType' exception.
--
-- The retuned value can be converted to the high-level representation using
-- 'Receive'. If only the high-level representation is needed then consider
-- using 'receiveTime' instead.
--
withTime
  :: forall a b
   . HasCallStack
  => Coercible a Object
  => TimeType
  -> Maybe a
  -> (Ptr CTime -> IO b) -> IO b
withTime ty mObject f = do
  Monad.when (ty == SuppressTime) $
    throwIO $ InvalidTimeType ty callStack

  cty <- send ty

  FFI.alloca $ \ptr -> do
    let object = maybe nullObject coerce mObject
    FFI.poke ptr $ CTime cty 0 0 0.0
    c_vpi_get_time object ptr
    f ptr

-- | Get the high-level representation of the current simulation time. The
-- value is safely read, meaning it will not become corrupted if the low-level
-- representation is deallocated.
--
-- The 'SuppressTime' format cannot be used for this function. Requesting this
-- as the format will throw an 'InvalidTimeType' exception.
--
-- For more information about safety, see 'Receive' and 'UnsafeReceive'.
--
receiveTime
  :: forall a
   . HasCallStack
  => Coercible a Object
  => TimeType
  -> Maybe a
  -> IO Time
receiveTime timeTy mObject =
  withTime timeTy mObject peekReceive

foreign import ccall "vpi_user.h vpi_get_value"
  c_vpi_get_value :: Object -> Ptr CValue -> IO ()

-- | Get a pointer to the low-level representation of the current value
-- associated with the given object, where the pointer is allocated on the
-- heap. The value is returned in the requested format.
--
-- The 'SuppressValue' format cannot be used for this function. Requesting this
-- as the format will throw an 'InvalidFormat' exception.
--
-- The returned value can be converted to the high-level representation using
-- 'Receive'. If only the high-level representation is needed then consider
-- using 'receiveValue' or 'unsafeReceiveValue' instead.
--
getValue
  :: HasCallStack
  => Coercible a Object
  => ValueFormat
  -> a
  -> IO (Ptr CValue)
getValue fmt object = do
  cfmt <- send fmt
  FFI.malloc >>= \ptr -> do
    FFI.pokeByteOff ptr 0 cfmt
    c_vpi_get_value (coerce object) ptr
    return ptr

-- | Get a pointer to the low-level representation of the current value
-- associated with the given object, where the pointer is allocated on the
-- stack. The value is returned in the requested format.
--
-- The 'SuppressValue' format cannot be used for this function. Requesting this
-- as the format will throw an 'InvalidFormat' exception.
--
-- The returned value can be converted to the high-level representation using
-- 'Receive'. If only the high-level representation is needed then consider
-- using 'receiveValue' or 'unsafeReceiveValue' instead.
--
withValue
  :: HasCallStack
  => Coercible a Object
  => ValueFormat
  -> a
  -> (Ptr CValue -> IO b) -> IO b
withValue fmt object f = do
  cfmt <- send fmt
  FFI.alloca $ \ptr -> do
    FFI.pokeByteOff ptr 0 cfmt
    c_vpi_get_value (coerce object) ptr
    f ptr

-- | Get the high-level representation of the current value associated with the
-- given object. The value is converted from a low-level representation with
-- the requested format.
--
-- The 'SuppressValue' format cannot be used for this function. Requesting this
-- as the format will throw an 'InvalidFormat' exception.
--
-- The low-level representation is allocated on the stack, meaning it will not
-- survive past the end of the current callback.
--
-- For more information about safety, see 'Receive' and 'UnsafeReceive'.
--
unsafeReceiveValue
  :: forall a
   . HasCallStack
  => Coercible a Object
  => Show a
  => Typeable a
  => ValueFormat
  -> a
  -> IO Value
unsafeReceiveValue fmt object =
  withValue fmt (coerce @a @Object object) $ \ptr -> do
    cvalue <- FFI.peek ptr
    size <- getProperty Size object
    unsafeReceive $ CValueSized cvalue size

-- | Get the high-level representation of the current value associated with the
-- given object. The value is converted from a low-level representation with
-- the requested format.
--
-- The 'SuppressValue' format cannot be used for this function. Requesting this
-- as the format will throw an 'InvalidFormat' exception.
--
-- For more information about safety, see 'Receive' and 'UnsafeReceive'.
--
receiveValue
  :: forall a
   . HasCallStack
  => Coercible a Object
  => Show a
  => Typeable a
  => ValueFormat
  -> a
  -> IO Value
receiveValue fmt object = do
  ptr <- getValue fmt $ coerce @a @Object object
  cvalue <- FFI.peek ptr
  size <- getProperty Size object
  receive $ CValueSized cvalue size

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

-- | Update the value of the given object via the given 'DelayMode'. The value
-- is unsafely sent, meaning if the high-level representation is reclaimed by
-- the garbage collector, the behaviour of the FFI call is undefined.
--
-- The 'SuppressTime' format and 'ObjTypeFmt' formats cannot be used for this
-- function. Attempting to send a value with these formats will throw an
-- 'InvalidFormat' exception.
--
-- For more information about safety, see 'Send' and 'UnsafeSend'.
--
unsafeSendValue
  :: HasCallStack
  => Coercible a Object
  => a
  -> Value
  -> DelayMode
  -> IO ()
  -- No return object, see NOTE [vpi_put_value and events]
unsafeSendValue object value delay =
  -- Safe use of castPtr to turn (CValue, CInt) into CValue
  unsafePokeSend value $ \ptr -> do
    let valuePtr = FFI.castPtr ptr
    (timePtr, flags) <- send delay
    Monad.void $ c_vpi_put_value (coerce object) valuePtr timePtr flags

-- | Update the value of the given object via the given 'DelayMode'. The value
-- is safely sent, meaning the FFI call will succeed if the inputs to this
-- function are valid.
--
-- The 'SuppressTime' format and 'ObjTypeFmt' formats cannot be used for this
-- function. Attempting to send a value with these formats will throw an
-- 'InvalidFormat' exception.
--
-- For more information about safety, see 'Send' and 'UnsafeSend'.
--
sendValue
  :: HasCallStack
  => Coercible a Object
  => a
  -> Value
  -> DelayMode
  -> IO ()
  -- No return object, see NOTE [vpi_put_value and events]
sendValue object value delay = do
  -- Safe use of castPtr to turn (CValue, CInt) into CValue
  valuePtr <- FFI.castPtr <$> pokeSend value
  (timePtr, flags) <- send delay
  Monad.void $ c_vpi_put_value (coerce object) valuePtr timePtr flags
