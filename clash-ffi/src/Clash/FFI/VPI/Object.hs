{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Clash.FFI.VPI.Object
  ( Handle(..)
  , nullHandle
  , isNullHandle
  , freeHandle
  , compareHandles
  , iterateHandle
  , HandleRef(..)
  , UnknownHandle(..)
  , ObjectType(..)
  ) where

import           Control.Exception (Exception)
import qualified Control.Monad as Monad (unless, void, when)
import qualified Control.Monad.IO.Class as IO (liftIO)
import qualified Data.List as List (genericLength)
import           Data.Maybe (fromMaybe)
import           Foreign.C.String (CString)
import           Foreign.C.Types
import qualified Foreign.Concurrent as FFI (newForeignPtr)
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

#if !MIN_VERSION_base(4,15,0)
import qualified Foreign.ForeignPtr as FFI (withForeignPtr)
#endif

import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import           Foreign.Storable (Storable)

#if MIN_VERSION_base(4,15,0)
import qualified GHC.ForeignPtr as FFI (unsafeWithForeignPtr)
#endif

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View (unsafeSend)
import           Clash.FFI.VPI.Object.Type

newtype Handle = Handle { handleToPtr :: Ptr Handle }
  deriving newtype (Show, Storable)

nullHandle :: Handle
nullHandle = Handle FFI.nullPtr

isNullHandle :: Handle -> Bool
isNullHandle (Handle ptr) =
  ptr == FFI.nullPtr

#if defined(VERILOG)
foreign import ccall "vpi_user.h vpi_free_object"
  c_vpi_free_object :: Handle -> IO CInt
#endif

#if defined(SYSTEMVERILOG)
foreign import ccall "vpi_user.h vpi_release_handle"
  c_vpi_release_handle :: Handle -> IO CInt
#endif

c_vpi_free :: Handle -> IO CInt
c_vpi_free =
#if defined(VERILOG)
  c_vpi_free_object
#elif defined(SYSTEMVERILOG)
  c_vpi_release_handle
#else
  error "VPI: Neither VERILOG or SYSTEMVERILOG is defined"
#endif

freeHandle :: HasCallStack => Handle -> SimCont o ()
freeHandle handle =
  Monad.unless (isNullHandle handle) $
    IO.liftIO $ Monad.void (c_vpi_free handle)

foreign import ccall "vpi_user.h vpi_compare_objects"
  c_vpi_compare_objects :: Handle -> Handle -> IO Bool

compareHandles :: Handle -> Handle -> SimCont o Bool
compareHandles x y =
  IO.liftIO (c_vpi_compare_objects x y)

foreign import ccall "vpi_user.h vpi_iterate"
  c_vpi_iterate :: CInt -> Handle -> IO Handle

foreign import ccall "vpi_user.h vpi_scan"
  c_vpi_scan :: Handle -> IO Handle

{-
NOTE [ForeignPtr for iterators]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When using objects created in FFI calls, we can choose to keep them as Ptr,
or convert them to ForeignPtr. The difference is that a ForeignPtr can have
associated finalizers, which are run when the ForeignPtr is garbage collected
by the Haskell RTS.

An earlier design of this library used ForeignPtr for all created handles,
however this causes a problem. Since Haskell is called from the simulator it
is likely that any created handles are deallocated shortly after any Haskell
callbacks complete and give control back to the simulator. For handles inside
callbacks this is fatal, as the callback will try to access the object it acts
on only to find it has been deallocated.

For iterators, we can make the assumption that user code will not attempt to
save and restore an iterator later. Making the iterator a ForeignPtr is
desirable, as it means we can lazily perform the FFI calls to get items without
having to also return a handle to the iterator to be freed later. Handles
returned by vpiIterate still require explicit deallocation.
-}

iterateHandle :: ObjectType -> Maybe Handle -> SimCont o [Handle]
iterateHandle ty parent = do
  cty  <- unsafeSend ty

  IO.liftIO $ do
    iHdl <- c_vpi_iterate cty (fromMaybe nullHandle parent)

    let iPtr = handleToPtr iHdl
    iterator <- FFI.newForeignPtr iPtr (Monad.void $ c_vpi_free iHdl)
    -- See NOTE [ForeignPtr for iterators]

    go iterator
 where
  go iterator =
#if MIN_VERSION_base(4,15,0)
    FFI.unsafeWithForeignPtr iterator $ \ptr ->
#else
    FFI.withForeignPtr iterator $ \ptr ->
#endif
      if ptr == FFI.nullPtr then pure [] else do
        nextChild <- c_vpi_scan (Handle ptr)

        if isNullHandle nextChild
          then pure []
          else fmap (nextChild :) (go iterator)

foreign import ccall "vpi_user.h vpi_handle"
  c_vpi_handle :: CInt -> Handle -> IO Handle

foreign import ccall "vpi_user.h vpi_handle_by_name"
  c_vpi_handle_by_name :: CString -> Handle -> IO Handle

foreign import ccall "vpi_user.h vpi_handle_by_index"
  c_vpi_handle_by_index :: Handle -> CInt -> IO Handle

foreign import ccall "vpi_user.h vpi_handle_by_multi_index"
  c_vpi_handle_by_multi_index :: Handle -> CInt -> Ptr CInt -> IO Handle

data UnknownHandle a
  = UnknownHandle a Handle CallStack
  deriving anyclass (Exception)

instance (Show a) => Show (UnknownHandle a) where
  show (UnknownHandle x h c) =
    mconcat
      [ "Unknown child for handle "
      , show h
      , " accessed via the type/name/index "
      , show x
      , "\n"
      , prettyCallStack c
      ]

class HandleRef a where
  childHandle :: HasCallStack => a -> Handle -> SimCont o Handle

instance HandleRef ObjectType where
  childHandle ty parent = do
    cty <- unsafeSend ty
    child <- IO.liftIO (c_vpi_handle cty parent)

    Monad.when (isNullHandle child) $
      Sim.throw (UnknownHandle ty parent callStack)

    pure child

instance HandleRef CString where
  childHandle str parent = do
    child <- IO.liftIO (c_vpi_handle_by_name str parent)

    Monad.when (isNullHandle child) $
      Sim.throw (UnknownHandle str parent callStack)

    pure child

instance HandleRef CInt where
  childHandle ix parent = do
    child <- IO.liftIO (c_vpi_handle_by_index parent ix)

    Monad.when (isNullHandle child) $
      Sim.throw (UnknownHandle ix parent callStack)

    pure child

instance HandleRef [CInt] where
  childHandle ixs parent = do
    let len = List.genericLength ixs
    ptr <- unsafeSend ixs
    child <- IO.liftIO (c_vpi_handle_by_multi_index parent len ptr)

    Monad.when (isNullHandle child) $
      Sim.throw (UnknownHandle ixs parent callStack)

    pure child

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

