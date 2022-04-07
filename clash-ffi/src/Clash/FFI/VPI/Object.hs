{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Clash.FFI.VPI.Object
  ( Object(..)
  , HandleObject(..)
  , module Clash.FFI.VPI.Object.Type
  ) where

import qualified Control.Monad as Monad (unless, void, when)
import qualified Control.Monad.IO.Class as IO (liftIO)
import qualified Data.List as List (genericLength)
import           Foreign.C.String (CString)
import           Foreign.C.Types
import           GHC.Stack (callStack)

import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import           Foreign.Storable (Storable)

import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View (unsafeSend)
import           Clash.FFI.VPI.Handle
import           Clash.FFI.VPI.Object.Type

class Handle a => HandleObject a where
  handleAsObject :: a -> Object

newtype Object
  = Object { objectPtr :: Ptr Object }
  deriving stock (Show)
  deriving newtype (Storable)

instance HandleObject Object where
  handleAsObject = id

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

foreign import ccall "vpi_user.h vpi_handle"
  c_vpi_handle :: CInt -> Object -> IO Object

-- TODO Should I have another one here with Method instead of ObjectType? The
-- VPI spec seems to conflate the two but methods can only be used for
-- traversing the hierarchy from some known point *I think*
instance HandleChild ObjectType Object where
  -- TODO Maybe we could know this if we make ObjectType a GADT
  type ChildHandle ObjectType Object = Object

  childHandle objTy parent = do
    cobjTy <- unsafeSend objTy
    child <- IO.liftIO (c_vpi_handle cobjTy parent)

    Monad.when (isNullHandle child) $
      Sim.throw (UnknownChild objTy parent callStack)

    pure child

foreign import ccall "vpi_user.h vpi_handle_by_name"
  c_vpi_handle_by_name :: CString -> Object -> IO Object

instance HandleChild CString Object where
  type ChildHandle CString Object = Object

  childHandle str parent = do
    child <- IO.liftIO (c_vpi_handle_by_name str parent)

    Monad.when (isNullHandle child) $
      Sim.throw (UnknownChild str parent callStack)

    pure child

foreign import ccall "vpi_user.h vpi_handle_by_index"
  c_vpi_handle_by_index :: Object -> CInt -> IO Object

instance HandleChild CInt Object where
  -- We don't know any better than "Object" here
  type ChildHandle CInt Object = Object

  childHandle ix parent = do
    child <- IO.liftIO (c_vpi_handle_by_index parent ix)

    Monad.when (isNullHandle child) $
      Sim.throw (UnknownChild ix parent callStack)

    pure child

foreign import ccall "vpi_user.h vpi_handle_by_multi_index"
  c_vpi_handle_by_multi_index :: Object -> CInt -> Ptr CInt -> IO Object

instance HandleChild [CInt] Object where
  type ChildHandle [CInt] Object = Object

  childHandle ixs parent = do
    let len = List.genericLength ixs
    ptr <- unsafeSend ixs
    child <- IO.liftIO (c_vpi_handle_by_multi_index parent len ptr)

    Monad.when (isNullHandle child) $
      Sim.throw (UnknownChild ixs parent callStack)

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

