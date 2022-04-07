{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Clash.FFI.VPI.Property
  ( PropertyType(..)
  , coerceProperty
  , unsafeReceiveProperty
  , receiveProperty
  , UndefinedProperty(..)
  , module Clash.FFI.VPI.Property.Type
  ) where

import           Control.Exception (Exception)
import qualified Control.Monad as Monad (when)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.Coerce
import           Data.Typeable (Typeable)

#if defined(SYSTEMVERILOG)
import           Data.Int (Int64)
#endif

import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import qualified Foreign.Marshal.Utils as FFI (toBool)
import qualified Foreign.Ptr as FFI (nullPtr)
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import           Unsafe.Coerce

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View
import           Clash.FFI.VPI.Object
import           Clash.FFI.VPI.Property.Type

foreign import ccall "vpi_user.h vpi_get"
  c_vpi_get :: CInt -> Object -> IO CInt

#if defined(SYSTEMVERILOG)
foreign import ccall "vpi_user.h vpi_get64"
  c_vpi_get64 :: CInt -> Object -> IO Int64
#endif

foreign import ccall "vpi_user.h vpi_get_str"
  c_vpi_get_str :: CInt -> Object -> IO CString

data UndefinedProperty p a
  = UndefinedProperty (Property p) a CallStack
  deriving anyclass (Exception)

instance (Show a) => Show (UndefinedProperty p a) where
  show (UndefinedProperty p a c) =
    mconcat
      [ "Undefined property "
      , show p
      , " for the handle "
      , show a
      , "\n"
      , prettyCallStack c
      ]

class PropertyType a where
  getProperty :: (HasCallStack, Show handle, Typeable handle, HandleObject handle) => Property a -> handle -> SimCont o a

getPropertyWith
  :: (HasCallStack, Show handle, Typeable handle, HandleObject handle, Eq a, Show a, Typeable a)
  => (CInt -> Object -> IO a)
  -> a
  -> Property a
  -> handle
  -> SimCont o a
getPropertyWith f err prop handle = do
  cprop <- unsafeSend prop
  value <- IO.liftIO (f cprop (handleAsObject handle))

  Monad.when (value == err) $
    Sim.throw (UndefinedProperty prop handle callStack)

  pure value

instance PropertyType CInt where
  getProperty =
    getPropertyWith c_vpi_get (-1)

instance PropertyType Bool where
  getProperty prop =
    -- This use of unsafeCoerce is safe because the parameter is phantom,
    -- and FFI calls which return booleans in the spec actually return CInt.
    fmap FFI.toBool . getProperty @CInt (unsafeCoerce prop)

#if defined(SYSTEMVERILOG)
instance PropertyType Int64 where
  getProperty =
    getPropertyWith c_vpi_get64 (-1)
#endif

instance PropertyType CString where
  getProperty =
    getPropertyWith c_vpi_get_str FFI.nullPtr

coerceProperty
  :: (HasCallStack, PropertyType a, Coercible a b, Show handle, Typeable handle, HandleObject handle)
  => Property a
  -> handle
  -> SimCont o b
coerceProperty prop =
  fmap coerce . getProperty prop . handleAsObject

unsafeReceiveProperty
  :: (HasCallStack, UnsafeReceive a, PropertyType (Received a), HandleObject handle, Show handle, Typeable handle)
  => Property (Received a)
  -> handle
  -> SimCont o a
unsafeReceiveProperty prop handle =
  getProperty prop (handleAsObject handle) >>= unsafeReceive

receiveProperty
  :: (HasCallStack, Receive a, PropertyType (Received a), HandleObject handle, Show handle, Typeable handle)
  => Property (Received a)
  -> handle
  -> SimCont o a
receiveProperty prop handle =
  getProperty prop (handleAsObject handle) >>= receive

