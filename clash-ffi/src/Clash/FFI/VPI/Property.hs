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
import           Control.Monad ((>=>))
import qualified Control.Monad as Monad (when)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.Coerce

#if defined(SYSTEMVERILOG)
import           Data.Int (Int64)
#endif

import           Data.Typeable (Typeable)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import qualified Foreign.Marshal.Utils as FFI (toBool)
import qualified Foreign.Ptr as FFI (nullPtr)
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)
import           Unsafe.Coerce

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View
import           Clash.FFI.VPI.Object (Handle(..))
import           Clash.FFI.VPI.Property.Type

foreign import ccall "vpi_user.h vpi_get"
  c_vpi_get :: CInt -> Handle -> IO CInt

#if defined(SYSTEMVERILOG)
foreign import ccall "vpi_user.h vpi_get64"
  c_vpi_get64 :: CInt -> Handle -> IO Int64
#endif

foreign import ccall "vpi_user.h vpi_get_str"
  c_vpi_get_str :: CInt -> Handle -> IO CString

data UndefinedProperty a
  = UndefinedProperty (Property a) Handle CallStack
  deriving anyclass (Exception)

instance Show (UndefinedProperty a) where
  show (UndefinedProperty p h c) =
    mconcat
      [ "Undefined property "
      , show p
      , " for the handle "
      , show h
      , "\n"
      , prettyCallStack c
      ]

class PropertyType a where
  getProperty :: HasCallStack => Property a -> Handle -> SimCont o a

getPropertyWith
  :: (HasCallStack, Eq a, Show a, Typeable a)
  => (CInt -> Handle -> IO a)
  -> a
  -> Property a
  -> Handle
  -> SimCont o a
getPropertyWith f err prop hdl = do
  cprop <- unsafeSend prop
  value <- IO.liftIO (f cprop hdl)

  Monad.when (value == err) $
    Sim.throw (UndefinedProperty prop hdl callStack)

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
  :: (HasCallStack, PropertyType a, Coercible a b)
  => Property a
  -> Handle
  -> SimCont o b
coerceProperty prop =
  fmap coerce . getProperty prop

unsafeReceiveProperty
  :: (HasCallStack, UnsafeReceive a, PropertyType (Received a))
  => Property (Received a)
  -> Handle
  -> SimCont o a
unsafeReceiveProperty prop =
  getProperty prop >=> unsafeReceive

receiveProperty
  :: (HasCallStack, Receive a, PropertyType (Received a))
  => Property (Received a)
  -> Handle
  -> SimCont o a
receiveProperty prop =
  getProperty prop >=> receive

