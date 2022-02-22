{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Clash.FFI.VPI.Property
  ( Property
      ( VpiObjectType
      , VpiName
      , VpiFullName
      , VpiSize
      , VpiFile
      , VpiLineNo
      , VpiScalar
      , VpiVector
      , VpiDirection
      , VpiNetType
      , VpiPortIndex
      , VpiConstType
#if defined(VERILOG_2001)
      , VpiSigned
      , VpiLocalParam
#endif
      )
  , PropertyType(..)
  , coerceProperty
  , unsafeReceiveProperty
  , receiveProperty
  , UndefinedProperty(..)
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
import           Foreign.Storable (Storable)
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View (UnsafeReceive(..), Receive(..))
import           Clash.FFI.VPI.Object (Handle(..), ObjectType)

newtype Property value = Property CInt
  deriving newtype (Eq, Show, Storable)

pattern VpiObjectType :: Property ObjectType
pattern VpiObjectType = Property 1

pattern VpiName :: Property CString
pattern VpiName = Property 2

pattern VpiFullName :: Property CString
pattern VpiFullName = Property 3

pattern VpiSize :: Property CInt
pattern VpiSize = Property 4

pattern VpiFile :: Property CString
pattern VpiFile = Property 5

pattern VpiLineNo :: Property CInt
pattern VpiLineNo = Property 6

pattern VpiScalar :: Property Bool
pattern VpiScalar = Property 17

pattern VpiVector :: Property Bool
pattern VpiVector = Property 18

pattern VpiDirection :: Property CInt
pattern VpiDirection = Property 20

pattern VpiNetType :: Property CInt
pattern VpiNetType = Property 22

pattern VpiPortIndex :: Property CInt
pattern VpiPortIndex = Property 29

pattern VpiConstType :: Property CInt
pattern VpiConstType = Property 40

#if defined(VERILOG_2001)
pattern VpiSigned :: Property Bool
pattern VpiSigned = Property 65

pattern VpiLocalParam :: Property Bool
pattern VpiLocalParam = Property 70
#endif

foreign import ccall "vpi_user.h vpi_get"
  c_vpi_get :: Property CInt -> Handle -> IO CInt

#if defined(SYSTEMVERILOG)
foreign import ccall "vpi_user.h vpi_get64"
  c_vpi_get64 :: Property Int64 -> Handle -> IO Int64
#endif

foreign import ccall "vpi_user.h vpi_get_str"
  c_vpi_get_str :: Property CString -> Handle -> IO CString

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
  => (Property a -> Handle -> IO a)
  -> a
  -> Property a
  -> Handle
  -> SimCont o a
getPropertyWith f err prop hdl = do
  value <- IO.liftIO (f prop hdl)

  Monad.when (value == err) $
    Sim.throw (UndefinedProperty prop hdl callStack)

  pure value

instance PropertyType CInt where
  getProperty =
    getPropertyWith c_vpi_get (-1)

instance PropertyType Bool where
  getProperty (Property prop) =
    fmap FFI.toBool . getProperty @CInt (Property prop)

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

