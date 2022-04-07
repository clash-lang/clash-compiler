{-# LANGUAGE TypeFamilies #-}

-- Used to improve the performance of derived instances.
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v0 #-}

module Clash.FFI.VPI.Info
  ( CInfo(..)
  , Info(..)
  , UnknownSimulator(..)
  , simulatorInfo
  ) where

import           Control.Exception (Exception)
import qualified Control.Exception as IO (throwIO)
import qualified Control.Monad as Monad (unless)
import           Data.ByteString (ByteString)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (withNewPtr)
import           Clash.FFI.View

data CInfo = CInfo
  { cinfoArgc    :: CInt
  , cinfoArgv    :: Ptr CString
  , cinfoProduct :: CString
  , cinfoVersion :: CString
  }
  deriving stock (Generic, Show)
  deriving anyclass (GStorable)

data Info = Info
  { infoArgs    :: [ByteString]
  , infoProduct :: ByteString
  , infoVersion :: ByteString
  }
  deriving stock (Show)

instance UnsafeReceive Info where
  type Received Info = CInfo

  unsafeReceive cinfo = do
    -- When passing +RTS to some simulators, they may replace the whole
    -- argument with NULL, so we check for that instead of using argc.
    args <- unsafeReceiveArray0 FFI.nullPtr (cinfoArgv cinfo)
    prod <- unsafeReceive (cinfoProduct cinfo)
    ver  <- unsafeReceive (cinfoVersion cinfo)

    pure (Info args prod ver)

instance Receive Info where
  receive cinfo = do
    args <- receiveArray0 FFI.nullPtr (cinfoArgv cinfo)
    prod <- receive (cinfoProduct cinfo)
    ver  <- receive (cinfoVersion cinfo)

    pure (Info args prod ver)

foreign import ccall "vpi_user.h vpi_get_vlog_info"
  c_vpi_get_vlog_info :: Ptr CInfo -> IO Bool

data UnknownSimulator
  = UnknownSimulator CallStack
  deriving anyclass (Exception)

instance Show UnknownSimulator where
  show (UnknownSimulator c) =
    mconcat
      [ "Could not identify the running simulator\n"
      , prettyCallStack c
      ]

simulatorInfo
  :: forall o
   . HasCallStack
  => SimCont o (Ptr CInfo)
  -> SimCont o (Ptr CInfo)
simulatorInfo alloc =
  fmap fst . Sim.withNewPtr alloc $ \ptr -> do
    isSuccess <- c_vpi_get_vlog_info ptr

    Monad.unless isSuccess $
      IO.throwIO (UnknownSimulator callStack)

    pure isSuccess

