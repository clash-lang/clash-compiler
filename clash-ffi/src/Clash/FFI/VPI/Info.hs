{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE TypeFamilies #-}

-- Used to improve the performance of derived instances.
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v0 #-}

module Clash.FFI.VPI.Info
  ( CInfo(..)
  , Info(..)
  , CouldNotGetInfo(..)
  , getInfo
  , receiveSimulatorInfo
  , unsafeReceiveSimulatorInfo
  ) where

import           Control.Exception (Exception)
import qualified Control.Exception as IO (throwIO)
import qualified Control.Monad as Monad (unless)
import           Data.ByteString (ByteString)
import           Data.Typeable (Typeable)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (stackPtr, withNewPtr)
import           Clash.FFI.View

-- | The low-level representation of the VPI information structure, as returned
-- by the @vpi_get_vlog_info@ function. This can optionally be converted to an
-- 'Info' using 'Receive'.
--
data CInfo = CInfo
  { cinfoArgc    :: CInt
  , cinfoArgv    :: Ptr CString
  , cinfoProduct :: CString
  , cinfoVersion :: CString
  }
  deriving stock (Generic, Show)
  deriving anyclass (GStorable)

-- | Information about the simulator connected to over VPI. This includes the
-- command line used to start the simulation tool. Depending on the simulator
-- this may include / remove arguments recognised by the simulator (i.e. it
-- will only contain other flags like RTS flags).
--
data Info = Info
  { infoArgs    :: [ByteString]
  , infoProduct :: ByteString
  , infoVersion :: ByteString
  }
  deriving stock (Show)

type instance CRepr Info = CInfo

instance UnsafeReceive Info where
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

-- | An exception thrown when the VPI call to get the simulator info fails.
--
data CouldNotGetInfo
  = CouldNotGetInfo CallStack
  deriving anyclass (Exception)

instance Show CouldNotGetInfo where
  show (CouldNotGetInfo c) =
    mconcat
      [ "Could not identify the running simulator\n"
      , prettyCallStack c
      ]

-- | Get the low-level representation of the simulator information. This can be
-- converted to the high-level representation using 'Receive'. If only the
-- high-level representation is needed then consider using
-- 'receiveSimulatorInfo' or 'unsafeReceiveSimulatorInfo' instead.
--
getInfo
  :: forall o
   . HasCallStack
  => SimCont o (Ptr CInfo)
  -> SimCont o (Ptr CInfo)
getInfo alloc =
  fmap fst . Sim.withNewPtr alloc $ \ptr -> do
    isSuccess <- c_vpi_get_vlog_info ptr

    Monad.unless isSuccess $
      IO.throwIO (CouldNotGetInfo callStack)

    pure isSuccess

-- | Get the high-level representation of the simulator information. The value
-- is unsafely read, meaning it may be corrupted if the low-level
-- representation is deallocated.
--
-- The low-level representation is allocated on the stack, meaning it will not
-- survive past the end of the current callback.
--
-- For more information about safety, see 'Receive' and 'UnsafeReceive'.
--
unsafeReceiveSimulatorInfo
  :: forall o
   . HasCallStack
  => Typeable o
  => SimCont o Info
unsafeReceiveSimulatorInfo =
  getInfo Sim.stackPtr >>= unsafePeekReceive

-- | Get the high-level representation of the simulator information. The value
-- is safely read, meaning it will not become corrupted if the low-level
-- representation is deallocated.
--
-- For more information about safety, see 'Receive' and 'UnsafeReceive'.
--
receiveSimulatorInfo
  :: forall o
   . HasCallStack
  => Typeable o
  => SimCont o Info
receiveSimulatorInfo =
  getInfo Sim.stackPtr >>= peekReceive
