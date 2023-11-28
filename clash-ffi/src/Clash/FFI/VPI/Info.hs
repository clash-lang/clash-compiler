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
  , withInfo
  , receiveSimulatorInfo
  , unsafeReceiveSimulatorInfo
  ) where

import           Control.Exception (Exception, throwIO)
import qualified Control.Monad as Monad (unless)
import           Data.ByteString (ByteString)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import qualified Foreign.Marshal.Alloc as FFI (alloca, malloc)
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics (Generic)
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

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
-- this may include / remove arguments recognized by the simulator (i.e. it
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
  unsafeReceive CInfo{..} = do
    -- When passing +RTS to some simulators, they may replace the whole
    -- argument with NULL, so we check in addition to argc.
    infoArgs <- unsafeReceiveArray0 (fromEnum cinfoArgc) FFI.nullPtr cinfoArgv
    infoProduct <- unsafeReceive cinfoProduct
    infoVersion <- unsafeReceive cinfoVersion

    pure Info{..}

instance Receive Info where
  receive CInfo{..} = do
    infoArgs <- receiveArray0 (fromEnum cinfoArgc) FFI.nullPtr cinfoArgv
    infoProduct <- receive cinfoProduct
    infoVersion  <- receive cinfoVersion

    pure Info{..}

foreign import ccall "vpi_user.h vpi_get_vlog_info"
  c_vpi_get_vlog_info :: Ptr CInfo -> IO Bool

-- | An exception thrown when the VPI call to get the simulator info fails.
--
newtype CouldNotGetInfo
  = CouldNotGetInfo CallStack
  deriving anyclass (Exception)

instance Show CouldNotGetInfo where
  show = \case
    CouldNotGetInfo c -> mconcat
      [ "Could not identify the running simulator\n"
      , prettyCallStack c
      ]

-- | Get the low-level representation of the simulator information, which is
-- allocated on the heap. This can be converted to the high-level representation
-- using 'Receive'. If only the high-level representation is needed then consider
-- using 'receiveSimulatorInfo' or 'unsafeReceiveSimulatorInfo' instead.
--
getInfo
  :: HasCallStack
  => IO (Ptr CInfo)
getInfo = do
  ptr <- FFI.malloc
  isSuccess <- c_vpi_get_vlog_info ptr

  Monad.unless isSuccess $
    throwIO $ CouldNotGetInfo callStack

  return ptr

-- | Get the low-level representation of the simulator information, which is
-- allocated on the stack. This can be converted to the high-level representation
-- using 'Receive'. If only the high-level representation is needed then consider
-- using 'receiveSimulatorInfo' or 'unsafeReceiveSimulatorInfo' instead.
--
withInfo
  :: HasCallStack
  => (Ptr CInfo -> IO a) -> IO a
withInfo f =
  FFI.alloca $ \ptr -> do
    isSuccess <- c_vpi_get_vlog_info ptr

    Monad.unless isSuccess $
      throwIO $ CouldNotGetInfo callStack

    f ptr

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
  :: HasCallStack
  => IO Info
unsafeReceiveSimulatorInfo =
  withInfo unsafePeekReceive

-- | Get the high-level representation of the simulator information. The value
-- is safely read, meaning it will not become corrupted if the low-level
-- representation is deallocated.
--
-- For more information about safety, see 'Receive' and 'UnsafeReceive'.
--
receiveSimulatorInfo
  :: HasCallStack
  => IO Info
receiveSimulatorInfo =
  getInfo >>= peekReceive
