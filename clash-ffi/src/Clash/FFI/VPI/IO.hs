{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

module Clash.FFI.VPI.IO
  ( simPutStr
  , simPutStrLn
  , simFlushIO
  ) where

import           Control.Exception (Exception, throwIO)
import qualified Control.Monad as Monad (void, when)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (snoc)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import           Clash.FFI.View (unsafeSend, ensureNullTerminated)

foreign import ccall "vpi_user.h vpi_printf"
  c_vpi_printf :: CString -> IO CInt

-- | A version of 'putStr' which outputs to the handle used by the simulator.
-- When running a VPI callback, the normal functions provided in @base@ may
-- not output anything in some simulators, but this function will.
--
simPutStr
  :: HasCallStack
  => ByteString
  -> IO ()
simPutStr bs =
  unsafeSend (ensureNullTerminated bs)
    $ Monad.void . c_vpi_printf

-- | A version of 'putStrLn' which outputs to the handle used by the simulator.
-- When running a VPI callback, the normal functions provided in @base@ may
-- not output anything in some simulators, but this function will.
--
simPutStrLn :: HasCallStack => ByteString -> IO ()
simPutStrLn =
  simPutStr . (`BS.snoc` '\n')

-- | An exception thrown when VPI could not flush the IO output buffer
-- controlled by the simulator.
--
newtype CouldNotFlushIO
  = CouldNotFlushIO CallStack
  deriving anyclass (Exception)

instance Show CouldNotFlushIO where
  show (CouldNotFlushIO c) =
    "Could not flush simulator output buffers\n" <> prettyCallStack c

foreign import ccall "vpi_user.h vpi_flush"
  c_vpi_flush :: IO Bool

-- | Flush the IO output buffer controlled by the simulator.
--
simFlushIO :: HasCallStack => IO ()
simFlushIO = do
  failed <- c_vpi_flush

  Monad.when failed $
    throwIO $ CouldNotFlushIO callStack
