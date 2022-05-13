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

import           Control.Exception (Exception)
import           Control.Monad ((>=>))
import qualified Control.Monad as Monad (void, when)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (snoc)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import           GHC.Stack (CallStack, HasCallStack, callStack, prettyCallStack)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View (unsafeSend)

foreign import ccall "vpi_user.h vpi_printf"
  c_vpi_printf :: CString -> IO CInt

-- | A version of 'putStr' which outputs to the handle used by the simulator.
-- When running a VPI callback, the normal functions provided in @base@ may
-- not output anything in some simulators, but this function will.
--
simPutStr
  :: forall o
   . HasCallStack
  => ByteString
  -> SimCont o ()
simPutStr =
  unsafeSend >=> IO.liftIO . Monad.void . c_vpi_printf

-- | A version of 'putStrLn' which outputs to the handle used by the simulator.
-- When running a VPI callback, the normal functions provided in @base@ may
-- not output anything in some simulators, but this function will.
--
simPutStrLn :: HasCallStack => ByteString -> SimCont o ()
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
simFlushIO :: HasCallStack => SimCont o ()
simFlushIO = do
  failed <- IO.liftIO c_vpi_flush

  Monad.when failed $
    Sim.throw (CouldNotFlushIO callStack)

  pure ()
