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

simPutStr
  :: forall o
   . HasCallStack
  => ByteString
  -> SimCont o ()
simPutStr =
  unsafeSend >=> IO.liftIO . Monad.void . c_vpi_printf

simPutStrLn :: HasCallStack => ByteString -> SimCont o ()
simPutStrLn =
  simPutStr . (`BS.snoc` '\n')

newtype CouldNotFlushIO
  = CouldNotFlushIO CallStack
  deriving anyclass (Exception)

instance Show CouldNotFlushIO where
  show (CouldNotFlushIO c) =
    "Could not flush simulator output buffers\n" <> prettyCallStack c

foreign import ccall "vpi_user.h vpi_flush"
  c_vpi_flush :: IO Bool

simFlushIO :: HasCallStack => SimCont o ()
simFlushIO = do
  failed <- IO.liftIO c_vpi_flush

  Monad.when failed $
    Sim.throw (CouldNotFlushIO callStack)

  pure ()

