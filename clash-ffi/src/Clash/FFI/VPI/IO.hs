module Clash.FFI.VPI.IO
  ( simPutStr
  , simPutStrLn
  ) where

import           Control.Monad ((>=>))
import qualified Control.Monad as Monad (void)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (snoc)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))

import           Clash.FFI.Monad (SimCont)
import           Clash.FFI.View (unsafeSend)

foreign import ccall "vpi_user.h vpi_printf"
  c_vpi_printf :: CString -> IO CInt

simPutStr :: ByteString -> SimCont o ()
simPutStr =
  unsafeSend >=> IO.liftIO . Monad.void . c_vpi_printf

simPutStrLn :: ByteString -> SimCont o ()
simPutStrLn =
  simPutStr . (`BS.snoc` '\n')

