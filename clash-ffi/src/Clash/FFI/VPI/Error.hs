{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

-- Used to improve the performance of derived instances.
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v0 #-}

module Clash.FFI.VPI.Error
  ( CErrorInfo(..)
  , ErrorInfo(..)
  , getErrorInfo
  , receiveErrorLevel
  , receiveErrorInfo
  , unsafeReceiveErrorInfo
  , module Clash.FFI.VPI.Error.Level
  , module Clash.FFI.VPI.Error.State
  ) where

import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.ByteString (ByteString)
import           Data.Typeable (Typeable)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics (Generic)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (stackPtr, withNewPtr)
import           Clash.FFI.View
import           Clash.FFI.VPI.Error.Level
import           Clash.FFI.VPI.Error.State

-- | The low-level representation of a VPI error, as returned by the
-- @vpi_chk_error@ function. This can optionally be converted to an 'ErrorInfo'
-- using 'Receive'.
--
data CErrorInfo = CErrorInfo
  { cerrorState   :: CInt
  , cerrorLevel   :: CInt
  , cerrorMessage :: CString
  , cerrorProduct :: CString
  , cerrorCode    :: CString
  , cerrorFile    :: CString
  , cerrorLine    :: CInt
  }
  deriving stock (Generic)
  deriving anyclass (GStorable)

-- | Information about an error, specifying the type of error, message, and the
-- location in the source verilog where the error occurred.
--
-- For the low-level representation of error values that are sent by VPI calls,
-- see 'CErrorInfo'.
--
data ErrorInfo = ErrorInfo
  { errorState    :: ErrorState
  , errorLevel    :: ErrorLevel
  , errorMessage  :: ByteString
  , errorProduct  :: ByteString
  , errorCode     :: ByteString
  , errorFile     :: FilePath
  , errorLine     :: Int
  }

instance UnsafeReceive ErrorInfo where
  type Received ErrorInfo = CErrorInfo

  unsafeReceive cerror = do
    state <- unsafeReceive (cerrorState cerror)
    level <- unsafeReceive (cerrorLevel cerror)
    msg <- unsafeReceive (cerrorMessage cerror)
    prod <- unsafeReceive (cerrorProduct cerror)
    code <- unsafeReceive (cerrorCode cerror)
    file <- receiveString (cerrorFile cerror)
    let line = fromIntegral (cerrorLine cerror)

    pure (ErrorInfo state level msg prod code file line)

instance Receive ErrorInfo where
  receive cerror = do
    state <- receive (cerrorState cerror)
    level <- receive (cerrorState cerror)
    msg <- receive (cerrorMessage cerror)
    prod <- receive (cerrorProduct cerror)
    code <- receive (cerrorCode cerror)
    file <- receiveString (cerrorFile cerror)
    let line = fromIntegral (cerrorLine cerror)

    pure (ErrorInfo state level msg prod code file line)

foreign import ccall "vpi_user.h vpi_chk_error"
  c_vpi_chk_error :: Ptr CErrorInfo -> IO CInt

-- | Get the low-level representation of the current error information. This
-- can be converted to the high-level representation using 'Receive'. If only
-- the high-level representation is needed then consider using
-- 'receiveErrorInfo' or 'unsafeReceiveErrorInfo' instead.
--
getErrorInfo
  :: forall o
   . Typeable o
  => SimCont o (Ptr CErrorInfo)
  -> SimCont o (Ptr CErrorInfo)
getErrorInfo alloc =
  fst <$> Sim.withNewPtr alloc c_vpi_chk_error

-- | Get the high-level representation of the current error information. The
-- value is unsafely read, meaning it may be corrupted if the low-level
-- representation is deallocated.
--
-- The low-level representation is allocated on the stack, meaning it will not
-- survive past the end of the current callback.
--
-- For more information about safety, see 'Receive' and 'UnsafeReceive'.
--
unsafeReceiveErrorInfo
  :: forall o
   . Typeable o
  => SimCont o ErrorInfo
unsafeReceiveErrorInfo =
  getErrorInfo Sim.stackPtr >>= unsafePeekReceive

-- | Get the high-level representation of the current error information. The
-- value is safely read, meaning it will not become corrupted if the low-level
-- representation is deallocated.
--
-- For more information about safety, see 'Receive' and 'UnsafeReceive'.
--
receiveErrorInfo
  :: forall o
   . Typeable o
  => SimCont o ErrorInfo
receiveErrorInfo =
  getErrorInfo Sim.stackPtr >>= peekReceive

-- | Get the error level of the current error information. For more complete
-- error information, use 'receiveErrorInfo' or 'unsafeReceiveErrorInfo' for
-- the high-level representation, or 'getErrorInfo' for the low-level
-- representation.
--
receiveErrorLevel
  :: forall o
   . Typeable o
  => SimCont o ErrorLevel
receiveErrorLevel =
  IO.liftIO (c_vpi_chk_error FFI.nullPtr) >>= receive

