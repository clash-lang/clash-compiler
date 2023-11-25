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
  , withErrorInfo
  , receiveErrorLevel
  , receiveErrorInfo
  , unsafeReceiveErrorInfo
  , module Clash.FFI.VPI.Error.Level
  , module Clash.FFI.VPI.Error.State
  ) where

import           Data.ByteString (ByteString)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import qualified Foreign.Marshal.Alloc as FFI (alloca, malloc)
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics (Generic)

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

type instance CRepr ErrorInfo = CErrorInfo

instance UnsafeReceive ErrorInfo where
  unsafeReceive CErrorInfo{..} = do
    errorState <- receive cerrorState
    errorLevel <- receive cerrorLevel
    errorMessage <- unsafeReceive cerrorMessage
    errorProduct <- unsafeReceive cerrorProduct
    errorCode <- unsafeReceive cerrorCode
    errorFile <- receiveString cerrorFile
    let errorLine = fromIntegral cerrorLine

    pure ErrorInfo{..}

instance Receive ErrorInfo where
  receive CErrorInfo{..} = do
    errorState <- receive cerrorState
    errorLevel <- receive cerrorLevel
    errorMessage <- receive cerrorMessage
    errorProduct <- receive cerrorProduct
    errorCode <- receive cerrorCode
    errorFile <- receiveString cerrorFile
    let errorLine = fromIntegral cerrorLine

    pure ErrorInfo{..}

foreign import ccall "vpi_user.h vpi_chk_error"
  c_vpi_chk_error :: Ptr CErrorInfo -> IO CInt

-- | Get the low-level representation of the current error information, which
-- is allocated on the heap. This can be converted to the high-level
-- representation using 'Receive'. If only the high-level representation is
-- needed then consider using 'receiveErrorInfo' or 'unsafeReceiveErrorInfo'
-- instead.
--
getErrorInfo :: IO (Ptr CErrorInfo)
getErrorInfo =
  FFI.malloc >>= \ptr -> c_vpi_chk_error ptr >> return ptr

-- | Get the low-level representation of the current error information, which
-- is allocated on the stack. This can be converted to the high-level
-- representation using 'Receive'. If only the high-level representation is
-- needed then consider using 'receiveErrorInfo' or 'unsafeReceiveErrorInfo'
-- instead.
--
withErrorInfo :: (Ptr CErrorInfo -> IO a) -> IO a
withErrorInfo f =
  FFI.alloca $ \ptr -> c_vpi_chk_error ptr >> f ptr

-- | Get the high-level representation of the current error information. The
-- value is unsafely read, meaning it may be corrupted if the low-level
-- representation is deallocated.
--
-- The low-level representation is allocated on the stack, meaning it will not
-- survive past the end of the current callback.
--
-- For more information about safety, see 'Receive' and 'UnsafeReceive'.
--
unsafeReceiveErrorInfo :: IO ErrorInfo
unsafeReceiveErrorInfo =
  withErrorInfo unsafePeekReceive

-- | Get the high-level representation of the current error information. The
-- value is safely read, meaning it will not become corrupted if the low-level
-- representation is deallocated.
--
-- For more information about safety, see 'Receive' and 'UnsafeReceive'.
--
receiveErrorInfo :: IO ErrorInfo
receiveErrorInfo =
  getErrorInfo >>= peekReceive

-- | Get the error level of the current error information. For more complete
-- error information, use 'receiveErrorInfo' or 'unsafeReceiveErrorInfo' for
-- the high-level representation, or 'getErrorInfo' for the low-level
-- representation.
--
receiveErrorLevel :: IO ErrorLevel
receiveErrorLevel =
  c_vpi_chk_error FFI.nullPtr >>= receive
