{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

-- Used to improve the performance of derived instances.
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}
{-# OPTIONS_GHC -fplugin-opt=Foreign.Storable.Generic.Plugin:-v0 #-}

module Clash.FFI.VPI.Error
  ( CVpiError(..)
  , VpiError(..)
  , ErrorState(..)
  , ErrorLevel(..)
  , simulationError
  , simulationErrorLevel
  ) where

import           Data.ByteString (ByteString)
import           Foreign.C.String (CString)
import           Foreign.C.Types (CInt(..))
import           Foreign.Ptr (Ptr)
import           Foreign.Storable.Generic (GStorable)
import           GHC.Generics (Generic)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (stackPtr, withNewPtr)
import           Clash.FFI.View (Receive(..), UnsafeReceive(..), receiveString)
import           Clash.FFI.VPI.Error.Level (ErrorLevel(..))
import           Clash.FFI.VPI.Error.State (ErrorState(..))

data CVpiError = CVpiError
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

data VpiError = VpiError
  { errorState    :: ErrorState
  , errorLevel    :: ErrorLevel
  , errorMessage  :: ByteString
  , errorProduct  :: ByteString
  , errorCode     :: ByteString
  , errorFile     :: FilePath
  , errorLine     :: Int
  }

instance UnsafeReceive VpiError where
  type Received VpiError = CVpiError

  unsafeReceive cerror = do
    state <- unsafeReceive (cerrorState cerror)
    level <- unsafeReceive (cerrorLevel cerror)
    msg <- unsafeReceive (cerrorMessage cerror)
    prod <- unsafeReceive (cerrorProduct cerror)
    code <- unsafeReceive (cerrorCode cerror)
    file <- receiveString (cerrorFile cerror)
    let line = fromIntegral (cerrorLine cerror)

    pure (VpiError state level msg prod code file line)

instance Receive VpiError where
  receive cerror = do
    state <- receive (cerrorState cerror)
    level <- receive (cerrorState cerror)
    msg <- receive (cerrorMessage cerror)
    prod <- receive (cerrorProduct cerror)
    code <- receive (cerrorCode cerror)
    file <- receiveString (cerrorFile cerror)
    let line = fromIntegral (cerrorLine cerror)

    pure (VpiError state level msg prod code file line)

foreign import ccall "vpi_user.h vpi_chk_error"
  c_vpi_chk_error :: Ptr CVpiError -> IO CInt

simulationError :: SimCont o (Ptr CVpiError) -> SimCont o (Ptr CVpiError, ErrorLevel)
simulationError alloc = do
  (ptr, clevel) <- Sim.withNewPtr alloc c_vpi_chk_error
  level <- receive clevel

  pure (ptr, level)

simulationErrorLevel :: SimCont o ErrorLevel
simulationErrorLevel =
  snd <$> simulationError Sim.stackPtr

