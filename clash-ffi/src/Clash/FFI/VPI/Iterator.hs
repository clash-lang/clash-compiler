module Clash.FFI.VPI.Iterator
  ( Iterator(..)
  , iterate
  , iterateAll
  , scan
  ) where

import           Prelude hiding (iterate)

import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.Maybe (fromMaybe)
import           Foreign.C.Types (CInt(..))
import           Foreign.Storable (Storable)

import           Clash.FFI.Monad (SimCont)
import           Clash.FFI.View
import           Clash.FFI.VPI.Object

newtype Iterator
  = Iterator { iteratorHandle :: Handle }
  deriving stock (Show)
  deriving newtype (Storable)

foreign import ccall "vpi_user.h vpi_iterate"
  c_vpi_iterate :: CInt -> Handle -> IO Iterator

iterate :: ObjectType -> Maybe Handle -> SimCont o Iterator
iterate objTy parent = do
  cobjTy <- unsafeSend objTy
  let handle = fromMaybe nullHandle parent

  IO.liftIO (c_vpi_iterate cobjTy handle)

foreign import ccall "vpi_user.h vpi_scan"
  c_vpi_scan :: Iterator -> IO Handle

scan :: Iterator -> SimCont o (Maybe Handle)
scan iterator
  | isNullHandle (iteratorHandle iterator)
  = pure Nothing

  | otherwise
  = do next <- IO.liftIO (c_vpi_scan iterator)
       pure (if isNullHandle next then Nothing else Just next)

iterateAll :: ObjectType -> Maybe Handle -> SimCont o [Handle]
iterateAll objTy parent = do
  iterator <- iterate objTy parent
  takeWhileNonNull iterator
 where
  -- We do not need to free the iterator here, when scan is called at the last
  -- item the iterator is automatically deallocated (according to the spec).
  takeWhileNonNull iterator = do
    scanned <- scan iterator

    case scanned of
      Just next -> fmap (next :) (takeWhileNonNull iterator)
      Nothing   -> pure []

