{-# LANGUAGE FlexibleContexts #-}

module Clash.FFI.VPI.Iterator
  ( Iterator(..)
  , iterate
  , iterateAll
  , scan
  ) where

import           Prelude hiding (iterate)

import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.Coerce
import           Foreign.C.Types (CInt(..))
import           Foreign.Storable (Storable)

import           Clash.FFI.Monad (SimCont)
import           Clash.FFI.View
import           Clash.FFI.VPI.Handle (Handle(..))
import           Clash.FFI.VPI.Object

newtype Iterator
  = Iterator { iteratorObject :: Object }
  deriving stock (Show)
  deriving newtype (Handle, Storable)

instance HandleObject Iterator where
  handleAsObject = iteratorObject

foreign import ccall "vpi_user.h vpi_iterate"
  c_vpi_iterate :: CInt -> Object -> IO Iterator

iterate
  :: HandleObject parent
  => ObjectType
  -> Maybe parent
  -> SimCont o Iterator
iterate objTy parent = do
  cobjTy <- unsafeSend objTy
  let object = maybe nullHandle handleAsObject parent

  IO.liftIO (c_vpi_iterate cobjTy object)

foreign import ccall "vpi_user.h vpi_scan"
  c_vpi_scan :: Iterator -> IO Object

scan
  :: (Coercible Object child, Handle child)
  => Iterator
  -> SimCont o (Maybe child)
scan iterator
  | isNullHandle (handleAsObject iterator)
  = pure Nothing

  | otherwise
  = do next <- IO.liftIO (c_vpi_scan iterator)
       pure (if isNullHandle next then Nothing else Just (coerce next))

iterateAll
  :: HandleObject parent
  => Coercible Object child
  => Handle child
  => ObjectType
  -> Maybe parent
  -> SimCont o [child]
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

