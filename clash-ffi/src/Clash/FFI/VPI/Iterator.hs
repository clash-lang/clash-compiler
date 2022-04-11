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
import           Clash.FFI.VPI.Object

newtype Iterator
  = Iterator { iteratorObject :: Object }
  deriving stock (Show)
  deriving newtype (IsObject, Storable)

foreign import ccall "vpi_user.h vpi_iterate"
  c_vpi_iterate :: CInt -> Object -> IO Iterator

iterate
  :: forall p o
   . Coercible p Object
  => ObjectType
  -> Maybe p
  -> SimCont o Iterator
iterate objTy parent = do
  cobjTy <- unsafeSend objTy
  let object = maybe nullObject coerce parent

  IO.liftIO (c_vpi_iterate cobjTy object)

foreign import ccall "vpi_user.h vpi_scan"
  c_vpi_scan :: Iterator -> IO Object

scan
  :: forall c o
   . IsObject c
  => Coercible Object c
  => Iterator
  -> SimCont o (Maybe c)
scan iterator
  | isNullObject (iteratorObject iterator)
  = pure Nothing

  | otherwise
  = do next <- IO.liftIO (c_vpi_scan iterator)
       pure (if isNullObject next then Nothing else Just (coerce next))

iterateAll
  :: forall c p o
   . IsObject c
  => Coercible Object c
  => Coercible p Object
  => ObjectType
  -> Maybe p
  -> SimCont o [c]
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

