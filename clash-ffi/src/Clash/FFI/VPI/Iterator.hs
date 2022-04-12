{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE FlexibleContexts #-}

module Clash.FFI.VPI.Iterator
  ( Iterator(..)
  , iterateAll
  , iterate
  , scan
  ) where

import           Prelude hiding (iterate)

import           Control.DeepSeq (NFData, deepseq)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.Coerce
import           Foreign.C.Types (CInt(..))
import           Foreign.Storable (Storable)

import           Clash.FFI.Monad (SimCont)
import           Clash.FFI.View
import           Clash.FFI.VPI.Object

-- | A VPI iterator is created to traverse a one-to-many relationship between
-- objects in a design. Iterators can be used in two different ways:
--
--   * Creating an iterator with 'iterate' and manually stepping with 'scan'.
--     This is similar to how iterators are used in C, with the caller being
--     responsible for freeing the iterator with 'freeObject' if it is not
--     fully traversed.
--
--   * Creating an iterator and fully traversing it with 'iterateAll'. This
--     fetches all elements of the iterator at once, which automatically frees
--     the iterator.
--
newtype Iterator
  = Iterator { iteratorObject :: Object }
  deriving stock (Show)
  deriving newtype (IsObject, Storable)

foreign import ccall "vpi_user.h vpi_iterate"
  c_vpi_iterate :: CInt -> Object -> IO Iterator

-- | Create an iterator for objects of the given type under the specified
-- parent object. If no parent object is given, the iterator iterates over
-- the top-level of the design.
--
-- If this iterator is not fully traversed with 'scan', then it needs to be
-- manually freed with 'freeObject'.
--
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

-- | Yield the next object in an iterator. If all elements have been traversed
-- then @Nothing@ is returned, which also indicates the iterator has been freed
-- (and does not need 'freeObject' to be called on it).
--
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

-- | Create an iterator for objects of a given type under the specified parent
-- object, and completely traverse the iterator using repeated calls to 'scan'.
-- This is generally safer than using 'iterate' and 'scan' separately, but
-- offers less control over iteration.
--
iterateAll
  :: forall c p o
   . IsObject c
  => NFData c
  => Coercible Object c
  => Coercible p Object
  => ObjectType
  -> Maybe p
  -> SimCont o [c]
iterateAll objTy parent = do
  iterator <- iterate objTy parent
  items <- takeWhileNonNull iterator

  -- We have to evalute to NF here to prevent leaking the iterator if the list
  -- is not fully forced by the caller.
  items `deepseq` pure items
 where
  -- We do not need to free the iterator here, when scan is called at the last
  -- item the iterator is automatically deallocated (according to the spec).
  takeWhileNonNull iterator = do
    scanned <- scan iterator

    case scanned of
      Just next -> fmap (next :) (takeWhileNonNull iterator)
      Nothing   -> pure []

