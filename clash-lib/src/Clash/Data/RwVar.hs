{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  A mutable variable for read-mostly state: reads are lock-free, writes are
  serialized against each other.

  Normalization state such as the global binding map is read orders of
  magnitude more often than it is written (on a large design, ~1.5M reads
  against ~9k writes). Holding a single 'Control.Concurrent.MVar.MVar' for
  those reads makes every reader queue behind every other reader, which is
  what a 'RwVar' avoids: 'readRwVar' never blocks, and only the writers in
  'modifyRwVar' take the lock.
-}

{-# LANGUAGE FlexibleContexts #-}

module Clash.Data.RwVar
  ( RwVar
  , newRwVar
  , readRwVar
  , modifyRwVar
  , modifyRwVar_
  ) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.IORef (IORef)

import qualified Control.Concurrent.MVar.Lifted as MVar
import qualified Data.IORef as IORef

-- | A variable holding read-mostly state.
--
-- The 'IORef' holds the current value; the 'MVar' serializes writers so that a
-- read-modify-write in 'modifyRwVar' is atomic with respect to other writers.
-- Readers ignore the lock entirely, so a reader can observe the value from
-- before a write that is in progress — exactly as it could when a writer had
-- not yet started.
data RwVar a = RwVar (IORef a) (MVar ())

-- | Create a 'RwVar' holding the given value.
newRwVar :: MonadBase IO m => a -> m (RwVar a)
newRwVar a = liftBase (RwVar <$> IORef.newIORef a <*> MVar.newMVar ())

-- | Read the current value. Never blocks.
readRwVar :: MonadBase IO m => RwVar a -> m a
readRwVar (RwVar ref _) = liftBase (IORef.readIORef ref)
{-# INLINE readRwVar #-}

-- | Atomically update the value, and return a result computed alongside it.
--
-- The update runs with the write lock held, so it sees a value no other writer
-- can be modifying. Keep it short: readers are unaffected, but other writers
-- are not.
modifyRwVar :: MonadBaseControl IO m => RwVar a -> (a -> m (a, b)) -> m b
modifyRwVar (RwVar ref lock) f =
  MVar.withMVar lock $ \() -> do
    a0 <- liftBase (IORef.readIORef ref)
    (a1, b) <- f a0
    liftBase (IORef.writeIORef ref a1)
    pure b

-- | 'modifyRwVar' for updates with no result.
modifyRwVar_ :: MonadBaseControl IO m => RwVar a -> (a -> m a) -> m ()
modifyRwVar_ var f = modifyRwVar var (fmap (, ()) . f)
