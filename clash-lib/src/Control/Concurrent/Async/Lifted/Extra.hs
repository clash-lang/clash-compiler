{-# LANGUAGE FlexibleContexts #-}

module Control.Concurrent.Async.Lifted.Extra where

import Control.Concurrent.Async.Lifted (mapConcurrently, mapConcurrently_)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl)

zipWithConcurrently ::
  (MonadBaseControl IO m, MonadBase IO m) =>
  (a -> b -> m c) ->
  [a] ->
  [b] ->
  m [c]
zipWithConcurrently f xs ys = mapConcurrently (uncurry f) (zip xs ys)

zipWithConcurrently_ ::
  (MonadBaseControl IO m, MonadBase IO m) =>
  (a -> b -> m c) ->
  [a] ->
  [b] ->
    m ()
zipWithConcurrently_ f xs ys = mapConcurrently_ (uncurry f) (zip xs ys)
