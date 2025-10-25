{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Clash.Normalize.Locks
  ( withMVarM
  , modifyMVarM
  , readMVarM

  -- * You shouldn't use these
  , takeMVarM
  , putMVarM
  ) where

import Data.List (find)
import Unsafe.Coerce (unsafeCoerce)
import Control.Concurrent.Lifted (myThreadId)
import Control.Concurrent.MVar.Lifted
  (MVar, modifyMVar_, withMVar, tryPutMVar, putMVar, readMVar, takeMVar, modifyMVar)
import Control.Exception.Lifted (onException)
import Control.Monad.IO.Class (liftIO)
import GHC.Stack (HasCallStack, callStack)

import Clash.Rewrite.Types (RewriteMonad, LockTrace(..), extra)
import Clash.Normalize.Types (NormalizeState)

import qualified Clash.Normalize.Types as Normalize
import qualified Clash.Rewrite.Types as Rewrite
import qualified Control.Lens as Lens
import qualified Data.Map.Strict as Map

-- | Get all locks in the 'NormalizeState' with their associated names. This is
-- unsafe as it uses 'unsafeCoerce' to coerce the 'MVar's to a common type.
unsafeGetNamedLocksM :: RewriteMonad NormalizeState [(String, MVar a)]
unsafeGetNamedLocksM = do
  transformCounters <- Lens.use Rewrite.transformCounters
  bindings <- Lens.use Rewrite.bindings
  curFun <- Lens.use Rewrite.curFun
  nameCounter <- Lens.use Rewrite.nameCounter
  globalHeap <- Lens.use Rewrite.globalHeap
  workFreeBinders <- Lens.use Rewrite.workFreeBinders
  ioLock <- Lens.use Rewrite.ioLock

  normalized <- Lens.use (extra . Normalize.normalized)
  specialisationCache <- Lens.use (extra . Normalize.specialisationCache)
  specialisationHistory <- Lens.use (extra . Normalize.specialisationHistory)
  inlineHistory <- Lens.use (extra . Normalize.inlineHistory)
  primitiveArgs <- Lens.use (extra . Normalize.primitiveArgs)
  recursiveComponents <- Lens.use (extra . Normalize.recursiveComponents)

  pure
    [ ("transformCounters", unsafeCoerce transformCounters)
    , ("bindings", unsafeCoerce bindings)
    , ("curFun", unsafeCoerce curFun)
    , ("nameCounter", unsafeCoerce nameCounter)
    , ("globalHeap", unsafeCoerce globalHeap)
    , ("workFreeBinders", unsafeCoerce workFreeBinders)
    , ("ioLock", unsafeCoerce ioLock)

    , ("extra.normalized", unsafeCoerce normalized)
    , ("extra.specialisationCache", unsafeCoerce specialisationCache)
    , ("extra.specialisationHistory", unsafeCoerce specialisationHistory)
    , ("extra.inlineHistory", unsafeCoerce inlineHistory)
    , ("extra.primitiveArgs", unsafeCoerce primitiveArgs)
    , ("extra.recursiveComponents", unsafeCoerce recursiveComponents)
    ]

-- | Given a lock, try to find its name in the 'NormalizeState'
getLockName :: HasCallStack => MVar a -> RewriteMonad NormalizeState (Maybe String)
getLockName lock = do
  -- Safety: we only use the named locks to check for equality of MVars. This is
  -- okay, because MVar equality is implemented as pointer equality and is
  -- therefore not dependent on the specific type.
  namedLocks <- unsafeGetNamedLocksM
  let maybeNamedLock = find ((== lock) . snd) namedLocks
  pure $ fst <$> maybeNamedLock

-- | Given a lock, try to find its name in the 'NormalizeState'. If it does not
-- exist, throw an error.
getLockNameOrError :: HasCallStack => MVar a -> RewriteMonad NormalizeState String
getLockNameOrError lock = do
  maybeName <- getLockName lock
  case maybeName of
    Just name -> pure name
    Nothing -> error "getLockNameOrError: could not find lock name"

-- | Convenience wrapper around 'getLockNameOrError' that passes the lock name to
-- the given function.
withLockNameOrError ::
  HasCallStack =>
  MVar a ->
  (String -> RewriteMonad NormalizeState b) ->
  RewriteMonad NormalizeState b
withLockNameOrError lock f = do
  lockName <- getLockNameOrError lock
  f lockName

-- | Modify the 'LockTrace' for a given lock name in the 'RewriteMonad'. If no
-- 'LockTrace' exists for the given lock name, a new empty one is created.
modifyDebugLockTraceM_ ::
  HasCallStack =>
  String ->
  (LockTrace -> RewriteMonad NormalizeState LockTrace) ->
  RewriteMonad NormalizeState ()
modifyDebugLockTraceM_ lockName f = do
  debugLockTrace <- Lens.use Rewrite.debugLockTrace
  modifyMVar_ debugLockTrace $ \traceMap -> do
    let lockTrace = Map.findWithDefault (LockTrace Nothing Map.empty) lockName traceMap
    newLockTrace <- f lockTrace
    pure (Map.insert lockName newLockTrace traceMap)

-- | Set the 'LockTrace' to indicate that the current thread is waiting on
-- the lock. Errors if the lock is already taken by the current thread, or if
-- the current thread is already waiting on the lock.
setWaiting :: (HasCallStack) => LockTrace -> RewriteMonad NormalizeState LockTrace
setWaiting LockTrace{ltTaken, ltWaiting} = do
  threadId <- myThreadId
  case (ltTaken, Map.lookup threadId ltWaiting) of
    (Just (tId, _), _) | tId == threadId ->
      error "setWaiting: current thread already holds the lock"
    _ ->
      pure $ LockTrace ltTaken (Map.insert threadId callStack ltWaiting)

-- | Set the 'LockTrace' to indicate that the current thread has taken the
-- lock. Errors if the lock is already taken by another thread or if the current
-- thread is not waiting on the lock.
setTaken :: HasCallStack => LockTrace -> RewriteMonad NormalizeState LockTrace
setTaken LockTrace{ltTaken, ltWaiting} = do
  threadId <- myThreadId
  case (ltTaken, Map.lookup threadId ltWaiting) of
    (Just (tId, _), _) | tId /= threadId ->
      error "setTaken: lock is already taken by another thread"
    (Just (tId, _), _) | tId == threadId ->
      error "setTaken: current thread already holds the lock"
    _ ->
      pure $ LockTrace
        { ltTaken = Just (threadId, callStack)
        , ltWaiting = Map.delete threadId ltWaiting
        }

-- | Clear the 'LockTrace' to indicate that the current thread has released
-- the lock.
clear :: HasCallStack => LockTrace -> RewriteMonad NormalizeState LockTrace
clear LockTrace{ltWaiting} = do
  threadId <- myThreadId
  pure $ LockTrace
    { ltTaken = Nothing
    , ltWaiting = Map.delete threadId ltWaiting
    }

-- | Wrap an action with debugging behavior depending on whether
-- @concurrentNormalizationDebug@ is enabled. A timeout, given through
-- @concurrentNormalizationTimeout@, is always set, but depending on the former
-- option debug information is included in the timeout exception.
wrap ::
  HasCallStack =>
  RewriteMonad NormalizeState b ->
  RewriteMonad NormalizeState b ->
  RewriteMonad NormalizeState b
wrap action debugAction = do
  timeout <- Lens.view (Rewrite.concurrentNormalizationTimeout)
  concurrentNormalizationDebug <- Lens.view (Rewrite.concurrentNormalizationDebug)
  if concurrentNormalizationDebug
    then debugAction
    else action

-- | Like 'takeMVar', but its usage is logged in the 'RewriteMonad' if
-- @concurrentNormalizationDebug@ is enabled.
takeMVarM :: HasCallStack => MVar a -> RewriteMonad NormalizeState a
takeMVarM mvar = wrap (takeMVar mvar) $
  withLockNameOrError mvar $ \lockName -> do
    modifyDebugLockTraceM_ lockName setWaiting
    withMVar mvar $ \val -> do
      modifyDebugLockTraceM_ lockName setTaken
      pure val

-- | Like 'putMVar', but its usage is logged in the 'RewriteMonad' if
-- @concurrentNormalizationDebug@ is enabled. This will throw an error if the
-- current thread does not hold the lock -- around clash-lib we never intent to
-- have this behavior. (Note that is why you should use 'withMVarM' instead).
putMVarM :: HasCallStack => MVar a -> a -> RewriteMonad NormalizeState ()
putMVarM mvar val = wrap (putMVar mvar val) $
  withLockNameOrError mvar $ \lockName -> do
    -- Ensure that the current thread holds the lock according to our own trace
    threadId <- liftIO myThreadId
    modifyDebugLockTraceM_ lockName $ \lockTrace@LockTrace{ltTaken} ->
      case ltTaken of
        Just (tId, _) | tId == threadId -> pure lockTrace
        _ -> error "putMVarM: current thread does not hold the lock"

    success <- liftIO $ tryPutMVar mvar val
    if success
      then modifyDebugLockTraceM_ lockName clear
      else error "putMVarM: MVar was already full"

-- | Like 'readMVar', but its usage is logged in the 'RewriteMonad' if
-- @concurrentNormalizationDebug@ is enabled.
readMVarM :: HasCallStack => MVar a -> RewriteMonad NormalizeState a
readMVarM mvar = wrap (readMVar mvar) $
  withLockNameOrError mvar $ \lockName -> do
    modifyDebugLockTraceM_ lockName setWaiting
    withMVar mvar $ \val -> do
      modifyDebugLockTraceM_ lockName clear
      pure val

-- | Like 'modifyMVar', but its usage is logged in the 'RewriteMonad' if
-- @concurrentNormalizationDebug@ is enabled.
withMVarM ::
  HasCallStack =>
  MVar a ->
  (a -> RewriteMonad NormalizeState b) ->
  RewriteMonad NormalizeState b
withMVarM mvar f = wrap (withMVar mvar f) $
  modifyMVarM mvar $ \a -> do
    b <- f a
    pure (a, b)

-- | Like 'modifyMVar', but its usage is logged in the 'RewriteMonad' if
-- @concurrentNormalizationDebug@ is enabled.
modifyMVarM ::
  HasCallStack =>
  MVar a ->
  (a -> RewriteMonad NormalizeState (a, b)) ->
  RewriteMonad NormalizeState b
modifyMVarM mvar f = wrap (modifyMVar mvar f) $
  withLockNameOrError mvar $ \lockName -> do
    modifyDebugLockTraceM_ lockName setWaiting
    a <- takeMVarM mvar
    modifyDebugLockTraceM_ lockName setTaken
    (newA, b) <- f a `onException` cleanup lockName a
    cleanup lockName newA
    pure b
  where
    cleanup lockName a = do
      modifyDebugLockTraceM_ lockName $ \lockTrace -> do
        success <- tryPutMVar mvar a
        if success
          then clear lockTrace
          else error "modifyMVarM: MVar was already full"
