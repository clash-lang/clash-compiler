{-|
Copyright  :  (C) 2025 Martijn Bastiaan
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Traced wrappers around Control.Concurrent.MVar operations.
Each function takes an extra String argument for context and uses HasCallStack
to provide detailed tracing information about MVar operations.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Clash.Normalize.TracedMVar
  ( MVar
  , newEmptyMVar
  , newMVar
  , takeMVar
  , putMVar
  , readMVar
  , modifyMVar
  , modifyMVar_
  , withMVar
  ) where

import Control.Concurrent.MVar (MVar)
import Control.Exception.Lifted (finally)
import Control.Monad (when)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import GHC.Stack (HasCallStack, callStack, getCallStack, SrcLoc(..), withFrozenCallStack)
import System.IO (hPutStrLn, stderr)

import Clash.Core.Pretty (unsafeLookupEnvBool)

import qualified Control.Concurrent.Lifted as Concurrent
import qualified Control.Concurrent.MVar.Lifted as MVar
import qualified Clash.Util.Interpolate as I

-- | Check if MVar tracing is enabled via CLASH_DEBUG_MVAR environment variable.
-- Defaults to False if not set.
traceEnabled :: Bool
traceEnabled = unsafeLookupEnvBool "CLASH_DEBUG_MVAR" False
{-# NOINLINE traceEnabled #-}

-- | Get the immediate callsite location (filename:lineno)
callSite :: HasCallStack => String
callSite = withFrozenCallStack $
  case getCallStack callStack of
    ((_,loc):_) -> srcLocFile loc ++ ":" ++ show (srcLocStartLine loc)
    [] -> "<no-callstack>"

-- | Trace a lock event with lock name and call stack
trace :: (HasCallStack, MonadBase IO m) => String -> String -> m ()
trace lockName event = do
  when traceEnabled $ do
    threadId <- Concurrent.myThreadId
    liftBase $ hPutStrLn stderr [I.i|[TracedMVar][#{threadId}][#{lockName}] #{event} #{callSite}|]

-- | Trace when trying to acquire a lock
traceTry :: (HasCallStack, MonadBase IO m) => String -> m ()
traceTry lockName = withFrozenCallStack $ trace lockName "try"

-- | Trace when a lock has been acquired
traceAcquire :: (HasCallStack, MonadBase IO m) => String -> m ()
traceAcquire lockName = withFrozenCallStack $ trace lockName "acq"

-- | Trace when a lock has been returned
traceReturn :: (HasCallStack, MonadBase IO m) => String -> m ()
traceReturn lockName = withFrozenCallStack $ trace lockName "ret"

-- | Wrap an MVar operation with optional tracing based on traceEnabled.
-- If tracing is disabled, just runs the action directly.
-- If tracing is enabled, runs the provided tracing action.
wrap :: (HasCallStack, Monad m) => m a -> m a -> m a
wrap action tracedAction = if traceEnabled then tracedAction else action

-- | Create an empty MVar
newEmptyMVar :: (HasCallStack, MonadBaseControl IO m) => String -> m (MVar a)
newEmptyMVar lockName = withFrozenCallStack $ wrap MVar.newEmptyMVar $ do
  traceTry lockName
  mvar <- MVar.newEmptyMVar
  traceReturn lockName
  return mvar

-- | Create a new MVar with an initial value
newMVar :: (HasCallStack, MonadBaseControl IO m) => String -> a -> m (MVar a)
newMVar lockName val = withFrozenCallStack $ wrap (MVar.newMVar val) $ do
  traceTry lockName
  mvar <- MVar.newMVar val
  traceReturn lockName
  return mvar

-- | Take a value from an MVar
takeMVar :: (HasCallStack, MonadBaseControl IO m) => String -> MVar a -> m a
takeMVar lockName mvar = withFrozenCallStack $ wrap (MVar.takeMVar mvar) $ do
  traceTry lockName
  val <- MVar.takeMVar mvar
  traceAcquire lockName
  return val

-- | Put a value into an MVar
putMVar :: (HasCallStack, MonadBaseControl IO m) => String -> MVar a -> a -> m ()
putMVar lockName mvar val = withFrozenCallStack $ wrap (MVar.putMVar mvar val) $ do
  traceTry lockName
  MVar.putMVar mvar val
  traceReturn lockName

-- | Read a value from an MVar without taking it
readMVar :: (HasCallStack, MonadBaseControl IO m) => String -> MVar a -> m a
readMVar lockName mvar = withFrozenCallStack $ wrap (MVar.readMVar mvar) $ do
  traceTry lockName
  val <- MVar.readMVar mvar
  traceAcquire lockName
  traceReturn lockName
  return val

-- | Modify the contents of an MVar
modifyMVar :: (HasCallStack, MonadBaseControl IO m) => String -> MVar a -> (a -> m (a, b)) -> m b
modifyMVar lockName mvar f = withFrozenCallStack $ wrap (MVar.modifyMVar mvar f) $ do
  traceTry lockName
  result <- MVar.modifyMVar mvar $ \a -> do
    traceAcquire lockName
    f a `finally` traceReturn lockName
  return result

-- | Modify the contents of an MVar (discarding result)
modifyMVar_ :: (HasCallStack, MonadBaseControl IO m) => String -> MVar a -> (a -> m a) -> m ()
modifyMVar_ lockName mvar f = withFrozenCallStack $ wrap (MVar.modifyMVar_ mvar f) $ do
  traceTry lockName
  MVar.modifyMVar_ mvar $ \a -> do
    traceAcquire lockName
    f a `finally` traceReturn lockName

-- | Perform an action with the value from an MVar
withMVar :: (HasCallStack, MonadBaseControl IO m) => String -> MVar a -> (a -> m b) -> m b
withMVar lockName mvar f = withFrozenCallStack $ wrap (MVar.withMVar mvar f) $ do
  traceTry lockName
  result <- MVar.withMVar mvar $ \a -> do
    traceAcquire lockName
    f a `finally` traceReturn lockName
  return result
