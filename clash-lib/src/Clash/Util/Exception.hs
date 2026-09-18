{-# LANGUAGE CPP #-}

module Clash.Util.Exception
  ( mapConcurrentlyWithBacktrace_
  , exceptionBacktrace
  )
where

import qualified Control.Exception as Exception
#if MIN_VERSION_base(4,20,0)
import Control.Concurrent.Async (ConcurrentlyE(..))
import Data.Foldable (traverse_)
#else
import Control.Concurrent.Async (mapConcurrently_)
#endif
#if MIN_VERSION_base(4,21,0)
import Control.Exception.Context (displayExceptionContext)
#endif

-- | Like @mapConcurrently_@, preserving each exception's original type and
-- context. ConcurrentlyE carries failures as values and cancels siblings.
mapConcurrentlyWithBacktrace_ :: Foldable f => (a -> IO b) -> f a -> IO ()
#if MIN_VERSION_base(4,20,0)
mapConcurrentlyWithBacktrace_ f xs = do
  result <- runConcurrentlyE $ traverse_ (ConcurrentlyE . Exception.try . f) xs
  case result of
#if MIN_VERSION_base(4,21,0)
    Left original@(Exception.SomeException e) ->
      Exception.rethrowIO
        (Exception.ExceptionWithContext (Exception.someExceptionContext original) e)
#else
    Left original -> Exception.throwIO (RethrowException original)
#endif
    Right () -> pure ()
#else
-- Before base-4.20, ErrorCall stores the call stack in the exception value,
-- which survives async's rethrows without special handling.
mapConcurrentlyWithBacktrace_ = mapConcurrently_
#endif

#if MIN_VERSION_base(4,20,0) && !MIN_VERSION_base(4,21,0)
-- GHC 9.10's equivalent of rethrowIO: retain the saved type and context
-- without collecting another backtrace at the rethrow site.
newtype RethrowException = RethrowException Exception.SomeException

instance Show RethrowException where
  show (RethrowException e) = show e

instance Exception.Exception RethrowException where
  toException (RethrowException e) = e
  backtraceDesired _ = False
#endif

-- | Exception context to append to 'Exception.displayException'. Empty on
-- older versions of base, where displayException already includes it (GHC
-- 9.10), or ErrorCall stores its call stack in the exception value.
exceptionBacktrace :: Exception.SomeException -> String
#if MIN_VERSION_base(4,21,0)
exceptionBacktrace = displayExceptionContext . Exception.someExceptionContext
#else
exceptionBacktrace _ = ""
#endif
