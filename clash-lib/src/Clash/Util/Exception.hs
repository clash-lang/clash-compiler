{-# LANGUAGE CPP #-}

module Clash.Util.Exception
  ( exceptionBacktrace
  )
where

import qualified Control.Exception as Exception
#if MIN_VERSION_base(4,21,0)
import Control.Exception.Context (displayExceptionContext)
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
