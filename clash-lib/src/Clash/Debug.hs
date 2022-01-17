{-# LANGUAGE CPP #-}

module Clash.Debug
  ( debugIsOn
  , traceIf
  , traceWhen
  , module Debug.Trace
  ) where

import Debug.Trace

debugIsOn :: Bool
#if defined(DEBUG)
debugIsOn = True
#else
debugIsOn = False
#endif

-- | Performs trace when first argument evaluates to 'True'
traceIf :: Bool -> String -> a -> a
traceIf True  msg = trace msg
traceIf False _   = id

traceWhen :: Monad m => Bool -> String -> m ()
traceWhen True = traceM
traceWhen False = const (pure ())

{-# INLINE traceIf #-}
