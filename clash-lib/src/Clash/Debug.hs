{-# LANGUAGE CPP #-}

module Clash.Debug
  ( debugIsOn
  , traceIf
  , traceWith
  , traceShowWith
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
{-# INLINE traceIf #-}

traceWith :: (a -> String) -> a -> a
traceWith f a = trace (f a) a

traceShowWith :: Show b => (a -> b) -> a -> a
traceShowWith f a = trace (show (f a)) a

