{-|
  Copyright  :  (C) 2018, Google Inc.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

  Utilities for tracing signals and dumping them in various ways. See
  @Clash.Signal.Trace@ for documentation. This module merely houses
  explicitly clocked versions of everything defined there.
-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

module Clash.Explicit.Signal.Trace
  (
  -- * Tracing functions
    traceSignal
  , traceSignalN
  , traceVecSignal
  , traceVecSignalN
  , unsafeTraceSignal
  , unsafeTraceSignalN
  , unsafeTraceVecSignal
  , unsafeTraceVecSignalN
  , waitForTrace
  , waitForTraces
  , I.getTraces

  -- * Re-export VCD dump functions
  , I.VCDError(..)
  , I.fromVCDError
  , I.dumpVCD
  , I.dumpVCD'
  , I.dumpVCD''
  ) where

-- Implicitly clocked version of this module:
import qualified Clash.Signal.Trace           as I

-- Clash:
import           Clash.Signal                 (exposeClockReset)
import           Clash.Signal.Internal        (Clock, Reset, Signal)
import           Clash.Sized.Vector           (Vec)
import           Clash.Class.BitPack          (BitPack, BitSize)
import           Clash.Promoted.Nat           (SNat(..))

-- Haskell / GHC:
import           Control.DeepSeq              (NFData)
import           GHC.Stack                    (HasCallStack)
import           GHC.TypeLits                 (KnownNat, type (+))

-- | Trace a single signal. Will emit error if a signal with the same name was
-- previously registered.
traceSignal
  :: HasCallStack
  => KnownNat (BitSize a)
  => BitPack a
  => NFData a
  => Clock domain gated
  -> Reset domain synchronous
  -> String
  -- ^ Name of signal in debugging output
  -> Signal domain a
  -- ^ Signal to trace
  -> IO (Signal domain a)
traceSignal = exposeClockReset I.traceSignal

-- | Trace a single vector signal: each element in the vector will show up as
-- a different trace. If the trace name already exists, this function will emit
-- an error.
traceVecSignal
  :: HasCallStack
  => KnownNat (BitSize a)
  => KnownNat n
  => BitPack a
  => NFData a
  => Clock domain gated
  -> Reset domain synchronous
  -> String
  -- ^ Name of signal in debugging output. Will be appended by _0, _1, ..., _n.
  -> Signal domain (Vec (n+1) a)
  -- ^ Signal to trace
  -> IO (Signal domain (Vec (n+1) a))
traceVecSignal = exposeClockReset I.traceVecSignal

-- | Same as @traceSignal@, but append /_num/ to name.
traceSignalN
  :: HasCallStack
  => KnownNat (BitSize a)
  => BitPack a
  => NFData a
  => Clock domain gated
  -> Reset domain synchronous
  -> String
  -- ^ Name of signal in debugging output
  -> SNat n
  -- ^ Number appended to name
  -> Signal domain a
  -- ^ Signal to trace
  -> IO (Signal domain a)
traceSignalN = exposeClockReset I.traceSignalN


-- | Same as @traceVecSignal@, but append /_num/ to name.
traceVecSignalN
  :: HasCallStack
  => KnownNat (BitSize a)
  => KnownNat n
  => BitPack a
  => NFData a
  => Clock domain gated
  -> Reset domain synchronous
  -> String
  -- ^ Name of signal in debugging output. Will be appended by _0, _1, ..., _n.
  -> SNat n
  -- ^ Number appended to name
  -> Signal domain (Vec (n+1) a)
  -- ^ Signal to trace
  -> IO (Signal domain (Vec (n+1) a))
traceVecSignalN = exposeClockReset I.traceVecSignalN

------------------------
-------- UNSAFE --------
------------------------
-- | Same as @traceSignal@, but without IO requirement. To use this safely, make
-- sure at least one value from this signal is evaluated before using dump
-- functions. Also see @waitForTraces@.
unsafeTraceSignal
  :: HasCallStack
  => KnownNat (BitSize a)
  => BitPack a
  => NFData a
  => Clock domain gated
  -> Reset domain synchronous
  -> String
  -- ^ Name of signal in debugging output
  -> Signal domain a
  -- ^ Signal to trace
  -> Signal domain a
unsafeTraceSignal = exposeClockReset I.unsafeTraceSignal

-- | Same as @traceVecSignal@, but without IO requirement. To use this safely,
-- make sure at least one value from this signal is evaluated before using dump
-- functions. Also see @waitForTraces@.
unsafeTraceVecSignal
  :: HasCallStack
  => KnownNat (BitSize a)
  => KnownNat n
  => BitPack a
  => NFData a
  => Clock domain gated
  -> Reset domain synchronous
  -> String
  -- ^ Name of signal in debugging output. Will be appended by _0, _1, ..., _n.
  -> Signal domain (Vec (n+1) a)
  -- ^ Signal to trace
  -> Signal domain (Vec (n+1) a)
unsafeTraceVecSignal = exposeClockReset I.unsafeTraceVecSignal

-- | Same as @traceSignalN@, but without IO requirement. To use this safely,
-- make sure at least one value from this signal is evaluated before using dump
-- functions. Also see @waitForTraces@.
unsafeTraceSignalN
  :: HasCallStack
  => KnownNat (BitSize a)
  => BitPack a
  => NFData a
  => Clock domain gated
  -> Reset domain synchronous
  -> String
  -- ^ Name of signal in debugging output
  -> SNat n
  -- ^ Number appended to name
  -> Signal domain a
  -- ^ Signal to trace
  -> Signal domain a
unsafeTraceSignalN = exposeClockReset I.unsafeTraceSignalN

-- | Same as @traceVecSignalN@, but without IO requirement. To use this safely,
-- make sure at least one value from this signal is evaluated before using dump
-- functions. Also see @waitForTraces@.
unsafeTraceVecSignalN
  :: HasCallStack
  => KnownNat (BitSize a)
  => KnownNat n
  => BitPack a
  => NFData a
  => Clock domain gated
  -> Reset domain synchronous
  -> String
  -- ^ Name of signal in debugging output. Will be appended by _0, _1, ..., _n.
  -> SNat n
  -- ^ Number appended to name
  -> Signal domain (Vec (n+1) a)
  -- ^ Signal to trace
  -> Signal domain (Vec (n+1) a)
unsafeTraceVecSignalN = exposeClockReset I.unsafeTraceVecSignalN

-- | Keep evaluating given signal until give trace name is present
waitForTrace
  :: NFData a
  => Clock domain gated
  -> Reset domain synchronous
  -> Signal dom a
  -> String
  -> IO ()
waitForTrace = exposeClockReset I.waitForTrace

-- | Keep evaluating given signal until all trace names are present.
waitForTraces
  :: NFData a
  => Clock domain gated
  -> Reset domain synchronous
  -> Signal dom a
  -> [String]
  -> IO ()
waitForTraces = exposeClockReset I.waitForTraces
