{-|





This module is intended to be imported qualified.

-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Signal.Trace (

  -- * Simulation and tracing
  Traceable,
  Simulation,
  Trace.Times(..), Trace.times,
  Trace.Config(..),
  simulate, simulateWith,
  emptySim,
  setStartStop, setClockStart, setTimes,
  fetch,

  -- ** Clock signals
  Trace.ClockWave,
  Trace.clockWave,

  -- ** Tracing
  trace,
  traceVec,
  traceReset, traceEnable,

  -- ** Traces
  Trace,
  toTrace, fromTrace,
  addTrace,
  fetchTrace,
  clockTrace,

  -- ** Helper types
  Trace.Period, Trace.Width, Trace.Value,
  TraceMap,

  -- * VCD File generation
  vcdText,
  writeVcd,

  -- * Storing signals
  -- ** Storing
  store,
  stores,
  storeSignal,
  storeTrace,
  storeTraces,

  -- ** Loading
  load,
  loads,
  loadSignal,
  loadSignals,
  loadTrace,
  loadTraces,

  -- ** Inspecting signal dump files
  Store.FileHeader(..),
  Store.SignalDescr(..),
  DataType.DataType(..),
  Store.loadHeader,

  -- * Deprecated tracing functions
  -- ** Simple
  Old.traceSignal1, Old.traceVecSignal1,
  -- ** Tracing in a multi-clock environment
  Old.traceSignal, Old.traceVecSignal,
  -- * VCD generation
  Old.dumpVCD,
  -- * Replay functions
  Old.dumpReplayable, Old.replay,
) where


import Clash.Signal (KnownDomain, Signal, Bundle, Unbundled, Reset, Enable)
import Clash.Sized.Vector (Vec)
import Clash.Time (Time, AtOrForTime)

import Clash.Class.BitPack (BitPack)
import Clash.XException (NFDataX)
import Data.Typeable (Typeable)

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import GHC.TypeLits (KnownNat)

import qualified Clash.Signal.Internal.Trace as Trace
import qualified Clash.Signal.Internal.Trace.Store as Store
import qualified Clash.Signal.Internal.Trace.DataType as DataType
import qualified Clash.Signal.Internal.Trace.OldTrace as Old


type Traceable a = (NFDataX a, BitPack a, Typeable a)

type Simulation = Trace.Simulation ()

-- | Simulate a design by forcefully evaluating an output signal.
-- If a list of signal names is provided, the simulation will stop early
-- if all signals were found.
simulate ::
  forall dom a.
  KnownDomain dom =>
  NFDataX a =>
  -- | Duration after clock start
  Time ->
  -- | Clock waves to render
  [Trace.ClockWave] ->
  -- | Names of the traces you definitely want to be in the output
  [String] ->
  -- | (One of) the outputs of the circuit containing the traces
  Signal dom a ->
  IO (Either String Simulation)
simulate = Trace.simulate @() @dom @a

-- | Simulate a design by forcefully evaluating an output signal.
-- Like 'simulate', but with more options.
simulateWith ::
  forall dom a.
  KnownDomain dom =>
  NFDataX a =>
  -- | Trace.Config options.
  Trace.Config ->
  -- | Time window to simulate for.
  Trace.Times ->
  -- | Clock waves to render
  [Trace.ClockWave] ->
  -- | Names of the traces you definitely want to be in the output
  [String] ->
  -- | (One of) the outputs of the circuit containing the traces
  Signal dom a ->
  IO (Either String Simulation)
simulateWith = Trace.simulateWith @()

-- | Create an empty simulation from a configuration and time settings.
emptySim :: Trace.Config -> Trace.Times -> IO Simulation
emptySim = Trace.emptySim @()

-- | Change when the 'Simulation' starts and stops.
setStartStop :: Time -> AtOrForTime -> Simulation -> Simulation
setStartStop = Trace.setStartStop @()

-- | Change when the clocks start in a 'Simulation'.
setClockStart :: Time -> Simulation -> Simulation
setClockStart = Trace.setClockStart @()

-- | Change the start, stop and clock start times in a 'Simulation'.
setTimes :: Trace.Times -> Simulation -> Simulation
setTimes = Trace.setTimes @()

-- | Retrieve a 'Signal' from the captured traces.
fetch ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  String ->
  Simulation ->
  Either String (Signal dom a)
fetch = Trace.fetch @() @dom

-- | Trace a 'Signal'.
-- This converts the signal to a trace, and stores it in global storage.
trace ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  String ->
  Signal dom a ->
  Signal dom a
trace = Trace.trace @()

-- | Trace all values in a vector signal individually.
traceVec ::
  forall dom a n.
  KnownDomain dom =>
  Traceable a =>
  KnownNat n =>
  String ->
  Signal dom (Vec n a) ->
  Signal dom (Vec n a)
traceVec = Trace.traceVec @()

-- | Like 'trace', but operates on a 'Reset'
traceReset ::
  KnownDomain dom =>
  -- | Name of signal in the simulation output
  String ->
  -- | Reset to trace
  Reset dom ->
  Reset dom
traceReset = Trace.traceReset @()

-- Like traceReset for enables
traceEnable ::
  KnownDomain dom =>
  String ->
  Enable dom ->
  Enable dom
traceEnable = Trace.traceEnable @()

type Trace = Trace.Trace ()

-- | Create a 'Trace' from a 'Signal'.
toTrace ::
  KnownDomain dom =>
  Traceable a =>
  Signal dom a ->
  Trace
toTrace = Trace.toTrace @()

-- | Create a 'Signal' from a 'Trace'.
-- This checks whether the type is compatible. See also @compatible@.
fromTrace ::
  Traceable a =>
  Trace ->
  Either String (Signal dom a)
fromTrace = Trace.fromTrace @()

-- | Add a 'Trace' to a 'Simulation'.
addTrace ::
  String ->
  Trace ->
  Simulation ->
  Either String Simulation
addTrace = Trace.addTrace @()

-- | Retrieve a captured 'Trace'.
fetchTrace ::
  String ->
  Simulation ->
  Either String Trace
fetchTrace = Trace.fetchTrace @()

-- | Create a clock trace from a domain.
clockTrace ::
  forall dom.
  KnownDomain dom =>
  Trace
clockTrace = Trace.clockTrace @() @dom

type TraceMap = Trace.TraceMap ()

-- | Create a VCD file for the given traces and simulation configuration.
-- Zero-width signals are not included, and instead warnings for these signals
-- are returned.
vcdText ::
  Simulation ->
  Either String (Text, [String])
vcdText = Trace.vcdText @()

-- | Create a VCD file for the given traces and simulation configuration,
-- and write it to a file.
-- Errors if the VCD generation fails.
writeVcd ::
  HasCallStack =>
  FilePath ->
  Simulation ->
  IO ()
writeVcd = Trace.writeVcd @()

-- | Store a trace in binary form.
store ::
  -- | Name of trace to dump
  String ->
  Simulation ->
  Either String ByteString
store = Store.store @()

-- | Store multiple traces into a single binary.
-- All traces must have the same clock period.
stores ::
  -- | Names of traces to dump
  [String] ->
  Simulation ->
  Either String ByteString
stores = Store.stores @()

-- | Store a 'Signal' in binary form.
storeSignal ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  -- | Name of trace to dump
  String ->
  -- | Range of samples to dump
  (Int,Int) ->
  -- | The signal to dump
  Signal dom a ->
  ByteString
storeSignal = Store.storeSignal @()

-- | Store a single trace as a signal dump.
storeTrace ::
  String ->
  (Int,Int) ->
  Trace ->
  ByteString
storeTrace = Store.storeTrace @()

-- | Store multiple traces in a single binary file.
-- All traces must have the same clock period.
storeTraces ::
  (Int,Int) ->
  [(String, Trace)] ->
  Either String ByteString
storeTraces = Store.storeTraces @()

-- | Load a trace from binary form.
load ::
  forall dom.
  KnownDomain dom =>
  -- | The name to use for the signal
  String ->
  -- | The binary data
  ByteString ->
  -- | The 'Simulation' to add the signal to
  Simulation ->
  Either String Simulation
load = Store.load @() @dom

-- | Load multiple traces from a single binary.
-- The traces will be loaded into a single domain.
loads ::
  forall dom.
  KnownDomain dom =>
  ByteString ->
  Simulation ->
  Either String Simulation
loads = Store.loads @() @dom

-- | Load a 'Signal' from binary form.
-- See also @compatible@.
loadSignal ::
  KnownDomain dom =>
  Traceable a =>
  ByteString ->
  Either String (Signal dom a)
loadSignal = Store.loadSignal @()

-- | Load multiple 'Signal's from binary form.
-- See also @compatible@.
loadSignals ::
  forall dom a.
  KnownDomain dom =>
  Traceable a =>
  Bundle a =>
  ByteString ->
  Either String (Unbundled dom a)
loadSignals = Store.loadSignals @()

-- | Convert binary data into a trace with the given clock period.
loadTrace ::
  Trace.Period ->
  ByteString ->
  Either String Trace
loadTrace = Store.loadTrace @()

-- | Load multiple traces from a single binary.
loadTraces ::
  Trace.Period ->
  ByteString ->
  Either String [(String, Trace)]
loadTraces = Store.loadTraces @()
