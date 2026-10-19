{-|





This module is intended to be imported qualified.

-}

{-# LANGUAGE DuplicateRecordFields #-}

module Clash.Signal.Trace (

  -- * Simulation and tracing
  Traceable,
  Simulation,
  Config(..),
  simulate, simulateWith,
  emptySim,
  setStartStop,setClockStart,
  fetch,

  -- ** Clock signals
  ClockWave,
  clockWave,

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
  Period, Width, Value,
  TraceMap,

  -- * VCD File generation
  vcdText,
  writeVcd,

  -- * Storing signals
  -- ** Storing
  store,
  stores,
  storeSignal,
  storeSignals,
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
  FileHeader(..),
  SignalDescr(..),
  DataType(..),
  loadHeader,

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

import Clash.Signal.Internal.Trace
import Clash.Signal.Internal.Trace.Vcd
import Clash.Signal.Internal.Trace.Store
import Clash.Signal.Internal.Trace.DataType
import qualified Clash.Signal.Internal.Trace.OldTrace as Old
