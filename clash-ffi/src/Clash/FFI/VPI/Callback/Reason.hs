{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Callback.Reason
  ( CallbackReason(..)
  , UnknownCallbackReason(..)
  ) where

import           Control.Exception (Exception, throwIO)
import           Data.Coerce
import           Foreign.C.Types (CInt)
import qualified Foreign.Marshal.Alloc as FFI (alloca, malloc)
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import qualified Foreign.Storable as FFI (peekByteOff, pokeByteOff)
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import           Clash.FFI.View
import           Clash.FFI.VPI.Object

{-
NOTE [object in `CallbackReason`]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a callback reason is related to some object, we use an existential type
to allow any valid object type to be used. However, this can only be used when
creating a reason to send, when receiving we do not know the type of the
object the callback acts on. This means any received callback will simply use
Object and needs to be coerced into the correct type. This coercion is
obviously unchecked / unsafe, so must be performed carefully.
-}

-- | A callback reason is used when defining a callback to say when it should
-- fire, and included in the input to a callback routine when triggered so the
-- callback can identify why it was fired.
--
-- Callback reasons are divided into three groups in the VPI specification:
--
--   [Simulation event callbacks]: These are callbacks which trigger before or
--   after some simulation event is performed. Events are things like values
--   being changed, or evaluation of a particular HDL statement. Event callbacks
--   typically have an associated VPI object (although in some cases this may
--   be optional if the callback can be triggered on all relevant objects such
--   as @AfterForce@ and @AfterRelease@). Event callbacks can also return time
--   values when they are triggered, and the format of that value is specified
--   here.
--
--   [Simulation time callbacks]: These are callbacks which trigger at a certain
--   point in time, which may be absolute, relative to the current simulation
--   time, or related to the Verilog scheduler (i.e. @NextSimTime@, which
--   triggers at the start of a time step). Time callbacks may in some cases
--   refer to an object, as objects carry their own internal time value.
--
--   [Simulator action / feature callbacks]: These are callbacks which are
--   triggered by the simulation tool when it performs some operation. They do
--   not have any associated handle or time value, as they relate to the
--   simulator tool instead of the simulation running in the tool. The
--   specification differentiates actions and features. An action is required
--   for a compliant VPI implementation but a feature is optional and may not
--   be implemented in all tools.
--
{- FOURMOLU_DISABLE -}
data CallbackReason
  = forall a. Coercible a Object => AfterValueChange a TimeType ValueFormat
  -- ^ Triggered after the value of the object @a@ changes.
  | forall a. Coercible a Object => BeforeStatement a TimeType
  -- ^ Triggered before the statement @a@ is about to be executed.
  --
  -- *NOTE:* @a@ must have an object type which is a behavioural statement.
  | forall a. Coercible a Object => AfterForce (Maybe a) TimeType ValueFormat
  -- ^ Triggered after a force event (potentially on a specific object)
  | forall a. Coercible a Object => AfterRelease (Maybe a) TimeType ValueFormat
  -- ^ Triggered after a release event (potentially on a specific object)
  | forall a. Coercible a Object => AtStartOfSimTime (Maybe a) Time
  -- ^ Triggered before events in a time queue are executed. If no object is
  -- given, the global time queue is used.
  --
  -- *NOTE:* @a@ must have an object type which is a time queue.
  | forall a. Coercible a Object => ReadWriteSynch (Maybe a) Time
  -- ^ Triggered before or after non-blocking events are executed for the
  -- specified time (optionally on the object @a@). The callback is allowed to
  -- write values and schedule events.
  | forall a. Coercible a Object => ReadOnlySynch (Maybe a) Time
  -- ^ Triggered before or after non-blocking events are executed for the
  -- specified time (optionally on the object @a@). The callback is not allowed
  -- to write values or schedule events.
  | forall a. Coercible a Object => NextSimTime (Maybe a) TimeType
  -- ^ Triggered before events in the next event queue are executed.
  | forall a. Coercible a Object => AfterDelay (Maybe a) Time
  -- ^ Triggered after a given amount of time, before execution of events in
  -- the specified time queue. The callback can be set for any time, even if no
  -- event is present.
  | EndOfCompile
  -- ^ Triggered when the simulator has finished compiling source files.
  | StartOfSimulation
  -- ^ Triggered when the simulator starts the cycle at time 0.
  | EndOfSimulation
  -- ^ Triggered when there are no more events in the simulator event queue, or
  -- the @$finish@ PLI task is called.
  | RuntimeError
  -- ^ Triggered when an error occurs during simulation.
  | TchkViolation
  -- ^ Triggers when a timing-check error occurs.
  | StartOfSave
  -- ^ Triggered before the simulator creates a save state.
  | EndOfSave
  -- ^ Triggered after the simulator creates a save state.
  | StartOfRestart
  -- ^ Triggered before the simulator restarts simulation from a save state.
  | EndOfRestart
  -- ^ Triggered after the simulator restarts simulation from a save state.
  | StartOfReset
  -- ^ Triggered before the simulator is reset to an initial state.
  | EndOfReset
  -- ^ Triggered after the simulator is reset to an initial state.
  | EnterInteractive
  -- ^ Triggered when the simulator enters interactive mode.
  | ExitInteractive
  -- ^ Triggered when the simulator exits interactive mode.
  | InteractiveScopeChange
  -- ^ Triggered when the scope is changed while in interactive mode.
  | UnresolvedSysTf
  -- ^ Triggered when an unknown system task / function is called.
#if defined(VERILOG_2001)
  | forall a. Coercible a Object => AfterAssign a TimeType ValueFormat
  -- ^ Triggered after a procedural assign statement is executed.
  | forall a. Coercible a Object => AfterDeassign a TimeType ValueFormat
  -- ^ Triggered after a procedural deassign statement is executed.
  | forall a. Coercible a Object => AfterDisable a TimeType ValueFormat
  -- ^ Triggered after a named block or task containing a system task or
  -- function has been disabled.
  | PliError
  -- ^ Triggered when an error occurs during a PLI/VPI call.
  | Signal
  -- ^ Triggered when a signal (i.e. SIGINT) occurs during simulation.
#endif
#if defined(VERILOG_2005)
  | forall a. Coercible a Object => NbaSynch (Maybe a) Time
  -- ^ Triggered immediately before non-blocking assignment events are processed
  -- in a time step.
  | forall a. Coercible a Object => AtEndOfSimTime (Maybe a) Time
  -- ^ Triggered after non-blocking events in a time step are executed, but
  -- before read-only events are processed.
#endif
{- FOURMOLU_ENABLE -}

type instance CRepr CallbackReason = (CInt, Object, Ptr CTime, Ptr CValue)

instance UnsafeSend CallbackReason where
  unsafeSend cbr f = case cbr of
    AfterValueChange object timeTy valueFmt -> do
      ctimeTy <- send timeTy
      FFI.alloca $ \ctime -> do
        FFI.pokeByteOff ctime 0 ctimeTy
        cfmt <- send valueFmt
        FFI.alloca $ \cvalue -> do
          FFI.pokeByteOff cvalue 0 cfmt
          f (1, coerce object, ctime, cvalue)

    BeforeStatement object timeTy -> do
      ctimeTy <- send timeTy
      FFI.alloca $ \ctime -> do
        FFI.pokeByteOff ctime 0 ctimeTy
        f (2, coerce object, ctime, FFI.nullPtr)

    AfterForce mObject timeTy valueFmt -> do
      let object = maybe nullObject coerce mObject
      ctimeTy <- send timeTy
      FFI.alloca $ \ctime -> do
        FFI.pokeByteOff ctime 0 ctimeTy
        cfmt <- send valueFmt
        FFI.alloca $ \cvalue -> do
          FFI.pokeByteOff cvalue 0 cfmt
          f (3, object, ctime, cvalue)

    AfterRelease mObject timeTy valueFmt -> do
      let object = maybe nullObject coerce mObject
      ctimeTy <- send timeTy
      FFI.alloca $ \ctime -> do
        FFI.pokeByteOff ctime 0 ctimeTy
        cfmt <- send valueFmt
        FFI.alloca $ \cvalue -> do
          FFI.pokeByteOff cvalue 0 cfmt
          f (4, object, ctime, cvalue)

    AtStartOfSimTime mObject time -> do
      let object = maybe nullObject coerce mObject
      ctime <- pokeSend time
      f (5, object, ctime, FFI.nullPtr)

    ReadWriteSynch mObject time -> do
      let object = maybe nullObject coerce mObject
      ctime <- pokeSend time
      f (6, object, ctime, FFI.nullPtr)

    ReadOnlySynch mObject time -> do
      let object = maybe nullObject coerce mObject
      ctime <- pokeSend time
      f (7, object, ctime, FFI.nullPtr)

    NextSimTime mObject timeTy -> do
      let object = maybe nullObject coerce mObject
      ctimeTy <- send timeTy
      FFI.alloca $ \ctime -> do
        FFI.pokeByteOff ctime 0 ctimeTy
        f (8, object, ctime, FFI.nullPtr)

    AfterDelay mObject time -> do
      let object = maybe nullObject coerce mObject
      ctime <- pokeSend time
      f (9, object, ctime, FFI.nullPtr)

    EndOfCompile ->
      f (10, nullObject, FFI.nullPtr, FFI.nullPtr)

    StartOfSimulation ->
      f (11, nullObject, FFI.nullPtr, FFI.nullPtr)

    EndOfSimulation ->
      f (12, nullObject, FFI.nullPtr, FFI.nullPtr)

    RuntimeError ->
      f (13, nullObject, FFI.nullPtr, FFI.nullPtr)

    TchkViolation ->
      f (14, nullObject, FFI.nullPtr, FFI.nullPtr)

    StartOfSave ->
      f (15, nullObject, FFI.nullPtr, FFI.nullPtr)

    EndOfSave ->
      f (16, nullObject, FFI.nullPtr, FFI.nullPtr)

    StartOfRestart ->
      f (17, nullObject, FFI.nullPtr, FFI.nullPtr)

    EndOfRestart ->
      f (18, nullObject, FFI.nullPtr, FFI.nullPtr)

    StartOfReset ->
      f (19, nullObject, FFI.nullPtr, FFI.nullPtr)

    EndOfReset ->
      f (20, nullObject, FFI.nullPtr, FFI.nullPtr)

    EnterInteractive ->
      f (21, nullObject, FFI.nullPtr, FFI.nullPtr)

    ExitInteractive ->
      f (22, nullObject, FFI.nullPtr, FFI.nullPtr)

    InteractiveScopeChange ->
      f (23, nullObject, FFI.nullPtr, FFI.nullPtr)

    UnresolvedSysTf ->
      f (24, nullObject, FFI.nullPtr, FFI.nullPtr)

#if defined(VERILOG_2001)
    AfterAssign object timeTy valueFmt -> do
      ctimeTy <- send timeTy
      FFI.alloca $ \ctime -> do
        FFI.pokeByteOff ctime 0 ctimeTy
        cfmt <- send valueFmt
        FFI.alloca $ \cvalue -> do
          FFI.pokeByteOff cvalue 0 cfmt
          f (25, coerce object, ctime, cvalue)

    AfterDeassign object timeTy valueFmt -> do
      ctimeTy <- send timeTy
      FFI.alloca $ \ctime -> do
        FFI.pokeByteOff ctime 0 ctimeTy
        cfmt <- send valueFmt
        FFI.alloca $ \cvalue -> do
          FFI.pokeByteOff cvalue 0 cfmt
          f (26, coerce object, ctime, cvalue)

    AfterDisable object timeTy valueFmt -> do
      ctimeTy <- send timeTy
      FFI.alloca $ \ctime -> do
        FFI.pokeByteOff ctime 0 ctimeTy
        cfmt <- send valueFmt
        FFI.alloca $ \cvalue -> do
          FFI.pokeByteOff cvalue 0 cfmt
          f (27, coerce object, ctime, cvalue)

    PliError ->
      f (28, nullObject, FFI.nullPtr, FFI.nullPtr)

    Signal ->
      f (29, nullObject, FFI.nullPtr, FFI.nullPtr)
#endif
#if defined(VERILOG_2005)
    NbaSynch mObject time -> do
      let object = maybe nullObject coerce mObject
      ctime <- pokeSend time
      f (30, object, ctime, FFI.nullPtr)

    AtEndOfSimTime mObject time -> do
      let object = maybe nullObject coerce mObject
      ctime <- pokeSend time
      f (31, object, ctime, FFI.nullPtr)
#endif

instance Send CallbackReason where
  send = \case
    AfterValueChange object timeTy valueFmt -> do
      ctimeTy <- send timeTy
      ctime <- FFI.malloc
      FFI.pokeByteOff ctime 0 ctimeTy

      cfmt <- send valueFmt
      cvalue <- FFI.malloc
      FFI.pokeByteOff cvalue 0 cfmt

      pure (1, coerce object, ctime, cvalue)

    BeforeStatement object timeTy -> do
      ctimeTy <- send timeTy
      ctime <- FFI.malloc
      FFI.pokeByteOff ctime 0 ctimeTy

      pure (2, coerce object, ctime, FFI.nullPtr)

    AfterForce mObject timeTy valueFmt -> do
      let object = maybe nullObject coerce mObject

      ctimeTy <- send timeTy
      ctime <- FFI.malloc
      FFI.pokeByteOff ctime 0 ctimeTy

      cfmt <- send valueFmt
      cvalue <- FFI.malloc
      FFI.pokeByteOff cvalue 0 cfmt

      pure (3, object, ctime, cvalue)

    AfterRelease mObject timeTy valueFmt -> do
      let object = maybe nullObject coerce mObject

      ctimeTy <- send timeTy
      ctime <- FFI.malloc
      FFI.pokeByteOff ctime 0 ctimeTy

      cfmt <- send valueFmt
      cvalue <- FFI.malloc
      FFI.pokeByteOff cvalue 0 cfmt

      pure (4, object, ctime, cvalue)

    AtStartOfSimTime mObject time -> do
      let object = maybe nullObject coerce mObject
      ctime <- pokeSend time
      pure (5, object, ctime, FFI.nullPtr)

    ReadWriteSynch mObject time -> do
      let object = maybe nullObject coerce mObject
      ctime <- pokeSend time
      pure (6, object, ctime, FFI.nullPtr)

    ReadOnlySynch mObject time -> do
      let object = maybe nullObject coerce mObject
      ctime <- pokeSend time
      pure (7, object, ctime, FFI.nullPtr)

    NextSimTime mObject timeTy -> do
      let object = maybe nullObject coerce mObject

      ctimeTy <- send timeTy
      ctime <- FFI.malloc
      FFI.pokeByteOff ctime 0 ctimeTy

      pure (8, object, ctime, FFI.nullPtr)

    AfterDelay mObject time -> do
      let object = maybe nullObject coerce mObject
      ctime <- pokeSend time
      pure (9, object, ctime, FFI.nullPtr)

    EndOfCompile ->
      pure (10, nullObject, FFI.nullPtr, FFI.nullPtr)

    StartOfSimulation ->
      pure (11, nullObject, FFI.nullPtr, FFI.nullPtr)

    EndOfSimulation ->
      pure (12, nullObject, FFI.nullPtr, FFI.nullPtr)

    RuntimeError ->
      pure (13, nullObject, FFI.nullPtr, FFI.nullPtr)

    TchkViolation ->
      pure (14, nullObject, FFI.nullPtr, FFI.nullPtr)

    StartOfSave ->
      pure (15, nullObject, FFI.nullPtr, FFI.nullPtr)

    EndOfSave ->
      pure (16, nullObject, FFI.nullPtr, FFI.nullPtr)

    StartOfRestart ->
      pure (17, nullObject, FFI.nullPtr, FFI.nullPtr)

    EndOfRestart ->
      pure (18, nullObject, FFI.nullPtr, FFI.nullPtr)

    StartOfReset ->
      pure (19, nullObject, FFI.nullPtr, FFI.nullPtr)

    EndOfReset ->
      pure (20, nullObject, FFI.nullPtr, FFI.nullPtr)

    EnterInteractive ->
      pure (21, nullObject, FFI.nullPtr, FFI.nullPtr)

    ExitInteractive ->
      pure (22, nullObject, FFI.nullPtr, FFI.nullPtr)

    InteractiveScopeChange ->
      pure (23, nullObject, FFI.nullPtr, FFI.nullPtr)

    UnresolvedSysTf ->
      pure (24, nullObject, FFI.nullPtr, FFI.nullPtr)

#if defined(VERILOG_2001)
    AfterAssign object timeTy valueFmt -> do
      ctimeTy <- send timeTy
      ctime <- FFI.malloc
      FFI.pokeByteOff ctime 0 ctimeTy

      cfmt <- send valueFmt
      cvalue <- FFI.malloc
      FFI.pokeByteOff cvalue 0 cfmt

      pure (25, coerce object, ctime, cvalue)

    AfterDeassign object timeTy valueFmt -> do
      ctimeTy <- send timeTy
      ctime <- FFI.malloc
      FFI.pokeByteOff ctime 0 ctimeTy

      cfmt <- send valueFmt
      cvalue <- FFI.malloc
      FFI.pokeByteOff cvalue 0 cfmt

      pure (26, coerce object, ctime, cvalue)

    AfterDisable object timeTy valueFmt -> do
      ctimeTy <- send timeTy
      ctime <- FFI.malloc
      FFI.pokeByteOff ctime 0 ctimeTy

      cfmt <- send valueFmt
      cvalue <- FFI.malloc
      FFI.pokeByteOff cvalue 0 cfmt

      pure (27, coerce object, ctime, cvalue)

    PliError ->
      pure (28, nullObject, FFI.nullPtr, FFI.nullPtr)

    Signal ->
      pure (29, nullObject, FFI.nullPtr, FFI.nullPtr)
#endif
#if defined(VERILOG_2005)
    NbaSynch mObject time -> do
      let object = maybe nullObject coerce mObject
      ctime <- pokeSend time
      pure (31, object, ctime, FFI.nullPtr)

    AtEndOfSimTime mObject time -> do
      let object = maybe nullObject coerce mObject
      ctime <- pokeSend time
      pure (31, object, ctime, FFI.nullPtr)
#endif

-- | An exception thrown when decoding a callback reason if an invalid value is
-- given for the C enum that specifies the constructor of 'CallbackReason'.
--
data UnknownCallbackReason
  = UnknownCallbackReason CInt CallStack
  deriving anyclass (Exception)

instance Show UnknownCallbackReason where
  show = \case
    UnknownCallbackReason x c -> mconcat
      [ "Unknown callback reason: "
      , show x
      , "\n"
      , prettyCallStack c
      ]

{- FOURMOLU_DISABLE -}
instance UnsafeReceive CallbackReason where
  unsafeReceive (creason, object, ctime, cvalue) =
    let mObject = if isNullObject object then Nothing else Just object in
    case creason of
      1 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        valueFmt <- FFI.peekByteOff cvalue 0 >>= receive
        pure $ AfterValueChange object timeTy valueFmt

      2 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        pure $ BeforeStatement object timeTy

      3 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        valueFmt <- FFI.peekByteOff cvalue 0 >>= receive
        pure $ AfterForce mObject timeTy valueFmt

      4 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        valueFmt <- FFI.peekByteOff cvalue 0 >>= receive
        pure $ AfterRelease mObject timeTy valueFmt

      5 -> do
        time <- peekReceive ctime
        pure $ AtStartOfSimTime mObject time

      6 -> do
        time <- peekReceive ctime
        pure $ ReadWriteSynch mObject time

      7 -> do
        time <- peekReceive ctime
        pure $ ReadOnlySynch mObject time

      8 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        pure $ NextSimTime mObject timeTy

      9 -> do
        time <- peekReceive ctime
        pure $ AfterDelay mObject time

      10 ->
        pure EndOfCompile

      11 ->
        pure StartOfSimulation

      12 ->
        pure EndOfSimulation

      13 ->
        pure RuntimeError

      14 ->
        pure TchkViolation

      15 ->
        pure StartOfSave

      16 ->
        pure EndOfSave

      17 ->
        pure StartOfRestart

      18 ->
        pure EndOfRestart

      19 ->
        pure StartOfReset

      20 ->
        pure EndOfReset

      21 ->
        pure EnterInteractive

      22 ->
        pure ExitInteractive

      23 ->
        pure InteractiveScopeChange

      24 ->
        pure UnresolvedSysTf

#if defined(VERILOG_2001)
      25 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        valueFmt <- FFI.peekByteOff cvalue 0 >>= receive
        pure $ AfterAssign object timeTy valueFmt

      26 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        valueFmt <- FFI.peekByteOff cvalue 0 >>= receive
        pure $ AfterDeassign object timeTy valueFmt

      27 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        valueFmt <- FFI.peekByteOff cvalue 0 >>= receive
        pure $ AfterDisable object timeTy valueFmt

      28 ->
        pure PliError

      29 ->
        pure Signal
#endif
#if defined(VERILOG_2005)
      30 -> do
        time <- peekReceive ctime
        pure $ NbaSynch mObject time

      31 -> do
        time <- peekReceive ctime
        pure $ AtEndOfSimTime mObject time
#endif

      n  -> throwIO $ UnknownCallbackReason n callStack

instance Receive CallbackReason where
  receive (creason, object, ctime, cvalue) =
    let mObject = if isNullObject object then Nothing else Just object in
    case creason of
      1 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        valueFmt <- FFI.peekByteOff cvalue 0 >>= receive
        pure $ AfterValueChange object timeTy valueFmt

      2 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        pure $ BeforeStatement object timeTy

      3 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        valueFmt <- FFI.peekByteOff cvalue 0 >>= receive
        pure $ AfterForce mObject timeTy valueFmt

      4 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        valueFmt <- FFI.peekByteOff cvalue 0 >>= receive
        pure $ AfterRelease mObject timeTy valueFmt

      5 -> do
        time <- peekReceive ctime
        pure $ AtStartOfSimTime mObject time

      6 -> do
        time <- peekReceive ctime
        pure $ ReadWriteSynch mObject time

      7 -> do
        time <- peekReceive ctime
        pure $ ReadOnlySynch mObject time

      8 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        pure $ NextSimTime mObject timeTy

      9 -> do
        time <- peekReceive ctime
        pure $ AfterDelay mObject time

      10 ->
        pure EndOfCompile

      11 ->
        pure StartOfSimulation

      12 ->
        pure EndOfSimulation

      13 ->
        pure RuntimeError

      14 ->
        pure TchkViolation

      15 ->
        pure StartOfSave

      16 ->
        pure EndOfSave

      17 ->
        pure StartOfRestart

      18 ->
        pure EndOfRestart

      19 ->
        pure StartOfReset

      20 ->
        pure EndOfReset

      21 ->
        pure EnterInteractive

      22 ->
        pure ExitInteractive

      23 ->
        pure InteractiveScopeChange

      24 ->
        pure UnresolvedSysTf

#if defined(VERILOG_2001)
      25 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        valueFmt <- FFI.peekByteOff cvalue 0 >>= receive
        pure $ AfterAssign object timeTy valueFmt

      26 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        valueFmt <- FFI.peekByteOff cvalue 0 >>= receive
        pure $ AfterDeassign object timeTy valueFmt

      27 -> do
        timeTy <- FFI.peekByteOff ctime 0 >>= receive
        valueFmt <- FFI.peekByteOff cvalue 0 >>= receive
        pure $ AfterDisable object timeTy valueFmt

      28 ->
        pure PliError

      29 ->
        pure Signal
#endif
#if defined(VERILOG_2005)
      30 -> do
        time <- peekReceive ctime
        pure $ NbaSynch mObject time

      31 -> do
        time <- peekReceive ctime
        pure $ AtEndOfSimTime mObject time
#endif

      n  -> throwIO $ UnknownCallbackReason n callStack
{- FOURMOLU_ENABLE -}
