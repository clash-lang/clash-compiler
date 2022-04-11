{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Callback.Reason
  ( CallbackReason(..)
  , UnknownCallbackReason(..)
  ) where

import           Control.Exception (Exception)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.Coerce
import           Foreign.C.Types (CInt)
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import qualified Foreign.Storable as FFI (peekByteOff, pokeByteOff)
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import qualified Clash.FFI.Monad as Sim
import           Clash.FFI.View
import           Clash.FFI.VPI.Handle
import           Clash.FFI.VPI.Object
import           Clash.FFI.VPI.Value

{-
NOTE [handle in `CallbackReason`]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a callback reason is related to some object, we use an existential type
to allow any valid handle type to be used. However, this can only be used when
creating a reason to send, when receiving we do not know the type of the
handle the callback acts on. This means any received callback will simply use
Object and needs to be coerced into the correct type. This coercion is
obviously unchecked / unsafe, so must be performed carefully.
-}

data CallbackReason
  = forall h. Coercible h Object => AfterValueChange h TimeType ValueFormat
  | forall h. Coercible h Object => BeforeStatement h TimeType
  | forall h. Coercible h Object => AfterForce (Maybe h) TimeType ValueFormat
  | forall h. Coercible h Object => AfterRelease (Maybe h) TimeType ValueFormat
  | forall h. Coercible h Object => AtStartOfSimTime (Maybe h) Time
  | forall h. Coercible h Object => ReadWriteSynch (Maybe h) Time
  | forall h. Coercible h Object => ReadOnlySynch (Maybe h) Time
  | forall h. Coercible h Object => NextSimTime (Maybe h) TimeType
  | forall h. Coercible h Object => AfterDelay (Maybe h) Time
  | EndOfCompile
  | StartOfSimulation
  | EndOfSimulation
  | RuntimeError
  | TchkViolation
  | StartOfSave
  | EndOfSave
  | StartOfRestart
  | EndOfRestart
  | StartOfReset
  | EndOfReset
  | EnterInteractive
  | ExitInteractive
  | InteractiveScopeChange
  | UnresolvedSysTf
#if defined(VERILOG_2001)
  | forall h. Coercible h Object => AfterAssign h TimeType ValueFormat
  | forall h. Coercible h Object => AfterDeassign h TimeType ValueFormat
  | forall h. Coercible h Object => AfterDisable h TimeType ValueFormat
  | PliError
  | Signal
#endif
#if defined(VERILOG_2005)
  | forall h. Coercible h Object => NbaSynch (Maybe h) Time
  | forall h. Coercible h Object => AtEndOfSimTime (Maybe h) Time
#endif

instance UnsafeSend CallbackReason where
  type Sent CallbackReason = (CInt, Object, Ptr CTime, Ptr CValue)

  unsafeSend =
    \case
      AfterValueChange handle timeTy valueFmt -> do
        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- unsafeSend valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (1, coerce handle, ctime, cvalue)

      BeforeStatement handle timeTy -> do
        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        pure (2, coerce handle, ctime, FFI.nullPtr)

      AfterForce mHandle timeTy valueFmt -> do
        let object = maybe nullHandle coerce mHandle

        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- unsafeSend valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (3, object, ctime, cvalue)

      AfterRelease mHandle timeTy valueFmt -> do
        let object = maybe nullHandle coerce mHandle

        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- unsafeSend valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (4, object, ctime, cvalue)

      AtStartOfSimTime mHandle time -> do
        let object = maybe nullHandle coerce mHandle
        ctime <- unsafePokeSend time
        pure (5, object, ctime, FFI.nullPtr)

      ReadWriteSynch mHandle time -> do
        let object = maybe nullHandle coerce mHandle
        ctime <- unsafePokeSend time
        pure (6, object, ctime, FFI.nullPtr)

      ReadOnlySynch mHandle time -> do
        let object = maybe nullHandle coerce mHandle
        ctime <- unsafePokeSend time
        pure (7, object, ctime, FFI.nullPtr)

      NextSimTime mHandle timeTy -> do
        let object = maybe nullHandle coerce mHandle

        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        pure (8, object, ctime, FFI.nullPtr)

      AfterDelay mHandle time -> do
        let object = maybe nullHandle coerce mHandle
        ctime <- unsafePokeSend time
        pure (7, object, ctime, FFI.nullPtr)

      EndOfCompile ->
        pure (10, nullHandle, FFI.nullPtr, FFI.nullPtr)

      StartOfSimulation ->
        pure (11, nullHandle, FFI.nullPtr, FFI.nullPtr)

      EndOfSimulation ->
        pure (12, nullHandle, FFI.nullPtr, FFI.nullPtr)

      RuntimeError ->
        pure (13, nullHandle, FFI.nullPtr, FFI.nullPtr)

      TchkViolation ->
        pure (14, nullHandle, FFI.nullPtr, FFI.nullPtr)

      StartOfSave ->
        pure (15, nullHandle, FFI.nullPtr, FFI.nullPtr)

      EndOfSave ->
        pure (16, nullHandle, FFI.nullPtr, FFI.nullPtr)

      StartOfRestart ->
        pure (17, nullHandle, FFI.nullPtr, FFI.nullPtr)

      EndOfRestart ->
        pure (18, nullHandle, FFI.nullPtr, FFI.nullPtr)

      StartOfReset ->
        pure (19, nullHandle, FFI.nullPtr, FFI.nullPtr)

      EndOfReset ->
        pure (20, nullHandle, FFI.nullPtr, FFI.nullPtr)

      EnterInteractive ->
        pure (21, nullHandle, FFI.nullPtr, FFI.nullPtr)

      ExitInteractive ->
        pure (22, nullHandle, FFI.nullPtr, FFI.nullPtr)

      InteractiveScopeChange ->
        pure (23, nullHandle, FFI.nullPtr, FFI.nullPtr)

      UnresolvedSysTf ->
        pure (24, nullHandle, FFI.nullPtr, FFI.nullPtr)

#if defined(VERILOG_2001)
      AfterAssign handle timeTy valueFmt -> do
        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- unsafeSend valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (25, coerce handle, ctime, cvalue)

      AfterDeassign handle timeTy valueFmt -> do
        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- unsafeSend valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (26, coerce handle, ctime, cvalue)

      AfterDisable handle timeTy valueFmt -> do
        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- unsafeSend valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (27, coerce handle, ctime, cvalue)

      PliError ->
        pure (28, nullHandle, FFI.nullPtr, FFI.nullPtr)

      Signal ->
        pure (29, nullHandle, FFI.nullPtr, FFI.nullPtr)
#endif
#if defined(VERILOG_2005)
      NbaSynch mHandle time -> do
        let object = maybe nullHandle coerce mHandle
        ctime <- unsafePokeSend time
        pure (31, object, ctime, FFI.nullPtr)

      AtEndOfSimTime mHandle time -> do
        let object = maybe nullHandle coerce mHandle
        ctime <- unsafePokeSend time
        pure (31, object, ctime, FFI.nullPtr)
#endif

instance Send CallbackReason where
  send =
    \case
      AfterValueChange handle timeTy valueFmt -> do
        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- send valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (1, coerce handle, ctime, cvalue)

      BeforeStatement handle timeTy -> do
        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        pure (2, coerce handle, ctime, FFI.nullPtr)

      AfterForce mHandle timeTy valueFmt -> do
        let object = maybe nullHandle coerce mHandle

        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- send valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (3, object, ctime, cvalue)

      AfterRelease mHandle timeTy valueFmt -> do
        let object = maybe nullHandle coerce mHandle

        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- send valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (4, object, ctime, cvalue)

      AtStartOfSimTime mHandle time -> do
        let object = maybe nullHandle coerce mHandle
        ctime <- pokeSend time
        pure (5, object, ctime, FFI.nullPtr)

      ReadWriteSynch mHandle time -> do
        let object = maybe nullHandle coerce mHandle
        ctime <- pokeSend time
        pure (6, object, ctime, FFI.nullPtr)

      ReadOnlySynch mHandle time -> do
        let object = maybe nullHandle coerce mHandle
        ctime <- pokeSend time
        pure (7, object, ctime, FFI.nullPtr)

      NextSimTime mHandle timeTy -> do
        let object = maybe nullHandle coerce mHandle

        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        pure (8, object, ctime, FFI.nullPtr)

      AfterDelay mHandle time -> do
        let object = maybe nullHandle coerce mHandle
        ctime <- pokeSend time
        pure (9, object, ctime, FFI.nullPtr)

      EndOfCompile ->
        pure (10, nullHandle, FFI.nullPtr, FFI.nullPtr)

      StartOfSimulation ->
        pure (11, nullHandle, FFI.nullPtr, FFI.nullPtr)

      EndOfSimulation ->
        pure (12, nullHandle, FFI.nullPtr, FFI.nullPtr)

      RuntimeError ->
        pure (13, nullHandle, FFI.nullPtr, FFI.nullPtr)

      TchkViolation ->
        pure (14, nullHandle, FFI.nullPtr, FFI.nullPtr)

      StartOfSave ->
        pure (15, nullHandle, FFI.nullPtr, FFI.nullPtr)

      EndOfSave ->
        pure (16, nullHandle, FFI.nullPtr, FFI.nullPtr)

      StartOfRestart ->
        pure (17, nullHandle, FFI.nullPtr, FFI.nullPtr)

      EndOfRestart ->
        pure (18, nullHandle, FFI.nullPtr, FFI.nullPtr)

      StartOfReset ->
        pure (19, nullHandle, FFI.nullPtr, FFI.nullPtr)

      EndOfReset ->
        pure (20, nullHandle, FFI.nullPtr, FFI.nullPtr)

      EnterInteractive ->
        pure (21, nullHandle, FFI.nullPtr, FFI.nullPtr)

      ExitInteractive ->
        pure (22, nullHandle, FFI.nullPtr, FFI.nullPtr)

      InteractiveScopeChange ->
        pure (23, nullHandle, FFI.nullPtr, FFI.nullPtr)

      UnresolvedSysTf ->
        pure (24, nullHandle, FFI.nullPtr, FFI.nullPtr)

#if defined(VERILOG_2001)
      AfterAssign handle timeTy valueFmt -> do
        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- send valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (25, coerce handle, ctime, cvalue)

      AfterDeassign handle timeTy valueFmt -> do
        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- send valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (26, coerce handle, ctime, cvalue)

      AfterDisable handle timeTy valueFmt -> do
        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- send valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (27, coerce handle, ctime, cvalue)

      PliError ->
        pure (28, nullHandle, FFI.nullPtr, FFI.nullPtr)

      Signal ->
        pure (29, nullHandle, FFI.nullPtr, FFI.nullPtr)
#endif
#if defined(VERILOG_2005)
      NbaSynch mHandle time -> do
        let object = maybe nullHandle coerce mHandle
        ctime <- pokeSend time
        pure (31, object, ctime, FFI.nullPtr)

      AtEndOfSimTime mHandle time -> do
        let object = maybe nullHandle coerce mHandle
        ctime <- pokeSend time
        pure (31, object, ctime, FFI.nullPtr)
#endif

data UnknownCallbackReason
  = UnknownCallbackReason CInt CallStack
  deriving anyclass (Exception)

instance Show UnknownCallbackReason where
  show (UnknownCallbackReason x c) =
    mconcat
      [ "Unknown callback reason: "
      , show x
      , "\n"
      , prettyCallStack c
      ]

instance UnsafeReceive CallbackReason where
  type Received CallbackReason = (CInt, Object, Ptr CTime, Ptr CValue)

  unsafeReceive (creason, object, ctime, cvalue) =
    let mObject = if isNullHandle object then Nothing else Just object in
    case creason of
      1 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= unsafeReceive
        pure (AfterValueChange object timeTy valueFmt)

      2 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        pure (BeforeStatement object timeTy)

      3 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= unsafeReceive
        pure (AfterForce mObject timeTy valueFmt)

      4 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= unsafeReceive
        pure (AfterRelease mObject timeTy valueFmt)

      5 -> do
        time <- unsafePeekReceive ctime
        pure (AtStartOfSimTime mObject time)

      6 -> do
        time <- unsafePeekReceive ctime
        pure (ReadWriteSynch mObject time)

      7 -> do
        time <- unsafePeekReceive ctime
        pure (ReadOnlySynch mObject time)

      8 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        pure (NextSimTime mObject timeTy)

      9 -> do
        time <- unsafePeekReceive ctime
        pure (AfterDelay mObject time)

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
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= unsafeReceive
        pure (AfterAssign object timeTy valueFmt)

      26 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= unsafeReceive
        pure (AfterDeassign object timeTy valueFmt)

      27 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= unsafeReceive
        pure (AfterDisable object timeTy valueFmt)

      28 ->
        pure PliError

      29 ->
        pure Signal
#endif
#if defined(VERILOG_2005)
      30 -> do
        time <- unsafePeekReceive ctime
        pure (NbaSynch mObject time)

      31 -> do
        time <- unsafePeekReceive ctime
        pure (AtEndOfSimTime mObject time)
#endif

      n  -> Sim.throw (UnknownCallbackReason n callStack)

instance Receive CallbackReason where
  receive (creason, object, ctime, cvalue) =
    let mObject = if isNullHandle object then Nothing else Just object in
    case creason of
      1 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= receive
        pure (AfterValueChange object timeTy valueFmt)

      2 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        pure (BeforeStatement object timeTy)

      3 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= receive
        pure (AfterForce mObject timeTy valueFmt)

      4 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= receive
        pure (AfterRelease mObject timeTy valueFmt)

      5 -> do
        time <- peekReceive ctime
        pure (AtStartOfSimTime mObject time)

      6 -> do
        time <- peekReceive ctime
        pure (ReadWriteSynch mObject time)

      7 -> do
        time <- peekReceive ctime
        pure (ReadOnlySynch mObject time)

      8 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        pure (NextSimTime mObject timeTy)

      9 -> do
        time <- peekReceive ctime
        pure (AfterDelay mObject time)

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
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= receive
        pure (AfterAssign object timeTy valueFmt)

      26 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= receive
        pure (AfterDeassign object timeTy valueFmt)

      27 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= receive
        pure (AfterDisable object timeTy valueFmt)

      28 ->
        pure PliError

      29 ->
        pure Signal
#endif
#if defined(VERILOG_2005)
      30 -> do
        time <- peekReceive ctime
        pure (NbaSynch mObject time)

      31 -> do
        time <- peekReceive ctime
        pure (AtEndOfSimTime mObject time)
#endif

      n -> Sim.throw (UnknownCallbackReason n callStack)

