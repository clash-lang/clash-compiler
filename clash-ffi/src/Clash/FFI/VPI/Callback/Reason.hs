{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Callback.Reason
  ( CallbackReason(..)
  ) where

import           Control.Exception (Exception)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Data.Maybe (fromMaybe)
import           Foreign.C.Types (CInt)
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)
import qualified Foreign.Storable as FFI (peekByteOff, pokeByteOff)
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import qualified Clash.FFI.Monad as Sim
import           Clash.FFI.View
import           Clash.FFI.VPI.Object
import           Clash.FFI.VPI.Time
import           Clash.FFI.VPI.Value

data CallbackReason where
  AfterValueChange :: Handle -> TimeType -> ValueFormat -> CallbackReason
  BeforeStatement :: Handle -> TimeType -> CallbackReason
  AfterForce :: Maybe Handle -> TimeType -> ValueFormat -> CallbackReason
  AfterRelease :: Maybe Handle -> TimeType -> ValueFormat -> CallbackReason
  AtStartOfSimTime :: Maybe Handle -> Time -> CallbackReason
  ReadWriteSynch :: Maybe Handle -> Time -> CallbackReason
  ReadOnlySynch :: Maybe Handle -> Time -> CallbackReason
  NextSimTime :: Maybe Handle -> TimeType -> CallbackReason
  AfterDelay :: Maybe Handle -> Time -> CallbackReason
  EndOfCompile :: CallbackReason
  StartOfSimulation :: CallbackReason
  EndOfSimulation :: CallbackReason
  RuntimeError :: CallbackReason
  TchkViolation :: CallbackReason
  StartOfSave :: CallbackReason
  EndOfSave :: CallbackReason
  StartOfRestart :: CallbackReason
  EndOfRestart :: CallbackReason
  StartOfReset :: CallbackReason
  EndOfReset :: CallbackReason
  EnterInteractive :: CallbackReason
  ExitInteractive :: CallbackReason
  InteractiveScopeChange :: CallbackReason
  UnresolvedSysTf :: CallbackReason
#if defined(VERILOG_2001)
  AfterAssign :: Handle -> TimeType -> ValueFormat -> CallbackReason
  AfterDeassign :: Handle -> TimeType -> ValueFormat -> CallbackReason
  AfterDisable :: Handle -> TimeType -> ValueFormat -> CallbackReason
  PliError :: CallbackReason
  Signal :: CallbackReason
#endif
#if defined(VERILOG_2005)
  NbaSynch :: Maybe Handle -> Time -> CallbackReason
  AtEndOfSimTime :: Maybe Handle -> Time -> CallbackReason
#endif

instance UnsafeSend CallbackReason where
  type Sent CallbackReason = (CInt, Handle, Ptr CTime, Ptr CValue)

  unsafeSend =
    \case
      AfterValueChange handle timeTy valueFmt -> do
        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- unsafeSend valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (1, handle, ctime, cvalue)

      BeforeStatement handle timeTy -> do
        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        pure (2, handle, ctime, FFI.nullPtr)

      AfterForce mHandle timeTy valueFmt -> do
        let handle = fromMaybe nullHandle mHandle

        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- unsafeSend valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (3, handle, ctime, cvalue)

      AfterRelease mHandle timeTy valueFmt -> do
        let handle = fromMaybe nullHandle mHandle

        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- unsafeSend valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (4, handle, ctime, cvalue)

      AtStartOfSimTime mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- unsafePokeSend time
        pure (5, handle, ctime, FFI.nullPtr)

      ReadWriteSynch mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- unsafePokeSend time
        pure (6, handle, ctime, FFI.nullPtr)

      ReadOnlySynch mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- unsafePokeSend time
        pure (7, handle, ctime, FFI.nullPtr)

      NextSimTime mHandle timeTy -> do
        let handle = fromMaybe nullHandle mHandle

        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        pure (8, handle, ctime, FFI.nullPtr)

      AfterDelay mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- unsafePokeSend time
        pure (7, handle, ctime, FFI.nullPtr)

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

        pure (25, handle, ctime, cvalue)

      AfterDeassign handle timeTy valueFmt -> do
        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- unsafeSend valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (26, handle, ctime, cvalue)

      AfterDisable handle timeTy valueFmt -> do
        ctimeTy <- unsafeSend timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- unsafeSend valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (27, handle, ctime, cvalue)

      PliError ->
        pure (28, nullHandle, FFI.nullPtr, FFI.nullPtr)

      Signal ->
        pure (29, nullHandle, FFI.nullPtr, FFI.nullPtr)
#endif
#if defined(VERILOG_2005)
      NbaSynch mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- unsafePokeSend time
        pure (31, handle, ctime, FFI.nullPtr)

      AtEndOfSimTime mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- unsafePokeSend time
        pure (31, handle, ctime, FFI.nullPtr)
#endif

instance Send CallbackReason where
  send =
    \case
      AfterValueChange handle timeTy valueFmt -> do
        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- send valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (1, handle, ctime, cvalue)

      BeforeStatement handle timeTy -> do
        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        pure (2, handle, ctime, FFI.nullPtr)

      AfterForce mHandle timeTy valueFmt -> do
        let handle = fromMaybe nullHandle mHandle

        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- send valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (3, handle, ctime, cvalue)

      AfterRelease mHandle timeTy valueFmt -> do
        let handle = fromMaybe nullHandle mHandle

        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- send valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (4, handle, ctime, cvalue)

      AtStartOfSimTime mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- pokeSend time
        pure (5, handle, ctime, FFI.nullPtr)

      ReadWriteSynch mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- pokeSend time
        pure (6, handle, ctime, FFI.nullPtr)

      ReadOnlySynch mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- pokeSend time
        pure (7, handle, ctime, FFI.nullPtr)

      NextSimTime mHandle timeTy -> do
        let handle = fromMaybe nullHandle mHandle

        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        pure (8, handle, ctime, FFI.nullPtr)

      AfterDelay mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- pokeSend time
        pure (9, handle, ctime, FFI.nullPtr)

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

        pure (25, handle, ctime, cvalue)

      AfterDeassign handle timeTy valueFmt -> do
        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- send valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (26, handle, ctime, cvalue)

      AfterDisable handle timeTy valueFmt -> do
        ctimeTy <- send timeTy
        ctime <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)

        cfmt <- send valueFmt
        cvalue <- fst <$> Sim.withNewPtr Sim.heapPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (27, handle, ctime, cvalue)

      PliError ->
        pure (28, nullHandle, FFI.nullPtr, FFI.nullPtr)

      Signal ->
        pure (29, nullHandle, FFI.nullPtr, FFI.nullPtr)
#endif
#if defined(VERILOG_2005)
      NbaSynch mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- pokeSend time
        pure (31, handle, ctime, FFI.nullPtr)

      AtEndOfSimTime mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- pokeSend time
        pure (31, handle, ctime, FFI.nullPtr)
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
  type Received CallbackReason = (CInt, Handle, Ptr CTime, Ptr CValue)

  unsafeReceive (creason, handle, ctime, cvalue) =
    let mHandle = if isNullHandle handle then Nothing else Just handle in
    case creason of
      1 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= unsafeReceive
        pure (AfterValueChange handle timeTy valueFmt)

      2 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        pure (BeforeStatement handle timeTy)

      3 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= unsafeReceive
        pure (AfterForce mHandle timeTy valueFmt)

      4 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= unsafeReceive
        pure (AfterRelease mHandle timeTy valueFmt)

      5 -> do
        time <- unsafePeekReceive ctime
        pure (AtStartOfSimTime mHandle time)

      6 -> do
        time <- unsafePeekReceive ctime
        pure (ReadWriteSynch mHandle time)

      7 -> do
        time <- unsafePeekReceive ctime
        pure (ReadOnlySynch mHandle time)

      8 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        pure (NextSimTime mHandle timeTy)

      9 -> do
        time <- unsafePeekReceive ctime
        pure (AfterDelay mHandle time)

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
        pure (AfterAssign handle timeTy valueFmt)

      26 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= unsafeReceive
        pure (AfterDeassign handle timeTy valueFmt)

      27 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= unsafeReceive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= unsafeReceive
        pure (AfterDisable handle timeTy valueFmt)

      28 ->
        pure PliError

      29 ->
        pure Signal
#endif
#if defined(VERILOG_2005)
      30 -> do
        time <- unsafePeekReceive ctime
        pure (NbaSynch mHandle time)

      31 -> do
        time <- unsafePeekReceive ctime
        pure (AtEndOfSimTime mHandle time)
#endif

      n  -> Sim.throw (UnknownCallbackReason n callStack)

instance Receive CallbackReason where
  receive (creason, handle, ctime, cvalue) =
    let mHandle = if isNullHandle handle then Nothing else Just handle in
    case creason of
      1 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= receive
        pure (AfterValueChange handle timeTy valueFmt)

      2 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        pure (BeforeStatement handle timeTy)

      3 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= receive
        pure (AfterForce mHandle timeTy valueFmt)

      4 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= receive
        pure (AfterRelease mHandle timeTy valueFmt)

      5 -> do
        time <- peekReceive ctime
        pure (AtStartOfSimTime mHandle time)

      6 -> do
        time <- peekReceive ctime
        pure (ReadWriteSynch mHandle time)

      7 -> do
        time <- peekReceive ctime
        pure (ReadOnlySynch mHandle time)

      8 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        pure (NextSimTime mHandle timeTy)

      9 -> do
        time <- peekReceive ctime
        pure (AfterDelay mHandle time)

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
        pure (AfterAssign handle timeTy valueFmt)

      26 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= receive
        pure (AfterDeassign handle timeTy valueFmt)

      27 -> do
        timeTy <- IO.liftIO (FFI.peekByteOff ctime 0) >>= receive
        valueFmt <- IO.liftIO (FFI.peekByteOff cvalue 0) >>= receive
        pure (AfterDisable handle timeTy valueFmt)

      28 ->
        pure PliError

      29 ->
        pure Signal
#endif
#if defined(VERILOG_2005)
      30 -> do
        time <- peekReceive ctime
        pure (NbaSynch mHandle time)

      31 -> do
        time <- peekReceive ctime
        pure (AtEndOfSimTime mHandle time)
#endif

      n  -> Sim.throw (UnknownCallbackReason n callStack)

