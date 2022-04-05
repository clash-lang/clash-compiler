{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Callback.Reason
  ( CallbackReason(..)
  ) where

import           Control.Exception (Exception)
import           Data.Maybe (fromMaybe)
import           Foreign.C.Types (CInt)
import           Foreign.Ptr (Ptr)
import qualified Foreign.Storable as FFI (pokeByteOff)
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import qualified Clash.FFI.Monad as Sim
import           Clash.FFI.View
import           Clash.FFI.VPI.Object
import           Clash.FFI.VPI.Time
import           Clash.FFI.VPI.Value

data CallbackReason where
  AfterValueChange :: Handle -> TimeType -> ValueFormat n -> CallbackReason
  BeforeStatement :: Handle -> TimeType -> CallbackReason
  AfterForce :: Maybe Handle -> TimeType -> ValueFormat n -> CallbackReason
  AfterRelease :: Maybe Handle -> TimeType -> ValueFormat n -> CallbackReason
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
  AfterAssign :: Handle -> TimeType -> ValueFormat n -> CallbackReason
  AfterDeassign :: Handle -> TimeType -> ValueFormat n -> CallbackReason
  AfterDisable :: Handle -> TimeType -> ValueFormat n -> CallbackReason
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
        cfmt <- unsafeSend valueFmt

        ctime <- Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 ctimeTy)
        cvalue <- Sim.withNewPtr Sim.stackPtr (\ptr -> FFI.pokeByteOff ptr 0 cfmt)

        pure (1, handle, ctime, cvalue)

      BeforeStatement handle timeTy -> do
        ctimeTy <- unsafeSend timeTy
        pure (2, handle, CTime ctimeTy 0 0 0.0, 0)

      AfterForce mHandle timeTy valueFmt -> do
        let handle = fromMaybe nullHandle mHandle
        ctimeTy <- unsafeSend timeTy
        cfmt <- unsafeSend valueFmt
        pure (3, handle, CTime ctimeTy 0 0 0.0, cfmt)

      AfterRelease mHandle timeTy valueFmt -> do
        let handle = fromMaybe nullHandle mHandle
        ctimeTy <- unsafeSend timeTy
        cfmt <- unsafeSend valueFmt
        pure (4, handle, CTime ctimeTy 0 0 0.0, cfmt)

      AtStartOfSimTime mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- unsafeSend time
        pure (5, handle, ctime, 0)

      ReadWriteSynch mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- unsafeSend time
        pure (6, handle, ctime, 0)

      ReadOnlySynch mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- unsafeSend time
        pure (7, handle, ctime, 0)

      NextSimTime mHandle timeTy -> do
        let handle = fromMaybe nullHandle mHandle
        ctimeTy <- unsafeSend timeTy
        pure (8, handle, CTime ctimeTy 0 0 0.0, 0)

      AfterDelay mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- unsafeSend time
        pure (7, handle, ctime, 0)

      EndOfCompile ->
        pure (10, nullHandle, CTime 0 0 0 0.0, 0)

      StartOfSimulation ->
        pure (11, nullHandle, CTime 0 0 0 0.0, 0)

      EndOfSimulation ->
        pure (12, nullHandle, CTime 0 0 0 0.0, 0)

      RuntimeError ->
        pure (13, nullHandle, CTime 0 0 0 0.0, 0)

      TchkViolation ->
        pure (14, nullHandle, CTime 0 0 0 0.0, 0)

      StartOfSave ->
        pure (15, nullHandle, CTime 0 0 0 0.0, 0)

      EndOfSave ->
        pure (16, nullHandle, CTime 0 0 0 0.0, 0)

      StartOfRestart ->
        pure (17, nullHandle, CTime 0 0 0 0.0, 0)

      EndOfRestart ->
        pure (18, nullHandle, CTime 0 0 0 0.0, 0)

      StartOfReset ->
        pure (19, nullHandle, CTime 0 0 0 0.0, 0)

      EndOfReset ->
        pure (20, nullHandle, CTime 0 0 0 0.0, 0)

      EnterInteractive ->
        pure (21, nullHandle, CTime 0 0 0 0.0, 0)

      ExitInteractive ->
        pure (22, nullHandle, CTime 0 0 0 0.0, 0)

      InteractiveScopeChange ->
        pure (23, nullHandle, CTime 0 0 0 0.0, 0)

      UnresolvedSysTf ->
        pure (24, nullHandle, CTime 0 0 0 0.0, 0)

#if defined(VERILOG_2001)
      AfterAssign handle timeTy valueFmt -> do
        ctimeTy <- unsafeSend timeTy
        cfmt <- unsafeSend valueFmt
        pure (25, handle, CTime ctimeTy 0 0 0.0, cfmt)

      AfterDeassign handle timeTy valueFmt -> do
        ctimeTy <- unsafeSend timeTy
        cfmt <- unsafeSend valueFmt
        pure (26, handle, CTime ctimeTy 0 0 0.0, cfmt)

      AfterDisable handle timeTy valueFmt -> do
        ctimeTy <- unsafeSend timeTy
        cfmt <- unsafeSend valueFmt
        pure (27, handle, CTime ctimeTy 0 0 0.0, cfmt)

      PliError ->
        pure (28, nullHandle, CTime 0 0 0 0.0, 0)

      Signal ->
        pure (29, nullHandle, CTime 0 0 0 0.0, 0)
#endif
#if defined(VERILOG_2005)
      NbaSynch mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- unsafeSend time
        pure (31, handle, ctime, 0)

      AtEndOfSimTime mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- unsafeSend time
        pure (31, handle, ctime, 0)
#endif

instance Send CallbackReason where
  send =
    \case
      AfterValueChange handle timeTy valueFmt -> do
        ctimeTy <- send timeTy
        cfmt <- send valueFmt
        pure (1, handle, CTime ctimeTy 0 0 0.0, cfmt)

      BeforeStatement handle timeTy -> do
        ctimeTy <- send timeTy
        pure (2, handle, CTime ctimeTy 0 0 0.0, 0)

      AfterForce mHandle timeTy valueFmt -> do
        let handle = fromMaybe nullHandle mHandle
        ctimeTy <- send timeTy
        cfmt <- send valueFmt
        pure (3, handle, CTime ctimeTy 0 0 0.0, cfmt)

      AfterRelease mHandle timeTy valueFmt -> do
        let handle = fromMaybe nullHandle mHandle
        ctimeTy <- send timeTy
        cfmt <- send valueFmt
        pure (4, handle, CTime ctimeTy 0 0 0.0, cfmt)

      AtStartOfSimTime mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- send time
        pure (5, handle, ctime, 0)

      ReadWriteSynch mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- send time
        pure (6, handle, ctime, 0)

      ReadOnlySynch mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- send time
        pure (7, handle, ctime, 0)

      NextSimTime mHandle timeTy -> do
        let handle = fromMaybe nullHandle mHandle
        ctimeTy <- send timeTy
        pure (8, handle, CTime ctimeTy 0 0 0.0, 0)

      AfterDelay mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- send time
        pure (9, handle, ctime, 0)

      EndOfCompile ->
        pure (10, nullHandle, CTime 0 0 0 0.0, 0)

      StartOfSimulation ->
        pure (11, nullHandle, CTime 0 0 0 0.0, 0)

      EndOfSimulation ->
        pure (12, nullHandle, CTime 0 0 0 0.0, 0)

      RuntimeError ->
        pure (13, nullHandle, CTime 0 0 0 0.0, 0)

      TchkViolation ->
        pure (14, nullHandle, CTime 0 0 0 0.0, 0)

      StartOfSave ->
        pure (15, nullHandle, CTime 0 0 0 0.0, 0)

      EndOfSave ->
        pure (16, nullHandle, CTime 0 0 0 0.0, 0)

      StartOfRestart ->
        pure (17, nullHandle, CTime 0 0 0 0.0, 0)

      EndOfRestart ->
        pure (18, nullHandle, CTime 0 0 0 0.0, 0)

      StartOfReset ->
        pure (19, nullHandle, CTime 0 0 0 0.0, 0)

      EndOfReset ->
        pure (20, nullHandle, CTime 0 0 0 0.0, 0)

      EnterInteractive ->
        pure (21, nullHandle, CTime 0 0 0 0.0, 0)

      ExitInteractive ->
        pure (22, nullHandle, CTime 0 0 0 0.0, 0)

      InteractiveScopeChange ->
        pure (23, nullHandle, CTime 0 0 0 0.0, 0)

      UnresolvedSysTf ->
        pure (24, nullHandle, CTime 0 0 0 0.0, 0)

#if defined(VERILOG_2001)
      AfterAssign handle timeTy valueFmt -> do
        ctimeTy <- send timeTy
        cfmt <- send valueFmt
        pure (25, handle, CTime ctimeTy 0 0 0.0, cfmt)

      AfterDeassign handle timeTy valueFmt -> do
        ctimeTy <- send timeTy
        cfmt <- send valueFmt
        pure (26, handle, CTime ctimeTy 0 0 0.0, cfmt)

      AfterDisable handle timeTy valueFmt -> do
        ctimeTy <- send timeTy
        cfmt <- send valueFmt
        pure (27, handle, CTime ctimeTy 0 0 0.0, cfmt)

      PliError ->
        pure (28, nullHandle, CTime 0 0 0 0.0, 0)

      Signal ->
        pure (29, nullHandle, CTime 0 0 0 0.0, 0)
#endif
#if defined(VERILOG_2005)
      NbaSynch mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- send time
        pure (31, handle, ctime, 0)

      AtEndOfSimTime mHandle time -> do
        let handle = fromMaybe nullHandle mHandle
        ctime <- send time
        pure (31, handle, ctime, 0)
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

{-
instance UnsafeReceive CallbackReason where
  type Received CallbackReason = (CInt, Handle, CTime, CInt)

  unsafeReceive (creason, handle, ctime, cfmt) =
    let mHandle = if isNullHandle handle then Nothing else Just handle in
    case creason of
      1 -> do
        timeTy <- unsafeReceive (ctimeType ctime)
        valueFmt <- unsafeReceive cfmt
        pure (AfterValueChange handle timeTy valueFmt)

      2 -> do
        timeTy <- unsafeReceive (ctimeType ctime)
        pure (BeforeStatement handle timeTy)

      3 -> do
        timeTy <- unsafeReceive (ctimeType ctime)
        valueFmt <- unsafeReceive cfmt
        pure (AfterForce mHandle timeTy valueFmt)

      4 -> do
        timeTy <- unsafeReceive (ctimeType ctime)
        valueFmt <- unsafeReceive cfmt
        pure (AfterRelease mHandle timeTy valueFmt)

      5 -> do
        time <- unsafeReceive ctime
        pure (AtStartOfSimTime mHandle time)

      6 -> do
        time <- unsafeReceive ctime
        pure (ReadWriteSynch mHandle time)

      7 -> do
        time <- unsafeReceive ctime
        pure (ReadOnlySynch mHandle time)

      8 -> do
        timeTy <- unsafeReceive (ctimeType ctime)
        pure (NextSimTime mHandle timeTy)

      9 -> do
        time <- unsafeReceive ctime
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
        timeTy <- unsafeReceive (ctimeType ctime)
        valueFmt <- unsafeReceive cfmt
        pure (AfterAssign handle timeTy valueFmt)

      26 -> do
        timeTy <- unsafeReceive (ctimeType ctime)
        valueFmt <- unsafeReceive cfmt
        pure (AfterDeassign handle timeTy valueFmt)

      27 -> do
        timeTy <- unsafeReceive (ctimeType ctime)
        valueFmt <- unsafeReceive cfmt
        pure (AfterDisable handle timeTy valueFmt)

      28 ->
        pure PliError

      29 ->
        pure Signal
#endif
#if defined(VERILOG_2005)
      30 -> do
        time <- unsafeReceive ctime
        pure (NbaSynch mHandle time)

      31 -> do
        time <- unsafeReceive ctime
        pure (AtEndOfSimTime mHandle time)
#endif

      n  -> Sim.throw (UnknownCallbackReason n callStack)

instance Receive CallbackReason where
  receive (creason, handle, ctime, cfmt) =
    let mHandle = if isNullHandle handle then Nothing else Just handle in
    case creason of
      1 -> do
        timeTy <- receive (ctimeType ctime)
        valueFmt <- receive cfmt
        pure (AfterValueChange handle timeTy valueFmt)

      2 -> do
        timeTy <- receive (ctimeType ctime)
        pure (BeforeStatement handle timeTy)

      3 -> do
        timeTy <- receive (ctimeType ctime)
        valueFmt <- receive cfmt
        pure (AfterForce mHandle timeTy valueFmt)

      4 -> do
        timeTy <- receive (ctimeType ctime)
        valueFmt <- receive cfmt
        pure (AfterRelease mHandle timeTy valueFmt)

      5 -> do
        time <- receive ctime
        pure (AtStartOfSimTime mHandle time)

      6 -> do
        time <- receive ctime
        pure (ReadWriteSynch mHandle time)

      7 -> do
        time <- receive ctime
        pure (ReadOnlySynch mHandle time)

      8 -> do
        timeTy <- receive (ctimeType ctime)
        pure (NextSimTime mHandle timeTy)

      9 -> do
        time <- receive ctime
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
        timeTy <- receive (ctimeType ctime)
        valueFmt <- receive cfmt
        pure (AfterAssign handle timeTy valueFmt)

      26 -> do
        timeTy <- receive (ctimeType ctime)
        valueFmt <- receive cfmt
        pure (AfterDeassign handle timeTy valueFmt)

      27 -> do
        timeTy <- receive (ctimeType ctime)
        valueFmt <- receive cfmt
        pure (AfterDisable handle timeTy valueFmt)

      28 ->
        pure PliError

      29 ->
        pure Signal
#endif
#if defined(VERILOG_2005)
      30 -> do
        time <- receive ctime
        pure (NbaSynch mHandle time)

      31 -> do
        time <- receive ctime
        pure (AtEndOfSimTime mHandle time)
#endif

      n  -> Sim.throw (UnknownCallbackReason n callStack)
-}

