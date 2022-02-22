{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Callback.Reason
  ( CallbackReason(..)
  ) where

import           Control.Exception (Exception)
import           Foreign.C.Types (CInt)
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View
import           Clash.FFI.VPI.Object (Handle)
import           Clash.FFI.VPI.Time
import           Clash.FFI.VPI.Value (ValueFormat)

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
  type Sent CallbackReason = (CInt, Handle, CTime, CInt)

  unsafeSend =
    pure . \case
      ValueChange -> 1
      Statement -> 2
      DidForce -> 3
      DidRelease -> 4
      AtStartOfSimTime -> 5
      ReadWriteSynch -> 6
      ReadOnlySynch -> 7
      NextSimTime -> 8
      AfterDelay -> 9
      EndOfCompile -> 10
      StartOfSimulation -> 11
      EndOfSimulation -> 12
      DidError -> 13
      TchkViolation -> 14
      StartOfSave -> 15
      EndOfSave -> 16
      StartOfRestart -> 17
      EndOfRestart -> 18
      StartOfReset -> 19
      EndOfReset -> 20
      EnterInteractive -> 21
      ExitInteractive -> 22
      InteractiveScopeChange -> 23
      UnresolvedSysTf -> 24
#if defined(VERILOG_2001)
      DidAssign -> 25
      DidDeassign -> 26
      DidDisable -> 27
      DidPliError -> 28
      DidSignal -> 29
#endif
#if defined(VERILOG_2005)
      NbaSynch -> 30
      AtEndOfSimTime -> 31
#endif

instance Send CallbackReason where
  send = unsafeSend

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
  type Received CallbackReason = CInt

  unsafeReceive = \case
    1 -> pure ValueChange
    2 -> pure Statement
    3 -> pure DidForce
    4 -> pure DidRelease
    5 -> pure AtStartOfSimTime
    6 -> pure ReadWriteSynch
    7 -> pure ReadOnlySynch
    8 -> pure NextSimTime
    9 -> pure AfterDelay
    10 -> pure EndOfCompile
    11 -> pure StartOfSimulation
    12 -> pure EndOfSimulation
    13 -> pure DidError
    14 -> pure TchkViolation
    15 -> pure StartOfSave
    16 -> pure EndOfSave
    17 -> pure StartOfRestart
    18 -> pure EndOfRestart
    19 -> pure StartOfReset
    20 -> pure EndOfReset
    21 -> pure EnterInteractive
    22 -> pure ExitInteractive
    23 -> pure InteractiveScopeChange
    24 -> pure UnresolvedSysTf
#if defined(VERILOG_2001)
    25 -> pure DidAssign
    26 -> pure DidDeassign
    27 -> pure DidDisable
    28 -> pure DidPliError
    29 -> pure DidSignal
#endif
#if defined(VERILOG_2005)
    30 -> pure NbaSynch
    31 -> pure AtEndOfSimTime
#endif
    n  -> Sim.throw (UnknownCallbackReason n callStack)

instance Receive CallbackReason where
  receive = unsafeReceive

