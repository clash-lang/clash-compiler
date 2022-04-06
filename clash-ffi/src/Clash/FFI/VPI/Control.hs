{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Clash.FFI.VPI.Control
#if defined(VERILOG_2001)
  ( Control(..)
  , StopValue(..)
  , DiagnosticLevel(..)
  , CouldNotControl(..)
  , controlSimulator
  ) where

import           Control.Exception (Exception)
import qualified Control.Monad as Monad (unless)
import qualified Control.Monad.IO.Class as IO (liftIO)
import           Foreign.C.Types (CInt(..))
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (throw)

data Control
  = Stop DiagnosticLevel
  | Finish DiagnosticLevel
  | Reset StopValue (Maybe CInt) DiagnosticLevel
  deriving stock (Show)

data StopValue
  = Interactive
  | Batch
  deriving stock (Show)

stopValueToCInt :: StopValue -> CInt
stopValueToCInt = \case
  Interactive -> 0
  Batch -> 1

data DiagnosticLevel
  = NoDiagnostics
  | TimeAndLocation
  | TimeLocationAndStats
  deriving stock (Show)

diagnosticLevelToCInt :: DiagnosticLevel -> CInt
diagnosticLevelToCInt = \case
  NoDiagnostics -> 0
  TimeAndLocation -> 1
  TimeLocationAndStats -> 2

data CouldNotControl
  = CouldNotControl Control CallStack
  deriving anyclass (Exception)

instance Show CouldNotControl where
  show (CouldNotControl x c) =
    mconcat
      [ "Could not perform simulator control command: "
      , show x
      , "\n"
      , prettyCallStack c
      ]

foreign import ccall "vpi_user.h vpi_control"
  c_vpi_control_end :: CInt -> CInt -> IO Bool

foreign import ccall "vpi_user.h vpi_control"
  c_vpi_control_restart :: CInt -> CInt -> CInt -> CInt -> IO Bool

controlSimulator :: Control -> SimCont o ()
controlSimulator control =
  liftOrThrow $
    case control of
      Stop d ->
        c_vpi_control_end 66 (diagnosticLevelToCInt d)

      Finish d ->
        c_vpi_control_end 67 (diagnosticLevelToCInt d)

      Reset s (Just r) d ->
        c_vpi_control_restart 68 (stopValueToCInt s) r (diagnosticLevelToCInt d)

      Reset s Nothing d ->
        c_vpi_control_restart 68 (stopValueToCInt s) 0 (diagnosticLevelToCInt d)
 where
  liftOrThrow f =
    IO.liftIO f >>= (`Monad.unless` Sim.throw (CouldNotControl control callStack))
#else
  () where
#endif

