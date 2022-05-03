{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

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
import           Data.Maybe (fromMaybe)
import           Foreign.C.Types (CInt(..))
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import           Clash.FFI.Monad (SimCont)
import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View

-- | A control command to send to the simulator. Depending on the simulator,
-- additional control commands may be available.
--
data Control
  = Stop DiagnosticLevel
  -- ^ Call the @\$stop@ built-in task after the VPI call.
  | Finish DiagnosticLevel
  -- ^ Call the @\$finish@ built-in task after the VPI call.
  | Reset StopValue (Maybe CInt) DiagnosticLevel
  -- ^ Call the @\$reset@ built-in task after the VPI call. The simulator can
  -- be reset into either interactive or batch mode, and optionally return an
  -- integer (for persisting a value between simulator runs).
  deriving stock (Show)

-- Control constant, stop value, reset value, diagnostic level
type instance CRepr Control = (CInt, CInt, CInt, CInt)

instance Send Control where
  send =
    \case
      Stop d -> (66, 0, 0, ) <$> send d
      Finish d -> (67, 0, 0, ) <$> send d
      Reset s r d ->
        (68,,,) <$> send s <*> pure (fromMaybe 0 r) <*> send d

-- | When resetting the simulator, the stop value determines whether the
-- simulator will enter interactive mode or immediately start processing again.
--
data StopValue
  = Interactive
  | Processing
  deriving stock (Show)

type instance CRepr StopValue = CInt

instance Send StopValue where
  send =
    pure . \case
      Interactive -> 0
      Processing -> 1

-- | When stopping
data DiagnosticLevel
  = NoDiagnostics
  | TimeAndLocation
  | TimeLocationAndStats
  deriving stock (Show)

type instance CRepr DiagnosticLevel = CInt

instance Send DiagnosticLevel where
  send =
    pure . \case
      NoDiagnostics -> 0
      TimeAndLocation -> 1
      TimeLocationAndStats -> 2

-- | An exception thrown when the simulator could not perform a control action.
--
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

-- | Send a control signal to the simulator, allowing simulation to be stopped,
-- restarted or reset. If the simulator does not accept the control action, a
-- 'CouldNotControl' exception is thrown.
--
controlSimulator :: forall o. Control -> SimCont o ()
controlSimulator control = do
  (c, s, r, d) <- send control

  success <-
    case control of
      Reset{} -> IO.liftIO (c_vpi_control_restart c s r d)
      _ -> IO.liftIO (c_vpi_control_end c d)

  Monad.unless success $
    Sim.throw (CouldNotControl control callStack)

  pure ()
#else
  () where
#endif
