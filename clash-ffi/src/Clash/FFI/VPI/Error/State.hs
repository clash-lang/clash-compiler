{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Error.State
  ( ErrorState(..)
  , UnknownErrorState(..)
  ) where

import           Control.Exception (Exception)
import           Foreign.C.Types (CInt)
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View (Receive(..), UnsafeReceive(..))

-- | The state of the simulator when an error occured. This specifies whether
-- the error occured before a simulation started, while in a PLI/VPI call, or
-- while the simulation was running.
--
data ErrorState
  = CompileError
  | PliError
  | RunError
  deriving stock (Eq, Show)

-- | An exception thrown when decoding an error state if an invalid value is
-- given for the C enum that specifies the constructor of 'ErrorState'.
--
data UnknownErrorState
  = UnknownErrorState CInt CallStack
  deriving anyclass (Exception)

instance Show UnknownErrorState where
  show (UnknownErrorState x c) =
    mconcat
      [ "Unknown error state: "
      , show x
      , "\n"
      , prettyCallStack c
      ]

instance UnsafeReceive ErrorState where
  type Received ErrorState = CInt

  unsafeReceive = \case
    1 -> pure CompileError
    2 -> pure PliError
    3 -> pure RunError
    n -> Sim.throw (UnknownErrorState n callStack)

instance Receive ErrorState where
  receive = unsafeReceive

