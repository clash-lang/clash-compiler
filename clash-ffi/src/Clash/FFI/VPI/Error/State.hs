{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Error.State
  ( ErrorState(..)
  ) where

import           Control.Exception (Exception)
import           Foreign.C.Types (CInt)
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View (Receive(..), UnsafeReceive(..))

data ErrorState
  = CompileError
  | PliError
  | RunError
  deriving stock (Eq, Show)

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

