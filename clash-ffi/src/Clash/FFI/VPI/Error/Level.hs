{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Error.Level
  ( ErrorLevel(..)
  , UnknownErrorLevel(..)
  ) where

import           Control.Exception (Exception)
import           Foreign.C.Types (CInt)
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View (Receive(..), UnsafeReceive(..))

-- | The level, or severity of an error returned by a call to @vpi_chk_error@.
--
data ErrorLevel
  = Success
  | Notice
  | Warning
  | Error
  | System
  | Internal
  deriving stock (Show)

-- | An exception thrown when decoding an error level if an invalid value is
-- given for the C enum that specifies the constructor of 'ErrorLevel'. While
-- it is not listed in the specification under error levels, the value of 0 is
-- a valid error level meaning the previous operation succeeded.
--
data UnknownErrorLevel
  = UnknownErrorLevel CInt CallStack
  deriving anyclass (Exception)

instance Show UnknownErrorLevel where
  show (UnknownErrorLevel x c) =
    mconcat
      [ "Unknown error level: "
      , show x
      , "\n"
      , prettyCallStack c
      ]

instance UnsafeReceive ErrorLevel where
  type Received ErrorLevel = CInt

  unsafeReceive = \case
    0 -> pure Success
    1 -> pure Notice
    2 -> pure Warning
    3 -> pure Error
    4 -> pure System
    5 -> pure Internal
    n -> Sim.throw (UnknownErrorLevel n callStack)

instance Receive ErrorLevel where
  receive = unsafeReceive

