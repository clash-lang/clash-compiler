{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Port.Direction
  ( Direction(..)
  , UnknownDirection(..)
  ) where

import           Control.Exception (Exception)
import           Foreign.C.Types (CInt)
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View

data Direction
  = Input
  | Output
  | InOut
  | MixedIO
  | NoDirection

data UnknownDirection
  = UnknownDirection CInt CallStack
  deriving anyclass (Exception)

instance Show UnknownDirection where
  show (UnknownDirection d c) =
    mconcat
      [ "Unknown port direction: "
      , show d
      , "\n"
      , prettyCallStack c
      ]

instance UnsafeReceive Direction where
  type Received Direction = CInt

  unsafeReceive = \case
    1 -> pure Input
    2 -> pure Output
    3 -> pure InOut
    4 -> pure MixedIO
    5 -> pure NoDirection
    n -> Sim.throw (UnknownDirection n callStack)

instance Receive Direction where
  receive = unsafeReceive

