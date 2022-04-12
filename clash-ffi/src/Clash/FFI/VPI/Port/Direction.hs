{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

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

-- | The direction of a port in a module. This does not include the mixed IO
-- or no direction port types from the specification, as they do not seem
-- possible with syntactically correct verilog (also according to the
-- specification).
--
data Direction
  = Input
  -- ^ An input port.
  | Output
  -- ^ An output port.
  | InOut
  -- ^ A bidirectional port.

-- | An exception thrown when decoding a port direction if an invalid value is
-- given for the C enum that specifies the constructor of 'Direction'.
--
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
    n -> Sim.throw (UnknownDirection n callStack)

instance Receive Direction where
  receive = unsafeReceive

