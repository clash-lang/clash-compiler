{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Clash.FFI.VPI.Object.Value.Scalar
  ( Scalar(..)
  , UnknownScalarValue(..)
  , scalarToBit
  , bitToScalar
  ) where

import           Control.Exception (Exception)
import           Foreign.C.Types (CInt)
import           GHC.Stack (CallStack, callStack, prettyCallStack)

import           Clash.Sized.Internal.BitVector
import           Clash.XException (hasUndefined)

import qualified Clash.FFI.Monad as Sim (throw)
import           Clash.FFI.View

-- | A VPI scalar value. This is similar to the 9-value logic type from VHDL,
-- in that scalar values have an associated strength.
--
-- While we expose this type, the default 'Receive' instance for
-- 'Clash.FFI.VPI.Object.Value' will receive values as types from
-- @clash-prelude@ instead of using this type. However, since this type has a
-- 'Receive' instance, values can still be decoded to this type manually if
-- needed.
--
data Scalar
  = S0 -- Strong 0
  | S1 -- Strong 1
  | SZ -- High impedence
  | SX -- Undefined
  | SH -- Weak 1
  | SL -- Weak 0
  | S_ -- Don't care
  deriving stock (Show)

instance UnsafeSend Scalar where
  type Sent Scalar = CInt

  unsafeSend =
    pure . \case
      S0 -> 0
      S1 -> 1
      SZ -> 2
      SX -> 3
      SH -> 4
      SL -> 5
      S_ -> 6

instance Send Scalar where
  send = unsafeSend

-- | An exception thrown when decoding a scalar value if an invalid value is
-- given for the C enum that specifies the constructor of 'Scalar'.
--
data UnknownScalarValue
  = UnknownScalarValue CInt CallStack
  deriving anyclass (Exception)

instance Show UnknownScalarValue where
  show (UnknownScalarValue x c) =
    mconcat
      [ "Unknown scalar value: "
      , show x
      , "\n"
      , prettyCallStack c
      ]

instance UnsafeReceive Scalar where
  type Received Scalar = CInt

  unsafeReceive = \case
    0 -> pure S0
    1 -> pure S1
    2 -> pure SZ
    3 -> pure SX
    4 -> pure SH
    5 -> pure SL
    6 -> pure S_
    n -> Sim.throw (UnknownScalarValue n callStack)

instance Receive Scalar where
  receive = unsafeReceive

bitToScalar :: Bit -> Scalar
bitToScalar b
  | hasUndefined b  = SX
  | b == low        = S0
  | b == high       = S1
  | otherwise       = SX

instance UnsafeSend Bit where
  type Sent Bit = Sent Scalar

  unsafeSend =
    unsafeSend . bitToScalar

instance Send Bit where
  send =
    send . bitToScalar

scalarToBit :: Scalar -> Bit
scalarToBit = \case
  S0 -> low
  S1 -> high
  SH -> high
  SL -> low
  _  -> Bit 1 0

instance UnsafeReceive Bit where
  type Received Bit = Received Scalar

  unsafeReceive =
    fmap scalarToBit . unsafeReceive

instance Receive Bit where
  receive =
    fmap scalarToBit . receive

