{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Value.Delay
  ( DelayMode(..)
  ) where

import           Foreign.C.Types (CInt)
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)

import           Clash.FFI.View (Send(..), UnsafeSend(..), unsafePokeSend, pokeSend)
import           Clash.FFI.VPI.Object.Time (CTime, Time)

data DelayMode
  = NoDelay
  | InertialDelay Time
  | TransportDelay Time
  | PureTransportDelay Time
  | Force
  | Release

instance UnsafeSend DelayMode where
  type Sent DelayMode = (Ptr CTime, CInt)

  unsafeSend = \case
    NoDelay ->
      pure (FFI.nullPtr, 1)

    InertialDelay after ->
      (,2) <$> unsafePokeSend after

    TransportDelay after ->
      (,3) <$> unsafePokeSend after

    PureTransportDelay after ->
      (,4) <$> unsafePokeSend after

    Force ->
      pure (FFI.nullPtr, 5)

    Release ->
      pure (FFI.nullPtr, 6)

instance Send DelayMode where
  send = \case
    NoDelay ->
      pure (FFI.nullPtr, 1)

    InertialDelay after ->
      (,2) <$> pokeSend after

    TransportDelay after ->
      (,3) <$> pokeSend after

    PureTransportDelay after ->
      (,4) <$> pokeSend after

    Force ->
      pure (FFI.nullPtr, 5)

    Release ->
      pure (FFI.nullPtr, 6)

