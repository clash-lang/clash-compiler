{-|
Copyright:    (C) 2022 Google Inc.
License:      BSD2 (see the file LICENSE)
Maintainer:   QBayLogic B.V. <devops@qbaylogic.com>
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.FFI.VPI.Object.Value.Delay
  ( DelayMode(..)
  ) where

import           Foreign.C.Types (CInt)
import           Foreign.Ptr (Ptr)
import qualified Foreign.Ptr as FFI (nullPtr)

import           Clash.FFI.View (Send(..), UnsafeSend(..), unsafePokeSend, pokeSend)
import           Clash.FFI.VPI.Object.Time (CTime, Time)

-- | The delay mode specifies how the value of an object is updated in a call
-- to @vpi_put_value@. The mode selected influences how the next value of the
-- object is determined (in the event of multiple attempted writes per step).
--
data DelayMode
  = NoDelay
  -- ^ The value is immediately updated.
  | InertialDelay Time
  -- ^ The value is updated after all events at the given time are executed.
  | TransportDelay Time
  -- ^ All scheduled events that occur later than the given time are removed
  -- and the value is updated.
  | PureTransportDelay Time
  -- ^ No scheduled events are removed and the value is updated.
  | Force
  -- ^ The object is forced to the new value without delay. This behaves the
  -- same as the verilog procedural @force@ keyword.
  | Release
  -- ^ The object is released from a forced value, then updated to the new
  -- value without delay. This behaves the same as the verilog procedural
  -- @release@ keyword.

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

