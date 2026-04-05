{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}

module MaybeUnpackUndefined where

import Clash.Prelude
import Clash.Sized.Internal.BitVector

state = maybeUnpack undefined# :: Maybe (Vec 3 Bit)
topEntity a = register @System state a
