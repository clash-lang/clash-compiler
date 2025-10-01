module T3021 where

import Clash.Prelude

topEntity ::
  (Unsigned 8, Signed 8, Index 8, Bit) -> (Unsigned 8, Signed 8, Index 8, Bit)
topEntity = ensureSpine
