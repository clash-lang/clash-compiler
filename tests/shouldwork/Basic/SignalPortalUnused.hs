module SignalPortalUnused where

import Clash.Prelude

topEntity :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
topEntity x = portalSource "unused" (x + 1)
{-# OPAQUE topEntity #-}
