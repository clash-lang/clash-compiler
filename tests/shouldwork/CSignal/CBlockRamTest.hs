module CBlockRamTest where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

topEntity :: CSignal 10 (Unsigned 7)
          -> CSignal 10 (Unsigned 7)
          -> CSignal 10 (Bool)
          -> CSignal 10 (Vec 4 Bit)
          -> CSignal 10 (Vec 4 Bit)
topEntity = cblockRam (Clock d10) (vcopy d128 (vcopyI H))
