module T1402b where

import Clash.Prelude
import Clash.Sized.Internal.BitVector
import GHC.Natural

topEntity :: BitVector 128 -> Natural
topEntity = unsafeMask
