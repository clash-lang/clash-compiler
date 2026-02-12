module LocalTop where

import Clash.Prelude
import Repro.InternalBB (bbV)

topEntity :: Signal System Int -> Signal System Int
topEntity = bbV
{-# OPAQUE topEntity #-}
