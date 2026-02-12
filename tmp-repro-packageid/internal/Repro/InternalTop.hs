module Repro.InternalTop where

import Clash.Prelude
import Repro.InternalBB (bb)

topEntity :: Signal System Int -> Signal System Int
topEntity = bb
{-# OPAQUE topEntity #-}
