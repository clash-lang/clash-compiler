module Shift where

import Clash.Prelude

topEntity :: Unsigned 32 -> Unsigned 32
topEntity y = shift y (-1)
