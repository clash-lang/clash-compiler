module Shift where

import CLaSH.Prelude

topEntity :: Unsigned 32 -> Unsigned 32
topEntity y = shift y (-1)
