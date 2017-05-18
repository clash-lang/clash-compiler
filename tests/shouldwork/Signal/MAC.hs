module MAC where

import CLaSH.Prelude

topEntity
  :: SystemClockReset
  => Integer
  -> (Signal System Integer, Signal System Integer)
  -> Signal System Integer
topEntity i = macT <^> i

macT s (x,y) = (s',o)
  where
    s' = s + (x * y)
    o  = s
