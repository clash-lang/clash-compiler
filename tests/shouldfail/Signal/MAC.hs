module MAC where

import Clash.Prelude

topEntity
  :: Integer
  -> Clock System Source
  -> Reset System Asynchronous
  -> (Signal System Integer, Signal System Integer)
  -> Signal System Integer
topEntity i = exposeClockReset (macT <^> i)

macT s (x,y) = (s',o)
  where
    s' = s + (x * y)
    o  = s
