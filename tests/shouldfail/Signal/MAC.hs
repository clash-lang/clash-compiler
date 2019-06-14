module MAC where

import Clash.Prelude

topEntity
  :: Integer
  -> Clock System
  -> Reset System
  -> Enable System
  -> (Signal System Integer, Signal System Integer)
  -> Signal System Integer
topEntity i = exposeClockResetEnable (macT <^> i)

macT s (x,y) = (s',o)
  where
    s' = s + (x * y)
    o  = s
