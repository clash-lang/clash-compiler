module MAC where

import Clash.Prelude

topEntity
  :: Int
  -> Clock System
  -> Reset System
  -> Enable System
  -> (Signal System Int, Signal System Int)
  -> Signal System Int
topEntity i = exposeClockResetEnable (macT <^> i)

macT s (x,y) = (s',o)
  where
    s' = s + (x * y)
    o  = s
