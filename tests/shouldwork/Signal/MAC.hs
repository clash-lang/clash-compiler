module MAC where

import CLaSH.Prelude

topEntity :: (Signal Integer, Signal Integer)
          -> Signal Integer
topEntity = macT <^> 0

macT s (x,y) = (s',o)
  where
    s' = s + (x * y)
    o  = s
