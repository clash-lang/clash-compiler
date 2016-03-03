module MAC where

import CLaSH.Prelude

topEntity :: Integer
          -> (Signal Integer, Signal Integer)
          -> Signal Integer
topEntity i = macT <^> i

macT s (x,y) = (s',o)
  where
    s' = s + (x * y)
    o  = s
