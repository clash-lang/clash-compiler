module MAC where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

topEntity :: (CSignal 100 Integer, CSignal 100 Integer)
          -> CSignal 100 Integer
topEntity = sync clk100 macT 0

clk100 = Clock d100

macT s (x,y) = (s',o)
  where
    s' = s + (x * y)
    o  = s
