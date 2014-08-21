module MAC where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

type Clk100 = Clk "clk" 100

clk100 :: SClock Clk100
clk100 = sclock

topEntity :: (CSignal Clk100 Integer, CSignal Clk100 Integer)
          -> (CSignal Clk100 Bool, CSignal Clk100 Integer)
topEntity = sync clk100 macT 0

macT s (x,y) = (s',Just o)
  where
    s' = s + (x * y)
    o  = s
