module MAC where

import CLaSH.Prelude
import CLaSH.Prelude.Explicit

type Clk100 = Clk "clk" 101

clk100 :: SClock Clk100
clk100 = sclock

topEntity :: (Signal' Clk100 Integer, Signal' Clk100 Integer)
          -> Signal' Clk100 Integer
topEntity = mealyB' clk100 macT 0

macT s (x,y) = (s', o)
  where
    s' = s + (x * y)
    o  = s
