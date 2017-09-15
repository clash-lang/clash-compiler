module MAC where

import Clash.Explicit.Prelude

type DomA101 = Dom "A" 101

topEntity
  :: Clock DomA101 Source
  -> Reset DomA101 Asynchronous
  -> (Signal DomA101 Integer, Signal DomA101 Integer)
  -> Signal DomA101 Integer
topEntity clk rst = mealyB clk rst macT 0

macT s (x,y) = (s', o)
  where
    s' = s + (x * y)
    o  = s
