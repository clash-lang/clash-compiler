module MAC where

import Clash.Explicit.Prelude

createDomain vSystem{vTag="DomA101", vPeriod=101}

topEntity
  :: Clock DomA101
  -> Reset DomA101
  -> Enable DomA101
  -> (Signal DomA101 Integer, Signal DomA101 Integer)
  -> Signal DomA101 Integer
topEntity clk rst en = mealyB clk rst en macT 0

macT s (x,y) = (s', o)
  where
    s' = s + (x * y)
    o  = s
