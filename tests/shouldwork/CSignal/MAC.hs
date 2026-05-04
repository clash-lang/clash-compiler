module MAC where

import Clash.Explicit.Prelude

createDomain vSystem{vName="DomA101", vPeriod=101}

topEntity
  :: Clock DomA101
  -> Reset DomA101
  -> Enable DomA101
  -> (Signal DomA101 Int, Signal DomA101 Int)
  -> Signal DomA101 Int
topEntity clk rst en = mealyB clk rst en macT 0

macT s (x,y) = (s', o)
  where
    s' = s + (x * y)
    o  = s
