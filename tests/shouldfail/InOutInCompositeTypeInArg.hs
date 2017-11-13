module InOutInCompositeTypeInArg where

import Clash.Prelude (Int,undefined)
import Clash.Signal (InOut,System,Signal)


topEntity :: (Signal System Int, InOut System Int) -> Signal System Int
topEntity _ = undefined

