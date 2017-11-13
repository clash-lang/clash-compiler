module InOutInCompositeTypeInResult where

import Clash.Prelude (Int,undefined)
import Clash.Signal (InOut,System,Signal)


topEntity :: Signal System Int -> (Signal System Int, InOut System Int)
topEntity _ = undefined
