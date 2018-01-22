module BiSignalInResult where

import Clash.Signal
import Clash.Prelude

topEntity :: (Signal System Int, BiSignalOut Undefined System Int)
          -> Signal System Int
topEntity (_, _) = 7
