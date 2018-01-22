module BiSignalInResult where

import Clash.Signal
import Clash.Prelude

topEntity :: BiSignalIn Undefined System Int
          -> (BiSignalIn Undefined System Int, Signal System Int)
topEntity a = (a, 7)
