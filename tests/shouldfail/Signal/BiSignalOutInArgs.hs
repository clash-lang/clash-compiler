module BiSignalInResult where

import Clash.Signal
import Clash.Prelude

topEntity :: BiSignalOut Undefined System Int
          -> Signal System Int
topEntity _ = 7
