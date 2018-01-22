module BiSignalInResult where

import Clash.Signal
import Clash.Prelude

topEntity :: BiSignalIn Undefined System Int
          -> BiSignalIn Undefined System Int
topEntity = id
