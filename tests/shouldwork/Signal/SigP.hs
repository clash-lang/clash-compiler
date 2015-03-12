module SigP where

import CLaSH.Prelude

topEntity :: Signal (Bool, Bool) -> (Signal Bool, Signal Bool)
topEntity = unbundle . register (False,False)
