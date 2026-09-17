module T3414 where

import Prelude (Bool)

-- The invalid trailing command-line argument must be reported before this
-- module's type error, regardless of the HDL backend.
topEntity :: Bool
topEntity = ()
