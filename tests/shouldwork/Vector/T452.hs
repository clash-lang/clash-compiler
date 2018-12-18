module T452 where

import Clash.Prelude

topEntity
  :: Vec 4 (Unsigned 4)
  -> Vec 4 (Unsigned 5)
topEntity = map (add (3 :: Unsigned 4))
