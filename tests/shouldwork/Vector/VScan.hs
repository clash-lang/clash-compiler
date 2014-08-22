module VScan where

import CLaSH.Prelude

topEntity :: Vec 4 Int -> (Vec 4 Int,Vec 4 Int)
topEntity vs = (sscanl (+) 0 vs, sscanr (+) 0 vs)

