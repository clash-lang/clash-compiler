module VScan where

import CLaSH.Prelude

topEntity :: Vec 4 Int -> (Vec 4 Int,Vec 4 Int)
topEntity vs = (vscanl1 (+) vs, vscanr1 (+) vs)

