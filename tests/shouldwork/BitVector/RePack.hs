module RePack where

import CLaSH.Prelude

topEntity :: (Unsigned 1,Unsigned 1)
topEntity = (unpack (pack True), unpack (pack False))
