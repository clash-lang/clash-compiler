{-# LANGUAGE TupleSections #-}
module PatHOCon where

import CLaSH.Prelude

topEntity :: Vec 8 (Unsigned 8) -> Vec 8 (Unsigned 4,Unsigned 8)
topEntity = map (4,)
