module HOCon where

import CLaSH.Prelude

topEntity :: Vec 8 Int -> Vec 8 (Maybe Int)
topEntity = map Just
