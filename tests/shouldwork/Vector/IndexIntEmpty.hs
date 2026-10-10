-- | Indexing empty vectors. Clash used to crash with "extractElems must be
-- called with maxN >= 1" when reducing 'index_int', which it does with
-- @-fclash-compile-ultra@.
module IndexIntEmpty where

import Clash.Prelude

topEntity :: Vec 2 (Vec 0 (Unsigned 8)) -> Index 4 -> Vec 2 (Unsigned 8)
topEntity xss i = map (!! i) xss
