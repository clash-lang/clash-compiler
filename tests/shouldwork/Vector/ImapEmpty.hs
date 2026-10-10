-- | 'imap' on empty vectors. Clash used to crash with "extractElems must be
-- called with maxN >= 1" when reducing it.
module ImapEmpty where

import Clash.Prelude

topEntity :: Vec 2 (Vec 0 (Unsigned 8)) -> Vec 2 (Vec 0 (Unsigned 8))
topEntity = map (imap (\_ x -> x + 1))
