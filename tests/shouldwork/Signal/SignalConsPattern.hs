-- | Pattern matching on the @(:-)@ constructor of 'Signal'. ANF left these
-- case-expressions in place but still let-bound the subterms of their
-- alternatives outside of them, out of scope of the pattern's variables.
module SignalConsPattern where

import Clash.Prelude
import Clash.Signal.Internal (Signal((:-)))

f :: Signal System (Signed 8) -> Signed 8
f (x :- _) = x * (x + 1)
{-# OPAQUE f #-}

g :: Signal System (Signed 8) -> Signed 8
g (x :- _) = x + 1
{-# OPAQUE g #-}

topEntity :: Signal System (Signed 8) -> (Signed 8, Signed 8)
topEntity x = (f x, g x)
