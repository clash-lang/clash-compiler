-- | Regression test for the keep-all-casts reimplementation of #1064: a
-- record update under 'fmap' inside a binder with a non-representable
-- result type. Such binders cannot be normalized; when the inline limit is
-- struck, flattening inlines their raw (non-ANF) bodies, and netlist
-- generation encounters a single-alternative case whose right-hand side is
-- a casted constructor application:
--
-- > case s |> Signal dom R ~ R of
-- >   R a b c -> (R a x c) |> R ~ Signal dom R
--
-- Netlist generation must compile this by substituting the pattern
-- variables with projections of the subject instead of throwing
-- "RHS of case-projection is not a variable".
module T1064C where

import Clash.Prelude

data R = R
  { f1 :: BitVector 8
  , f2 :: Bool
  , f3 :: Bool
  }
  deriving (Generic, NFDataX, BitPack)

-- The tuple result contains a function, making the result type
-- non-representable: 'updR' is not normalized as a separate binder.
updR :: Signal System R -> Signal System Bool -> (Signal System R, Bool -> Bool)
updR r b = ((\r' b' -> r' { f2 = b' }) <$> r <*> b, not)

use :: Signal System R -> Signal System Bool -> Signal System Bool
use r b = let (r', g) = updR r b in fmap (g . f2) r'

-- More use sites than the inline limit (20), so some casts of the
-- un-normalized 'updR' survive the main normalization loop and are only
-- inlined during flattening.
topEntity :: Signal System R -> Vec 24 (Signal System Bool) -> Vec 24 (Signal System Bool)
topEntity r = map (use r)
