-- | Regression test for the keep-all-casts reimplementation of #1064: the
-- worker/wrapper split in 'inlineCastNonRep'.
--
-- Two ingredients:
--
-- * @accum@ is a value-recursive signal reached through specialized
--   workers, like the Fib feedback tests. The wrapper must *not* fire
--   here: it would create global mutual recursion (@f <-> f_cast@), which
--   the pipeline cannot handle. The provenance-based cycle check
--   ('originReachableFrom') detects this even though the back-reference
--   goes through a pre-specialization clone with a different unique.
--
-- * @stepper@ is a (non-recursive) binder whose casted applications appear
--   at more use sites than the inline limit (20); a Vec 24 of uses. With
--   per-call-site inlining this tripped the limit ("already inlined 20
--   times") and left casts of un-normalized bodies behind; with the
--   wrapper it normalizes cleanly.
module T1064F where

import Clash.Explicit.Prelude

accum :: Clock System -> Reset System -> Enable System -> Signal System (Unsigned 8)
accum clk rst en = s
 where
  s = register clk rst en 0 (s + 1)

stepper :: Signal System Bool -> Signal System Bool -> Signal System Bool
stepper a b = mux a b (not <$> b)

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Vec 24 (Signal System Bool) ->
  Vec 24 (Signal System Bool)
topEntity clk rst en xs = map (stepper msb) xs
 where
  msb = (accum clk rst en) .==. pure 1
