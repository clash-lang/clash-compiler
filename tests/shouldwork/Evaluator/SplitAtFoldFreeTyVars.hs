-- | Regression test for commit
-- https://github.com/clash-lang/clash-compiler/commit/3b571c052d09e75a7573d7b2317e511fbea1b491
-- ("Stop Evaluator introducing free variables").
--
-- The compile-time evaluation rules for 'splitAt' and 'fold' (through
-- @fold_split@) used to take the result tuple type from the type of the
-- primitive itself, instead of from the type of the primitive applied to its
-- type arguments. The resulting terms mentioned the primitive's own type
-- variables (@m@, @n@, @a@), which are free. With -fclash-debug-invariants
-- (which the testsuite enables) this is reported as caseCon introducing free
-- variables.
module SplitAtFoldFreeTyVars where

import Clash.Prelude

-- Strict in its first argument, lazy in its second, so that 'fold' leaves
-- some of the results of @fold_split@ unevaluated in its WHNF.
f :: (Int, Int) -> (Int, Int) -> (Int, Int)
f x y = case x of (p, q) -> (p, q + snd y)

topEntity :: Int -> Int -> Int -> Int -> (Vec 1 Int, Int)
topEntity a b c d =
  -- The case subjects of 'fst' and 'snd' are applied primitives on a known
  -- vector spine, so caseCon evaluates them.
  ( fst (splitAt d1 (a :> b :> Nil))
  , snd (fold f ((a, a) :> (b, b) :> (c, c) :> (d, d) :> Nil))
  )
