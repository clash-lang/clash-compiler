-- | Regression test for commit
-- https://github.com/clash-lang/clash-compiler/commit/92dcfe581cc204283074e409a58dc6482d219c43
-- ("Evaluator: create heap-bindings for free variables").
--
-- The evaluator pushes an @Apply x@ frame for every argument @x@ of an
-- application whose head is not a data constructor or primitive. If the
-- argument is a variable that is not on the heap (i.e., it is free, like the
-- lambda-bound @x@ of 'topEntity'), the evaluator must still create a heap
-- binding for it. Otherwise, when evaluation gets stuck (here: on the
-- lambda-bound @b@) and the stack is unwound, the @Apply x@ frame refers to a
-- variable that cannot be found on the heap, and Clash crashes with
-- "Clash.Core.Evaluator.unwindStack".
module FreeVarHeapBinding where

import Clash.Prelude

-- OPAQUE, so 'f' is unfolded by the evaluator rather than inlined by GHC
f :: Bool -> BitVector 8 -> BitVector 8
f b = if b then (+1) else (*2)
{-# OPAQUE f #-}

topEntity :: Bool -> BitVector 8 -> BitVector 8
-- The case subject is a primitive ('eq#') applied to @f b x@, so caseCon asks
-- the evaluator for its WHNF.
topEntity b x = if f b x == 3 then 1 else 0
