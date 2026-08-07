-- | Regression test for the keep-all-casts reimplementation of #1064: a
-- top entity returning a newtype of a function type whose body is a
-- case-of-tuple with local state. After 'etaExpansionTL' eta-expands
-- through the newtype, the resulting redexes
--
-- > (case ds of (clk, rst) -> (letrec .. in \x -> e) |> co) arg
--
-- must be dissolved by running the propagation and cast transformations to
-- a fixpoint; leftovers with inner lambdas make ANF's binder collection
-- hoist bindings across a lambda, introducing free variables.
module T1064E where

import Clash.Explicit.Prelude

newtype Wrap a b = Wrap ((a, b) -> (b, a))

topEntity :: (Clock System, Reset System) -> Wrap (Signal System Bool) (Signal System Bool)
topEntity (clk, rst) =
  let cnt :: Signal System (Unsigned 8)
      cnt = register clk rst enableGen 0 (cnt + 1)
  in Wrap (\(x, y) -> (mux (cnt .==. 0) x y, y))
