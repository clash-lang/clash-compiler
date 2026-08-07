-- | Regression test for the keep-all-casts reimplementation of #1064:
-- specializing on a (cast) argument of a function whose result type is a
-- bare type variable instantiated at a function type. 'specialize'' used to
-- abstract the type arguments over fresh type variables, after which the
-- excess term arguments were consumed by an uninstantiated type variable,
-- crashing type inference ("applied an argument to something with the
-- non-function type").
--
-- Mimics the shape of clash-cores' @vioProbe#@ applied through its result
-- type variable, with a constant signal argument (which the keep-all-casts
-- translation turns into a cast, triggering @argCastSpec@ before @typeSpec@
-- has monomorphized the application).
module T1064A where

import Clash.Prelude

mkF :: forall a. SNat 3 -> a -> a
mkF SNat x = x
{-# OPAQUE mkF #-}

topEntity :: Signal System Bool -> Signal System Bool
topEntity i =
  mkF @(Signal System Bool -> Signal System Bool)
    (SNat @3)
    (\x -> x .&&. i)
    (pure True)
