-- | Regression test for the keep-all-casts reimplementation of #1064:
-- dictionary evidence produced by the ghc-typelits-knownnat plugin comes
-- with casts like @d |> SNatKn "*" ~ KnownNat (Div (n + 7) 8 * 8)@ that are
-- refl under the cast-equality oracle. 'argCastSpec' must not specialize on
-- them (it fires at the App node before 'elimCastCast' can drop the cast);
-- doing so pollutes the specialization cache with cast-decorated variants
-- of otherwise identical dictionaries and can hit the specialization limit
-- on the subtraction evidence @$w$cnatSing2@.
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module T1064D where

import Clash.Prelude

-- Byte-align: resizes a BitVector to the next multiple of 8. The
-- @KnownNat (Div (n + 7) 8 * 8)@ evidence is derived by the
-- ghc-typelits-knownnat plugin, producing the @SNatKn ~ KnownNat@ casts
-- this test is about.
byteAlign ::
  forall n.
  KnownNat n =>
  BitVector n ->
  BitVector (Div (n + 7) 8 * 8)
byteAlign = resize
{-# NOINLINE byteAlign #-}

topEntity ::
  (BitVector 15, BitVector 23, BitVector 8) ->
  (BitVector 16, BitVector 24, BitVector 8)
topEntity (a, b, c) = (byteAlign a, byteAlign b, byteAlign c)
