{-# LANGUAGE DataKinds #-}

-- | Annotated arguments of functions that are polymorphic in the annotated
-- type. Clash used to leave the type variable under the 'Annotate' unsubstituted
-- when it specialized these functions, failing with "Cannot reduce to an
-- integer: n" and "Cannot translate domain tag: dom".
module AnnotatePolymorphic where

import Clash.Explicit.Prelude
import Clash.Annotations.SynthesisAttributes

f :: KnownNat n => BitVector n `Annotate` 'StringAttr "foo" "bar" -> BitVector n
f x = x + 1
{-# OPAQUE f #-}

g ::
  KnownDomain dom =>
  Clock dom `Annotate` 'StringAttr "foo" "bar" ->
  Signal dom Bit ->
  Signal dom Bit
g clk = delay clk enableGen 0
{-# OPAQUE g #-}

topEntity ::
  Clock System ->
  Signal System Bit ->
  BitVector 8 ->
  (Signal System Bit, BitVector 8)
topEntity clk x y = (g clk x, f y)
