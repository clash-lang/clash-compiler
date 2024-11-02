module T2831 where

import Clash.Prelude

f :: forall n. SNat n -> Unsigned 4
f n@SNat = case compareSNat n (SNat @15) of
  SNatLE -> at @n @(16 - n - 1) SNat vec
  SNatGT -> 0
 where
  vec :: Vec 16 (Unsigned 4)
  vec = repeat 1

topEntity :: Unsigned 4
topEntity = f d17
