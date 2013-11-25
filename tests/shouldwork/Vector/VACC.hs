{-# LANGUAGE DataKinds #-}
module VACC where

import CLaSH.Prelude

d4 = SNat :: SNat 4
d1 = SNat :: SNat 1
d2 = SNat :: SNat 2


topEntity :: Vec 8 Bit -> Vec 16 Bit
topEntity x = o <++> p <++> q <++> k <++> l
  where
    y = vtake d4 x
    z = vdrop d4 x
    o = vtakeI y :: Vec 2 Bit
    p = vdropI z :: Vec 2 Bit
    q = vselect d1 d2 d4 x
    k = viterate d4 (xor L) H
    l = viterateI (xor H) L :: Vec 4 Bit
