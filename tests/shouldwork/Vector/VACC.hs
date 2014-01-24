{-# LANGUAGE DataKinds #-}
module VACC where

import CLaSH.Prelude

d4 = snat :: SNat 4
d1 = snat :: SNat 1
d2 = snat :: SNat 2


topEntity :: Vec 8 Bit -> Vec 16 Bit
topEntity x = o <++> p <++> q <++> k <++> l
  where
    y = vtake d4 x
    z = vdrop d4 x
    o = vtakeI y :: Vec 2 Bit
    p = vdropI z :: Vec 2 Bit
    q = vselect d1 d2 d4 x
    k = vgenerateI (xor L) H :: Vec 4 Bit
    l = viterateI  (xor H) L :: Vec 4 Bit
