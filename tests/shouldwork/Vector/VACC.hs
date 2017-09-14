{-# LANGUAGE DataKinds #-}
module VACC where

import Clash.Prelude

topEntity :: Vec 8 Bit -> Vec 16 Bit
topEntity x = o ++ p ++ q ++ k ++ l
  where
    y = take d4 x
    z = drop d4 x
    o = takeI y :: Vec 2 Bit
    p = dropI z :: Vec 2 Bit
    q = select d1 d2 d4 x
    k = generateI (xor low) high :: Vec 4 Bit
    l = iterateI  (xor high) low :: Vec 4 Bit
