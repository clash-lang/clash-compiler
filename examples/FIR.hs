{-# LANGUAGE DataKinds #-}
module FIR where

import CLaSH.Prelude

dotp as bs = vfoldl (+) 0 (vzipWith (*) as bs)

fir hs pxs x = (x +>> pxs, dotp pxs hs)

topEntity :: Vec 4 (Signed 16)
          -> Signed 16
          -> (Vec 4 (Signed 16),Signed 16)
topEntity pxs x = fir (2 :> 3 :> (-2) :> 8 :> Nil) pxs x

