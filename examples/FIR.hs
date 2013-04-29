{-# LANGUAGE DataKinds #-}
module FIR where

import CLaSH.Prelude

dotp as bs = vfoldl (+) 0 (vzipWith (*) as bs)

fir :: Vec 4 (Sync (Signed 16))
          -> Sync (Signed 16)
          -> Sync (Signed 16)
fir hs x = y
  where
    y  = dotp hs xs
    xs = rememberN x

topEntity :: Sync (Signed 16) -> Sync (Signed 16)
topEntity = fir (0 :> 1 :> 2 :> 3 :> Nil)
