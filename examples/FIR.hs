{-# LANGUAGE DataKinds #-}
module FIR where

import CLaSH.Prelude

dotp as bs = vfoldl (+) 0 (vzipWith (*) as bs)

topEntity :: Vec 4 (Signed 16)
          -> Vec 4 (Signed 16)
          -> Signed 16
topEntity as bs = dotp as bs
