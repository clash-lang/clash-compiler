module VZip where

import CLaSH.Prelude

topEntity :: Vec 8 (Int,Int) -> Vec 8 (Int,Int)
topEntity xs = zip ys zs
  where
    (ys,zs) = unzip xs
