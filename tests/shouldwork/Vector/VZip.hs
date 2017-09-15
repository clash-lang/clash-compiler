module VZip where

import Clash.Prelude

topEntity :: Vec 8 (Int,Int) -> Vec 8 (Int,Int)
topEntity xs = zip ys zs
  where
    (ys,zs) = unzip xs
