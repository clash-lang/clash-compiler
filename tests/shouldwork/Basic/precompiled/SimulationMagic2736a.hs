{-# OPTIONS_GHC -O2 -fspec-constr #-}
module SimulationMagic2736a where

import Clash.Prelude

f0 :: Int
f0 | clashSimulation = 123
   | otherwise       = 456
