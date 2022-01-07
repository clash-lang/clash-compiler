module RWMulti35 where

import Clash.Prelude

import RWMulti

topEntity
  :: Clock P30
  -> Clock P50
  -> Signal P30 (Unsigned 4)
  -> Signal P50 (Unsigned 4)
topEntity = ram
{-# NOINLINE topEntity #-}

testBench
  :: Signal P50 Bool
testBench =
  tb topEntity $(listToVecTH
                 $ sampleN 20 $ tbOutput (ram @P30 @P50) clockGen clockGen)
-- testBench = tb topEntity $(listToVecTH [0 :: Unsigned 4, 1, 3, 4, 6, 8, 9, 11, 13, 14])
{-# NOINLINE testBench #-}
