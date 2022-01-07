module RWMulti53 where

import Clash.Prelude

import RWMulti

topEntity
  :: Clock P50
  -> Clock P30
  -> Signal P50 (Unsigned 4)
  -> Signal P30 (Unsigned 4)
topEntity = ram
{-# NOINLINE topEntity #-}

testBench
  :: Signal P30 Bool
testBench =
  tb topEntity $(listToVecTH
                 $ sampleN 20 $ tbOutput (ram @P50 @P30) clockGen clockGen)
-- testBench = tb topEntity $(listToVecTH [0 :: Unsigned 4, 0, 1, 1, 2, 2, 3, 4,
--                                         4, 5, 5, 6, 7, 7, 8, 8])
{-# NOINLINE testBench #-}
