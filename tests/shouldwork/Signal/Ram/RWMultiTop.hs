module RWMultiTop where

import Clash.Prelude

import RWMulti

topEntity35
  :: Clock P30
  -> Clock P50
  -> Signal P30 (Unsigned 4)
  -> Signal P50 (Unsigned 4)
topEntity35 = ram
{-# NOINLINE topEntity35 #-}
{-# ANN topEntity35 (defSyn "topEntity35") #-}

testBench35
  :: Signal P50 Bool
testBench35 =
  tb topEntity35 $(listToVecTH $
                   sampleN 20 $ tbOutput (ram @P30 @P50) clockGen clockGen)
-- testBench35 =
--   tb topEntity35 $(listToVecTH [0 :: Unsigned 4, 1, 3, 4, 6, 8, 9, 11, 13, 14])
{-# NOINLINE testBench35 #-}
{-# ANN testBench35 (TestBench 'topEntity35) #-}

topEntity53
  :: Clock P50
  -> Clock P30
  -> Signal P50 (Unsigned 4)
  -> Signal P30 (Unsigned 4)
topEntity53 = ram
{-# NOINLINE topEntity53 #-}
{-# ANN topEntity53 (defSyn "topEntity53") #-}

testBench53
  :: Signal P30 Bool
testBench53 =
  tb topEntity53 $(listToVecTH $
                   sampleN 20 $ tbOutput (ram @P50 @P30) clockGen clockGen)
-- testBench53 =
--   tb topEntity53 $(listToVecTH [0 :: Unsigned 4, 0, 1, 1, 2, 2, 3, 4, 4, 5, 5,
--                                 6, 7, 7, 8, 8])
{-# NOINLINE testBench53 #-}
{-# ANN testBench53 (TestBench 'topEntity53) #-}
