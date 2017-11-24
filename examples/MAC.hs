module MAC where

import Clash.Prelude

ma acc (x,y) = acc + x * y

macT acc (x,y) = (acc',o)
  where
    acc' = ma acc (x,y)
    o    = acc

mac = macT `mealy` 0

topEntity
  :: SystemClockReset
  => Signal System (Signed 9,Signed 9)
  -> Signal System (Signed 9)
topEntity = mac
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = stimuliGenerator $(listToVecTH [(1,1) :: (Signed 9,Signed 9),(2,2),(3,3),(4,4)])
    expectedOutput = outputVerifier $(listToVecTH [0 :: Signed 9,1,5,14])
    done           = expectedOutput (topEntity testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
