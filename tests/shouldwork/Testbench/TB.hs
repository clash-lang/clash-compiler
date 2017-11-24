module TB where

import Clash.Prelude

type Inp   = (Signed 4,Outp)
type Outp  = (Maybe (Signed 8,Bool),Bit)

topEntity :: SystemClockReset => Signal System Inp -> Signal System Outp
topEntity = transfer `mealy` initS
{-# NOINLINE topEntity #-}

transfer s i = (i,o)
  where
    o = snd s

initS = (0,(Nothing,0))

testBench :: Signal System Bool
testBench = done'
  where
    testInput      = stimuliGenerator
                      $(listToVecTH ([ (1,(Just (4,True), 0))
                                     , (3,(Nothing, 1))
                                     ]::[(Signed 4,(Maybe (Signed 8,Bool),Bit))]))
    expectedOutput = outputVerifier
                      $(listToVecTH ([(Nothing,0)
                                     ,(Just (4,True), 0)
                                     ]::[(Maybe (Signed 8,Bool),Bit)]))
    done           = expectedOutput (topEntity testInput)
    done'          = withClockReset (tbSystemClockGen (not <$> done')) systemResetGen done
