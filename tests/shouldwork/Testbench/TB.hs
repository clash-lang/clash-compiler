module TB where

import CLaSH.Prelude

type Inp   = (Signed 4,Outp)
type Outp  = (Maybe (Signed 8,Bool),Bit)

topEntity :: SignalP Inp -> SignalP Outp
topEntity = transfer <^> initS

transfer s i = (i,o)
  where
    o = snd s

initS = (0,(Nothing,L))

testInput :: Signal Inp
testInput = stimuliGenerator $(v ([ (1,(Just (4,True), L))
                                  , (3,(Nothing, H))
                                  ]::[(Signed 4,(Maybe (Signed 8,Bool),Bit))]))

expectedOutput :: Signal Outp -> Signal Bool
expectedOutput = outputVerifier $(v ([(Nothing,L)
                                      ,(Just (4,False), L)
                                      ]::[(Maybe (Signed 8,Bool),Bit)]))

