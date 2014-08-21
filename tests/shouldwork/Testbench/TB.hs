module TB where

import CLaSH.Prelude

type Inp   = (Signed 4,Outp)
type Outp  = (Maybe (Signed 8,Bool),Bit)

topEntity :: SWrapped Inp -> SWrapped Outp
topEntity = transfer <^> initS

transfer s i = (i,o)
  where
    o = snd s

initS = (0,(Nothing,low))

testInput :: Signal Inp
testInput = stimuliGenerator $(v ([ (1,(Just (4,True), low))
                                  , (3,(Nothing, high))
                                  ]::[(Signed 4,(Maybe (Signed 8,Bool),Bit))]))

expectedOutput :: Signal Outp -> Signal Bool
expectedOutput = outputVerifier $(v ([(Nothing,low)
                                      ,(Just (4,False), low)
                                      ]::[(Maybe (Signed 8,Bool),Bit)]))

