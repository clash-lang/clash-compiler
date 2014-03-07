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

f b = Just (4,b)

testInput :: [Inp]
testInput = [ (1,(f True, L))
            , (3,(Nothing, H))
            ]

expectedOutput :: [Outp]
expectedOutput = [(Nothing,L)
                 ,(f False, L)
                 ]

