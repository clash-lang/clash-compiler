module Ram where

import CLaSH.Prelude
import qualified Data.List as L

zeroAt0 :: Signal (Unsigned 8,Unsigned 8) -> Signal (Unsigned 8,Unsigned 8)
zeroAt0 a = mux en a (bundle (0,0))
  where
    en = register False (signal True)

topEntity :: Signal (Unsigned 8) -> Signal (Unsigned 8,Unsigned 8)
topEntity rd = zeroAt0 dout
  where
    dout = asyncRamPow2 wr rd (signal True) (bundle (wr,wr))
    wr   = register 1 (wr + 1)

testInput :: Signal (Unsigned 8)
testInput = cnt
  where
    cnt = register 0 (cnt + 1)

expectedOutput :: Signal (Unsigned 8, Unsigned 8) -> Signal Bool
expectedOutput = outputVerifier $(v $ L.map (\x -> (x,x)) [0::Unsigned 8,1,2,3,4,5,6,7,8])
