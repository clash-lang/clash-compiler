module Ram where

import CLaSH.Prelude

zeroAt0 :: Signal (Unsigned 8) -> Signal (Unsigned 8)
zeroAt0 a = mux en a 0
  where
    en = register False (signal True)

topEntity :: Signal (Unsigned 8) -> Signal (Unsigned 8)
topEntity rd = zeroAt0 (unpack <$> dout)
  where
    dout = asyncRamPow2 wr rd (signal True) wr
    wr   = register 1 (wr + 1)

testInput :: Signal (Unsigned 8)
testInput = cnt
  where
    cnt = register 0 (cnt + 1)

expectedOutput :: Signal (Unsigned 8) -> Signal Bool
expectedOutput = outputVerifier $(v [0::Unsigned 8,1,2,3,4,5,6,7,8])
