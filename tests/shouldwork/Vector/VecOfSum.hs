module VecOfSum where

import Clash.Prelude

data Sum = Empty
         | Half Integer Integer Integer Integer
         | Full (Unsigned 4) (Signed 9) Bool Integer Bit (Maybe (Bool,Signed 3)) (Unsigned 3, Signed 4)

topEntity :: Vec 3 Sum -> Vec 5 Sum
topEntity xs = (Full 4 12 True 12 high (Just (False, 1)) (2,7)) :> Empty :> xs
