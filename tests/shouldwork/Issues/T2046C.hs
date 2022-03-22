module T2046C where

import Clash.Prelude
import T2046BType

inBit :: (Enum a) => a -> Bool
inBit x = case fromEnum x of
        0 -> True
        1 -> True
        _ -> False

topEntity :: Bool
topEntity =
  let ((a,b),(c,d),(e,f),(g,h),i) = ((0,1),(0,1),(0,1),(0,1),0) :: T2046B
   in and [ inBit a, inBit b
          , inBit c, inBit d
          , inBit e, inBit f
          , inBit g, inBit h
          , inBit i
          ]

