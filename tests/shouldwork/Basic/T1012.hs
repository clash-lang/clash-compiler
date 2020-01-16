module T1012 where

import Clash.Prelude

f :: BitVector 5 -> Maybe (Bit -> Bit -> Bit)
f i = case slice d1 d0 i of
    2 -> op
    3 -> op
    _ -> Nothing
  where op :: Maybe (Bit -> Bit -> Bit)
        op = case (slice d4 d2 i, b) of
            (0, 0) -> Just (+)
            _ -> Nothing
          where b :: Bit
                b = case i ! 0 of
                    0 -> 0
                    1 -> 0

topEntity :: BitVector 5 -> Bit -> Bit -> Maybe Bit
topEntity = (sequenceA .) . sequenceA . f
