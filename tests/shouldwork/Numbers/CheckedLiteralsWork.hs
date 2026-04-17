{-# LANGUAGE DataKinds #-}

module CheckedLiteralsWork (topEntity) where

import Clash.Num.Erroring (Erroring)
import Clash.Num.Overflowing (Overflowing)
import Clash.Num.Saturating (Saturating)
import Clash.Num.Wrapping (Wrapping)
import Clash.Num.Zeroing (Zeroing)
import Clash.Prelude
import Clash.Signal.Delayed.Internal (DSignal)

goodBit :: Bit
goodBit = 1

goodBitVector :: BitVector 3
goodBitVector = 7

goodUnsigned :: Unsigned 2
goodUnsigned = 3

goodSigned :: Signed 2
goodSigned = -2

goodIndex :: Index 4
goodIndex = 3

goodUFixed :: UFixed 1 2
goodUFixed = 0.75

goodSFixed :: SFixed 2 2
goodSFixed = -1.25

goodSignal :: Signal System (Unsigned 2)
goodSignal = 3

goodDSignal :: DSignal System 0 (Signed 2)
goodDSignal = -2

goodWrapping :: Wrapping (Unsigned 2)
goodWrapping = 3

goodSaturating :: Saturating (Signed 2)
goodSaturating = -2

goodOverflowing :: Overflowing (Unsigned 2)
goodOverflowing = 3

goodZeroing :: Zeroing (Signed 2)
goodZeroing = -1

goodErroring :: Erroring (UFixed 1 2)
goodErroring = 0.75

topEntity :: Unsigned 2 -> Unsigned 2
topEntity x = x + 3
{-# OPAQUE topEntity #-}
