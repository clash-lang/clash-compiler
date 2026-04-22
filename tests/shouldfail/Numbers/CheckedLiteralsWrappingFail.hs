{-# LANGUAGE DataKinds #-}

module CheckedLiteralsWrappingFail where

import Clash.Num.Wrapping (Wrapping)
import Clash.Prelude

bad :: Wrapping (Unsigned 2)
bad = 4

topEntity :: Signal System Bit
topEntity = 0
