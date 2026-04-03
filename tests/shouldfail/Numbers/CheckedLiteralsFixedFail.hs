{-# LANGUAGE DataKinds #-}

module CheckedLiteralsFixedFail where

import Clash.Prelude

bad :: UFixed 0 1
bad = 0.75

topEntity :: Signal System Bit
topEntity = 0
