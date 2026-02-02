{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module SrcLocTest where

import Clash.Prelude
import SrcLocCounter

topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Bool, Unsigned 8)
topEntity = exposeClockResetEnable @System counter

