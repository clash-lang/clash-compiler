{-# LANGUAGE LambdaCase #-}

module ResetSynchronizerSync where

import Clash.Prelude
import Data.Proxy
import ResetSynchronizer

topEntity :: Clock XilinxSystem -> Reset XilinxSystem -> Signal XilinxSystem ResetCount
topEntity = polyTopEntity @XilinxSystem

testBench :: Signal System Bool
testBench = fst (polyTestBench (Proxy @XilinxSystem))
