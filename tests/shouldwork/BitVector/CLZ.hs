{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module CLZ where

import GHC.Prim
import GHC.Types
import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Word -> Word -> Word
topEntity (W# w1) (W# w2) = W# (clz# (or# w1 w2))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst (61 :> Nil)
    done           = expectedOutput (topEntity <$> pure 3 <*> pure 5)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
