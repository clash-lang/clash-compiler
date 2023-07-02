{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module DFold2 where

import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Proxy

topEntity :: Vec 4 (Vec 4 (Unsigned 8)) -> Vec 4 (Vec 4 (Unsigned 8))
topEntity = smap (flip rotateRightS)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure (replicate d4 (0 :> 1 :> 2 :> 3 :> Nil))
    expectedOutput = outputVerifier' clk rst (((0:>1:>2:>3:>Nil):>
                                      (3:>0:>1:>2:>Nil):>
                                      (2:>3:>0:>1:>Nil):>
                                      (1:>2:>3:>0:>Nil):>Nil):>Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
