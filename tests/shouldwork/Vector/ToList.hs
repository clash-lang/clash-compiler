{-# LANGUAGE CPP #-}

module ToList where

import Clash.Prelude
import Clash.Explicit.Testbench
import qualified Data.List as L

topEntity :: Vec 3 Int -> Int
topEntity xs = L.foldr (+) 0 (toList xs)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = pure (1 :> 2 :> 3 :> Nil)
    expectedOutput = outputVerifier' clk rst (6 :> Nil)
    done           = expectedOutput (topEntity <$> testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
