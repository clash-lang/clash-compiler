{-# LANGUAGE CPP #-}

module ReplaceInt where

import Clash.Prelude
import Clash.Sized.Vector (replace)
import Clash.Explicit.Testbench

replace_int
  :: KnownNat n
  => Vec n a
  -> Int
  -> a
  -> Vec n a
replace_int v i a = replace i a v
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE replace_int #-}


topEntity
  :: SystemClockResetEnable
  => Signal System Int
  -> Signal System (Vec 5 Char)
topEntity i = a
  where
    a =
      register
        (replace_int (repeat 'a') 3 'c')
        (replace_int <$> a <*> i <*> pure 'x')
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    testInput      = stimuliGenerator clk rst (1 :> 2 :> 3 :> Nil)
    expectedOutput = outputVerifier' clk rst ( ('a' :> 'a' :> 'a' :> 'c' :> 'a' :> Nil)
                                           :> ('a' :> 'x' :> 'a' :> 'c' :> 'a' :> Nil)
                                           :> ('a' :> 'x' :> 'x' :> 'c' :> 'a' :> Nil)
                                           :> ('a' :> 'x' :> 'x' :> 'x' :> 'a' :> Nil)
                                           :> Nil)
    done           = expectedOutput (exposeClockResetEnable topEntity clk rst enableGen testInput)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
