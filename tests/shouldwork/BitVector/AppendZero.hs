{-# LANGUAGE CPP #-}

module AppendZero where

import Clash.Prelude
import Clash.Explicit.Testbench
import Clash.Sized.BitVector ((++#))

topEntity
  :: Clock  System
  -> Reset  System
  -> Signal System ( BitVector 16
                   , BitVector 16
                   , BitVector 32
                   )
topEntity clk rst =
  pure
    ( (22 :: BitVector 16) ++# (0  :: BitVector 0)
    , (0  :: BitVector 0)  ++# (22 :: BitVector 16)
    , (22 :: BitVector 16) ++# (22 :: BitVector 16)
    )
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    expectedOutput = outputVerifier' clk rst ((22, 22, 1441814) :> Nil)
    done           = expectedOutput (topEntity clk rst)
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
