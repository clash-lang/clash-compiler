{-# LANGUAGE CPP #-}

module ExtendingNumZero where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock  System
  -> Reset  System
  -> Signal System (BitVector 16)
  -> Signal System ( BitVector 17
                   , BitVector 17
                   , BitVector 17
                   , BitVector 17
                   , BitVector 16
                   , BitVector 16
                   )
topEntity clk rst =
  fmap (\n ->
    ( add (n :: BitVector 16) (0 :: BitVector 0)
    , add (0 :: BitVector 0)  (n :: BitVector 16)
    , sub (n :: BitVector 16) (0 :: BitVector 0)
    , sub (0 :: BitVector 0)  (n :: BitVector 16)
    , mul (n :: BitVector 16) (0 :: BitVector 0)
    , mul (0 :: BitVector 0)  (n :: BitVector 16)
    ))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    n              = 22 :: BitVector 17
    n1             = 22 :: BitVector 16
    expectedOutput = outputVerifier' clk rst ((n, n, n, -n, 0, 0) :> Nil)
    done           = expectedOutput (topEntity clk rst (pure n1))
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
