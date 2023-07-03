{-# LANGUAGE CPP #-}

module UnsignedZero where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock  System
  -> Reset  System
  -> Signal System (Unsigned 16)
  -> Signal System ( Unsigned 17
                   , Unsigned 17
                   , Unsigned 17
                   , Unsigned 16
                   , Unsigned 16
                   )
topEntity clk rst =
  fmap (\n ->
    ( add (n :: Unsigned 16) (0 :: Unsigned 0)
    , add (0 :: Unsigned 0)  (n :: Unsigned 16)
    , sub (n :: Unsigned 16) (0 :: Unsigned 0)
    , mul (n :: Unsigned 16) (0 :: Unsigned 0)
    , mul (0 :: Unsigned 0)  (n :: Unsigned 16)
    ))
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    n              = 22 :: Unsigned 17
    n1             = 22 :: Unsigned 16
    expectedOutput = outputVerifier' clk rst ((n, n, n, 0, 0) :> Nil)
    done           = expectedOutput (topEntity clk rst (pure n1))
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE testBench #-}
