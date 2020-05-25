module SignedZero where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity
  :: Clock  System
  -> Reset  System
  -> Signal System (Signed 16)
  -> Signal System ( Signed 17
                   , Signed 17
                   , Signed 17
                   , Signed 17
                   , Signed 16
                   , Signed 16
                   )
topEntity clk rst =
  fmap (\n ->
    ( add (n :: Signed 16) (0 :: Signed 0)
    , add (0 :: Signed 0)  (n :: Signed 16)
    , sub (n :: Signed 16) (0 :: Signed 0)
    , sub (0 :: Signed 0)  (n :: Signed 16)
    , mul (n :: Signed 16) (0 :: Signed 0)
    , mul (0 :: Signed 0)  (n :: Signed 16)
    ))
{-# NOINLINE topEntity #-}

testBench :: Signal System Bool
testBench = done
  where
    n              = 22 :: Signed 17
    n1             = 22 :: Signed 16
    expectedOutput = outputVerifier' clk rst ((n, n, n, -n, 0, 0) :> Nil)
    done           = expectedOutput (topEntity clk rst (pure n1))
    clk            = tbSystemClockGen (not <$> done)
    rst            = systemResetGen
{-# NOINLINE testBench #-}
