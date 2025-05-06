{-# LANGUAGE CPP #-}

module T2904 where

import Clash.Prelude
import Clash.Explicit.Testbench

topEntity :: Vec 3 (Unsigned 8)
topEntity = case imap f (5 :> 6 :> 7 :> Nil) of
  (Cons x xs) -> Cons maxBound xs
 where
  f :: Index 3 -> Unsigned 8 -> Unsigned 8
  f i x = case i of
    0 -> x + 1
    1 -> x + 2
    _ -> x + 3
{-# CLASH_OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  done = outputVerifier' clk rst ((maxBound :> 8 :> 10 :> Nil) :> Nil) (pure topEntity)
  clk  = tbSystemClockGen (not <$> done)
  rst  = systemResetGen
{-# CLASH_OPAQUE testBench #-}
