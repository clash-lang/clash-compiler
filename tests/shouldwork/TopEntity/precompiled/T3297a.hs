{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module T3297a where

import Clash.Prelude
import Clash.Explicit.Testbench

-- | The top entity we select with @-main-is topEntity@.
topEntity :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
topEntity = fmap (+1)
{-# OPAQUE topEntity #-}

-- | A design under test used only by 'testBench', deliberately kept out of
-- 'topEntity's closure.
dut :: Signal System (Unsigned 8) -> Signal System (Unsigned 8)
dut = fmap (*2)
{-# OPAQUE dut #-}

-- | Magically named test bench that exercises 'dut' (not 'topEntity'), so it
-- lies entirely outside 'topEntity's closure.
--
-- This module is compiled into the @clash-testsuite@ library, so Clash loads it
-- from its external interface file (via 'loadExternalModule'). Before #3297 was
-- fixed, compiling with @-main-is topEntity@ pruned everything outside
-- 'topEntity's closure before loading, yet still selected the magically named
-- 'testBench' as a top entity. Its binding was therefore never loaded, and
-- Clash crashed with "No top entity called '...testBench...' found".
testBench :: Signal System Bool
testBench = done
 where
  testInput      = stimuliGenerator clk rst (1 :> 2 :> 3 :> Nil)
  expectedOutput = outputVerifier' clk rst (2 :> 4 :> 6 :> Nil)
  done           = expectedOutput (dut testInput)
  clk            = tbSystemClockGen (not <$> done)
  rst            = systemResetGen
{-# OPAQUE testBench #-}
