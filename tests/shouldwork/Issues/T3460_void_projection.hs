-- | A case whose subject projects the zero-width 'CallStack' field out of @Q@.
-- Rendering that projection used to reference an undeclared @c$sel@ (Verilog)
-- or crash the VHDL backend.
module T3460_void_projection where

import Clash.Prelude
import Clash.Explicit.Testbench
import GHC.Stack.Types (CallStack(..))

-- The Bool field makes Q non-void. If CallStack were Q's only field, q itself
-- would be void and mkProjection would take its (already working) void-subject
-- path.
data Q = Q { qb :: Bool, qcs :: CallStack }

-- OPAQUE, so Clash can't see that the case below always picks EmptyCallStack.
{-# OPAQUE mkQ #-}
mkQ :: Bool -> Q
mkQ b = Q b EmptyCallStack

-- g must consume cs and be OPAQUE, otherwise cs is dead and the case is removed.
{-# OPAQUE g #-}
g :: CallStack -> Bool -> Bool
g _ b = not b

{-# OPAQUE topEntity #-}
topEntity :: Bool -> Bool
topEntity b = g cs b
 where
  q = mkQ b
  -- The alternatives differ, so Clash keeps the case and its subject stays a
  -- projection. (GHC's own FreezeCallStack check is collapsed by Clash, as both
  -- of its alternatives return the subject.)
  cs = case qcs q of
    FreezeCallStack{} -> qcs q
    _ -> EmptyCallStack

testBench :: Signal System Bool
testBench = done
 where
  testInput = stimuliGenerator clk rst (True :> False :> Nil)
  expectedOutput = outputVerifier' clk rst (False :> True :> Nil)
  done = expectedOutput (fmap topEntity testInput)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
