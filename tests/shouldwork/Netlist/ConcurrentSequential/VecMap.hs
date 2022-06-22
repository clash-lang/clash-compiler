module VecMap where

import Clash.Explicit.Prelude
import Clash.Explicit.SimIO
import Clash.Explicit.Testbench

-- This test is still pretty simple (like Plus), but also requires netlist
-- generation to be able to generate a more complex blackbox in both sequential
-- and concurrent contexts. If the blackbox for `map` renders new declarations
-- or assignments incorrectly the generated netlist may behave differently (or
-- not even be valid HDL at all).

type Data = Vec 8 (Index 8)

initData :: Data
initData =
  -- indicesI isn't used since it produces an `imap` too
  (0 :> 1 :> 2 :> 3 :> 4 :> 5 :> 6 :> 7 :> Nil)

machine :: Reg Data -> Index 8 -> SimIO (Index 8)
machine rxs _ = do
  xs <- readReg rxs
  let next = map (satPred SatBound) xs

  writeReg rxs next
  pure (last xs)

{-# NOINLINE topEntityConc #-}
{-# ANN topEntityConc (defSyn "topEntityConc") #-}
topEntityConc
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Index 8)
topEntityConc clk rst ena =
  let regIn  = mealy clk rst ena
                 (\xs _ -> let next = map (satPred SatBound) xs in (next, last next))
                 initData
                 regOut
      regOut = register clk rst ena 0 regIn
   in regOut

{-# NOINLINE topEntitySeq #-}
{-# ANN topEntitySeq (defSyn "topEntitySeq") #-}
topEntitySeq
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Index 8)
topEntitySeq clk rst ena =
  let regIn  = mealyIO clk machine (reg initData) regOut
      regOut = register clk rst ena 0 regIn
   in regOut

-- Clash doesn't seem to have a problem with declaring a test bench for more
-- than one DUT. It seems more truthful than just marking it as the test bench
-- for one of the two entities, but maybe it's not "proper"
{-# ANN testBench (TestBench 'topEntityConc) #-}
{-# ANN testBench (TestBench 'topEntitySeq) #-}
testBench
  :: Signal System Bool
testBench = done
 where
  clk = tbClockGen (not <$> done)
  rst = resetGen
  ena = enableGen

  c = topEntityConc clk rst ena
  s = topEntitySeq clk rst ena

  check x y =
    assert clk rst "Inconsistent simulation" x y (x .==. 0)

  done =
    ignoreFor clk rst ena (SNat @1) False (check c s)

