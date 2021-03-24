{-# OPTIONS_GHC -Wno-orphans -fconstraint-solver-iterations=0#-}
{-# LANGUAGE BangPatterns #-}
module DualBlockRamDefinitions where

import qualified Prelude as P

import Clash.Explicit.Prelude
import Clash.Explicit.BlockRam
import Clash.Explicit.Testbench
import Data.Bifunctor (bimap)
import DualBlockRamTypes

createDomain vSystem{vName="A", vPeriod=hzToPeriod 20e6} -- fast
createDomain vSystem{vName="B", vPeriod=hzToPeriod 10e6} -- slow
createDomain vSystem{vName="C", vPeriod=hzToPeriod 7e6} -- slower

topEntity ::
  ( KnownDomain domA
  , KnownDomain domB
  ) =>

  -- Clocks
  Clock  domA ->
  Clock  domB ->

  --Operations
  Signal domA (RamOp 73 ThisOrThat) ->
  Signal domB (RamOp 73 ThisOrThat) ->

  --Output
  ( Signal domA ThisOrThat
  , Signal domB ThisOrThat )

topEntity = trueDualPortBlockRam
{-#NOINLINE topEntity #-}

{- Testvectors
Setup 0 - 2: Setup cycles
Test0 3 - 12: Write to different addresses and check if value is present at output.
Test1 13 -22: Read stored values written by A from A, same for B.
Test2 23 - 32: Read stored values written by B from A, same for B.
Test3 33 - 42: Conflict R R - No problem
Test4 43 - 52: Conflict W R - (Value, undefined)
Test5 53 - 62: Conflict R W - (undefined, value)
Test6 63 - 72: Conflict W W - (undefined, undefined)
Test7 73 - 82: Multiple writes to same address with different values
-}

addrsA = $(listToVecTH $ P.take 40 [0 :: Index 73 ..])
addrsB = $(listToVecTH $ P.take 20 [40 :: Index 73..])
valuesA = $(listToVecTH $ P.map This $ P.take 40 [60..])
valuesB = $(listToVecTH $ P.map That $ P.take 20 [80..])

--Test 0
opsA0 = zipWith RamWrite addrsA valuesA
opsB0 = zipWith RamWrite addrsB valuesB

--Test 1
opsA1 = fmap RamRead addrsA
opsB1 = fmap RamRead addrsB

--Test 2
opsA2 = twice $ fmap RamRead addrsB
opsB2 = take d20 $ fmap RamRead addrsA

-- Test 3
addrs3 = take d20 addrsA
values3 = take d20 valuesA
opsA3 = twice $ fmap RamRead addrs3
opsB3 = fmap RamRead addrs3

-- Test 4
addrs4 =  addrsB
values4 = take d20 valuesA
opsA4 = twice $ zipWith RamWrite addrs4 values4
opsB4 = fmap RamRead addrs4

-- Test 5
addrs5 =  take d20 addrsA
values5 = valuesB
opsA5 = twice $ fmap RamRead addrs5
opsB5 = zipWith RamWrite addrs5 values5

-- Test 6
addrs6 =  addrsB
values6 = valuesB
opsA6 = twice $ zipWith RamWrite addrs6 values6
opsB6 = zipWith RamWrite addrs6 values6

-- Test 7
opsA7 = zipWith RamWrite (replicate d40 $ head addrsA) valuesA
opsB7 = zipWith RamWrite (replicate d20 $ head addrsB) valuesB

--All operations
opsA = opsA0 ++ opsA1 ++ opsA2 ++ opsA3 ++ opsA4 ++ opsA5 ++ opsA6 ++ opsA7
opsB = opsB0 ++ opsB1 ++ opsB2 ++ opsB3 ++ opsB4 ++ opsB5 ++ opsB6 ++ opsB7

topOut clkA rstA clkB rstB = out
  where
    out = topEntity clkA clkB (inputWritesA clkA rstA )  (inputWritesB clkB rstB)

simEntityAB = topOut clk20TH rst20 clk10TH rst10
simEntityBC = topOut clk10TH rst10 clk7TH rst7

inputWritesA clk rst = stimuliGenerator clk rst opsA
inputWritesB clk rst = stimuliGenerator clk rst opsB

clk20TH = clockGen @A
clk10TH = clockGen @B
clk7TH = clockGen @C

rst20 :: Reset A
rst20 = resetGen @A
rst10 :: Reset B
rst10 = resetGen @B
rst7 :: Reset C
rst7 = resetGen @C

twice = concatMap (replicate d2)
strictAnd !a !b = (&&) a b
