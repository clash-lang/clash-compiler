{-# OPTIONS_GHC -Wno-orphans -fconstraint-solver-iterations=0#-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module DualBlockRamDefinitions where

import qualified Prelude as P

import Clash.Explicit.Prelude
import Clash.Explicit.BlockRam
import DualBlockRamTypes

-- Creating domains A, B and C
createDomain vSystem{vName="A", vPeriod=50000} -- fast
createDomain vSystem{vName="B", vPeriod=100000} -- slow
createDomain vSystem{vName="C", vPeriod=140000}--hzToPeriod 7e6} -- slower

-- Clocks for domains A, B and C
clk20 = clockGen @A
clk10 = clockGen @B
clk7 = clockGen @C

-- Resets for domains A, B and C.
rst20 :: Reset A
rst20 = resetGen @A
rst10 :: Reset B
rst10 = resetGen @B
rst7 :: Reset C
rst7 = resetGen @C

topEntity ::
  ( KnownDomain domA
  , KnownDomain domB
  ) =>
  -- Configuration
  TDPConfig ->

  -- Clocks
  Clock  domA ->
  Clock  domB ->

  --Operations
  Signal domA (RamOp 30 ThisOrThat) ->
  Signal domB (RamOp 30 ThisOrThat) ->

  --Output
  ( Signal domA ThisOrThat
  , Signal domB ThisOrThat )

topEntity = trueDualPortBlockRam

{-#NOINLINE topEntity #-}

twice = concatMap (replicate d2)
strictAnd !a !b = (&&) a b


{- Testvectors
Setup : Setup cycles
Test0 : Write to different addresses and check if value is present at output.
Test1 : Read stored values written by A from A, same for B.
Test2 : Read stored values written by B from A, same for B.
Test3 : Conflict R R - No problem
Test4 : Conflict W R - (Value, undefined)
Test5 : Conflict R W - (undefined, value)
Test6 : Conflict W W - (undefined, undefined)
Test7 : Multiple writes to same address with different values
Test8 : W N
Test9 : N W
Test10 : R N
Test11 : N R
Test12 : N N
-}

addrsA = $(listToVecTH $ P.take 20 [0 :: Addr ..])
addrsB = $(listToVecTH $ P.take 10 [20 :: Addr ..])
valuesA = $(listToVecTH $ P.map This $ P.take 20 [30..])
valuesB = $(listToVecTH $ P.map That $ P.take 10 [40..])

opsA0,opsA1,opsA2,opsA3,opsA4,opsA5,opsA6,opsA7,opsA8,opsA9,opsA10,opsA11,opsA12 :: Vec 22 (RamOp 30 ThisOrThat)
opsB0,opsB1,opsB2,opsB3,opsB4,opsB5,opsB6,opsB7,opsB8,opsB9,opsB10,opsB11,opsB12 :: Vec 11 (RamOp 30 ThisOrThat)

--Test 0
opsA0 = twice (RamRead 0 :> Nil) ++ (zipWith RamWrite addrsA valuesA)
opsB0 = RamRead 0 :> (zipWith RamWrite addrsB valuesB)

--Test 1
opsA1 = twice (RamRead 1 :> Nil) ++ (fmap RamRead addrsA)
opsB1 = RamRead 1 :>  (fmap RamRead addrsB)

--Test 2
opsA2 = twice $ (RamRead 2 :> (fmap RamRead addrsB))
opsB2 = RamRead 2 :> (take d10 $ fmap RamRead addrsA)

-- Test 3
addrs3 = take d10 addrsA
values3 = take d10 valuesA
opsA3 = twice $ (RamRead 3 :> (fmap RamRead addrs3))
opsB3 = RamRead 3 :> (fmap RamRead addrs3)

-- Test 4
addrs4 =  addrsB
values4 = take d10 valuesA
opsA4 = twice $ (RamRead 4 :> (zipWith RamWrite addrs4 values4))
opsB4 = RamRead 4 :> (fmap RamRead addrs4)

-- Test 5
addrs5 =  take d10 addrsA
values5 = valuesB
opsA5 = twice $ (RamRead 5 :> (fmap RamRead addrs5))
opsB5 = RamRead 5 :> (zipWith RamWrite addrs5 values5)

-- Test 6
addrs6 =  addrsB
values6 = valuesB
opsA6 = twice $ RamRead 6 :> (zipWith RamWrite addrs6 values6)
opsB6 = RamRead 6 :> (zipWith RamWrite addrs6 values6)

-- Test 7
opsA7 = twice (RamRead 7 :> Nil) ++ (zipWith RamWrite (replicate d20 $ head addrsA) valuesA)
opsB7 = RamRead 7 :> (zipWith RamWrite (replicate d10 $ head addrsB) valuesB)

-- Test 8
opsA8 = twice (RamRead 8 :> Nil) ++ (fmap RamRead addrsA)
opsB8 = RamRead 8 :> (replicate d10 RamNoOp)

-- Test 9
opsA9 = twice (RamRead 9 :> Nil) ++ (zipWith RamWrite addrsA $ twice valuesB)
opsB9 = RamRead 9 :> (replicate d10 RamNoOp)

-- Test 10
opsA10 = twice (RamRead 10 :> Nil) ++ (replicate d20 RamNoOp)
opsB10 = RamRead 10 :> (zipWith RamWrite addrsB $ take d10 valuesA)

-- Test 11
opsA11 = twice (RamRead 11 :> Nil) ++ replicate d20 RamNoOp
opsB11 = RamRead 11 :> (fmap RamRead addrsB)

-- Test12
opsA12 = twice (RamRead 1 :> Nil) ++ (fmap RamRead addrsA)
opsB12 = RamRead 1 :>  (fmap RamRead addrsB)

--All operations
opsA = (RamNoOp :> opsA0) ++ opsA1 ++ opsA2 ++ opsA3 ++ opsA4 ++ opsA5 ++ opsA6
 ++ opsA7 ++ opsA8 ++ opsA9 ++ opsA10 ++ opsA11 ++ opsA12
opsB = (RamNoOp :> opsB0) ++ opsB1 ++ opsB2 ++ opsB3 ++ opsB4 ++ opsB5 ++ opsB6
 ++ opsB7 ++ opsB8 ++ opsB9 ++ opsB10 ++ opsB11 ++ opsB12


-- opsA = RamRead 0 :> RamWrite 0 (This 2) :> RamRead 0 :> RamRead 0 :> RamRead 0 :> RamRead 0 :> RamRead 1 :> RamRead 2 :> RamRead 3 :> RamRead 4 :> RamRead 5 :> Nil
-- opsB = RamRead 20 :> RamRead 20 :> RamWrite 0 (This 1) :> Nil
inputWritesA, inputWritesB :: KnownDomain dom => Clock dom -> Reset dom -> Signal dom (RamOp 30 ThisOrThat)
inputWritesA clk rst = stimuliGenerator clk rst opsA
inputWritesB clk rst = stimuliGenerator clk rst opsB

topOut clkA clkB wmA wmB rstA rstB =
  topEntity (tdpDefault{writeModeA = wmA, writeModeB = wmB}) clkA clkB (inputWritesA clkA rstA ) (inputWritesB clkB rstB)
