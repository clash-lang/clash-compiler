{-# OPTIONS_GHC -Wno-orphans -Wno-missing-signatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module DualBlockRamDefinitions where

import qualified Prelude as P
import Clash.Explicit.Prelude hiding (fromList)
import Clash.Sized.Vector(fromList)
import Language.Haskell.TH
import DualBlockRamTypes

createDomain vSystem{vName="A", vPeriod=hzToPeriod 20e6} -- fast
createDomain vSystem{vName="B", vPeriod=hzToPeriod 10e6} -- slow
createDomain vSystem{vName="C", vPeriod=hzToPeriod 7e6} -- slower

tdpRam :: (KnownDomain domA, KnownDomain domB) => TdpRam domA domB
tdpRam = trueDualPortBlockRam content
 where
  Just content =  $(lift $ fromList @73 $ P.take 73 ((cycle [This, That . truncateB]) <*> [0..]))

{- Testvectors
Test0 : Write to different addresses and check if value is present at output.
Test1 : Read stored values written by A from A, same for B.
Test2 : Read stored values written by B from A, same for B.
Test3 : Conflict R R - No problem
Test4 : Conflict W R - (Value, undefined)
Test5 : Conflict R W - (undefined, value)
Test6 : Conflict W W - (undefined, undefined)
Test7 : Multiple writes to same address with different values
Test8 : R N
Test9 : N R
Test10 : W N
Test11 : N W
Test12 : N N
-}

addrsA = $(listToVecTH $ P.take 20 [0 :: Index 73 ..])
addrsB = $(listToVecTH $ P.take 10 [20 :: Index 73..])
valuesA = $(listToVecTH $ P.map This $ P.take 20 [30..])
valuesB = $(listToVecTH $ P.map That $ P.take 10 [40..])

--Test 0
opsA0 = zipWith RamWrite addrsA valuesA
opsB0 = zipWith RamWrite addrsB valuesB

--Test 1
opsA1 = fmap RamRead addrsA
opsB1 = fmap RamRead addrsB

--Test 2
opsA2 = twice $ fmap RamRead addrsB
opsB2 = take d10 $ fmap RamRead addrsA

-- Test 3
addrs3 = take d10 addrsA
values3 = take d10 valuesA
opsA3 = twice $ fmap RamRead addrs3
opsB3 = fmap RamRead addrs3

-- Test 4
addrs4 =  addrsB
values4 = take d10 valuesA
opsA4 = twice $ zipWith RamWrite addrs4 values4
opsB4 = fmap RamRead addrs4

-- Test 5
addrs5 =  take d10 addrsA
values5 = valuesB
opsA5 = twice $ fmap RamRead addrs5
opsB5 = zipWith RamWrite addrs5 values5

-- Test 6
addrs6 =  addrsB
values6 = valuesB
opsA6 = twice $ zipWith RamWrite addrs6 values6
opsB6 = zipWith RamWrite addrs6 values6

-- Test 7
opsA7 = zipWith RamWrite (replicate d20 $ head addrsA) valuesA
opsB7 = zipWith RamWrite (replicate d10 $ head addrsB) valuesB

-- Test 8
opsA8 = fmap RamRead addrsA
opsB8 = replicate d10 RamNoOp

-- Test 9
opsA9 = replicate d20 RamNoOp
opsB9 = fmap RamRead addrsB

-- Test 10
opsA10 = zipWith RamWrite addrsA $ twice valuesB
opsB10 = replicate d10 RamNoOp

-- Test 11
opsA11 = replicate d20 RamNoOp
opsB11 = zipWith RamWrite addrsB $ take d10 valuesA

-- Test12
opsA12 = replicate d20 RamNoOp
opsB12 = replicate d10 RamNoOp

-- All operations
opsA = opsA0 ++ opsA1 ++ opsA2 ++ opsA3 ++ opsA4 ++ opsA5 ++ opsA6
 ++ opsA7 ++ opsA8 ++ opsA9 ++ opsA10 ++ opsA11 ++ opsA12
opsB = opsB0 ++ opsB1 ++ opsB2 ++ opsB3 ++ opsB4 ++ opsB5 ++ opsB6
 ++ opsB7 ++ opsB8 ++ opsB9 ++ opsB10 ++ opsB11 ++ opsB12

topOut top clkA rstA clkB rstB = out
  where
    out = top clkA clkB (inputWritesA clkA rstA )  (inputWritesB clkB rstB)

simEntityAB = topOut tdpRam clk20TH noRst20 clk10TH noRst10
simEntityBC = topOut tdpRam clk10TH noRst10 clk7TH noRst7

inputWritesA clk rst = stimuliGenerator clk rst opsA
inputWritesB clk rst = stimuliGenerator clk rst opsB

clk20TH = clockGen @A
clk10TH = clockGen @B
clk7TH = clockGen @C

noRst20 :: Reset A
noRst20 = unsafeFromHighPolarity (pure False)
noRst10 :: Reset B
noRst10 = unsafeFromHighPolarity (pure False)
noRst7 :: Reset C
noRst7 = unsafeFromHighPolarity (pure False)

twice = concatMap (replicate d2)
strictAnd !a !b = (&&) a b
