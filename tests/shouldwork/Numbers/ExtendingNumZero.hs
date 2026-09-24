{-# LANGUAGE CPP #-}

module ExtendingNumZero where

import Clash.Prelude
import Clash.Explicit.Testbench

type Results a b =
  ( AResult a b, AResult b a, AResult a b, AResult b a
  , MResult a b, MResult b a
  )

extendingNumZero
  :: (ExtendingNum a b, ExtendingNum b a)
  => a -> b -> Results a b
extendingNumZero n z =
  (add n z, add z n, sub n z, sub z n, mul n z, mul z n)

topBitVector
  :: BitVector 16 -> BitVector 0 -> Results (BitVector 16) (BitVector 0)
topBitVector = extendingNumZero
{-# OPAQUE topBitVector #-}

topSigned :: Signed 16 -> Signed 0 -> Results (Signed 16) (Signed 0)
topSigned = extendingNumZero
{-# OPAQUE topSigned #-}

topUnsigned :: Unsigned 16 -> Unsigned 0 -> Results (Unsigned 16) (Unsigned 0)
topUnsigned = extendingNumZero
{-# OPAQUE topUnsigned #-}

topIndex :: Index 65536 -> Index 1 -> Results (Index 65536) (Index 1)
topIndex = extendingNumZero
{-# OPAQUE topIndex #-}

topBothZero
  :: BitVector 0 -> Signed 0 -> Unsigned 0 -> Index 1
  -> ( Results (BitVector 0) (BitVector 0)
     , Results (Signed 0) (Signed 0)
     , Results (Unsigned 0) (Unsigned 0)
     , Results (Index 1) (Index 1)
     )
topBothZero bv s u i =
  ( extendingNumZero bv bv
  , extendingNumZero s s
  , extendingNumZero u u
  , extendingNumZero i i
  )
{-# OPAQUE topBothZero #-}

testBenchBitVector :: Signal System Bool
testBenchBitVector = done
 where
  inputs = 0 :> 1 :> 22 :> 32768 :> maxBound :> Nil
  expected n = let n' = resize n in (n', n', n', negate n', 0, 0)
  testInput = stimuliGenerator clk rst inputs
  expectedOutput = outputVerifier' clk rst (map expected inputs)
  done = expectedOutput (topBitVector <$> testInput <*> pure 0)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
{-# OPAQUE testBenchBitVector #-}
{-# ANN testBenchBitVector (TestBench 'topBitVector) #-}

testBenchSigned :: Signal System Bool
testBenchSigned = done
 where
  inputs = 0 :> 1 :> 22 :> (-1) :> minBound :> maxBound :> Nil
  expected n = let n' = resize n in (n', n', n', negate n', 0, 0)
  testInput = stimuliGenerator clk rst inputs
  expectedOutput = outputVerifier' clk rst (map expected inputs)
  done = expectedOutput (topSigned <$> testInput <*> pure 0)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
{-# OPAQUE testBenchSigned #-}
{-# ANN testBenchSigned (TestBench 'topSigned) #-}

testBenchUnsigned :: Signal System Bool
testBenchUnsigned = done
 where
  inputs = 0 :> 1 :> 22 :> 32768 :> maxBound :> Nil
  expected n = let n' = resize n in (n', n', n', negate n', 0, 0)
  testInput = stimuliGenerator clk rst inputs
  expectedOutput = outputVerifier' clk rst (map expected inputs)
  done = expectedOutput (topUnsigned <$> testInput <*> pure 0)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
{-# OPAQUE testBenchUnsigned #-}
{-# ANN testBenchUnsigned (TestBench 'topUnsigned) #-}

testBenchIndex :: Signal System Bool
testBenchIndex = done
 where
  inputs = 0 :> 1 :> 22 :> 32768 :> maxBound :> Nil
  -- Subtracting a positive Index from zero is undefined. Still check zero
  -- minus zero, and generate HDL for both operand orders.
  maskUnderflow n (a, b, c, d, e, f) =
    (a, b, c, if n == 0 then d else 0, e, f)
  expected n = (n, n, n, 0, 0, 0)
  testInput = stimuliGenerator clk rst inputs
  expectedOutput = outputVerifier' clk rst (map expected inputs)
  actual = topIndex <$> testInput <*> pure 0
  done = expectedOutput (maskUnderflow <$> testInput <*> actual)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
{-# OPAQUE testBenchIndex #-}
{-# ANN testBenchIndex (TestBench 'topIndex) #-}

testBenchBothZero :: Signal System Bool
testBenchBothZero = done
 where
  expectedOutput = outputVerifier' clk rst
    (((0, 0, 0, 0, 0, 0), (0, 0, 0, 0, 0, 0),
      (0, 0, 0, 0, 0, 0), (0, 0, 0, 0, 0, 0)) :> Nil)
  done = expectedOutput (topBothZero <$> pure 0 <*> pure 0 <*> pure 0 <*> pure 0)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
{-# OPAQUE testBenchBothZero #-}
{-# ANN testBenchBothZero (TestBench 'topBothZero) #-}
