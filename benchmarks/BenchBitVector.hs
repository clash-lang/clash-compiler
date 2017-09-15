{-# LANGUAGE CPP, DataKinds, MagicHash, TypeOperators, TemplateHaskell #-}

{-# OPTIONS_GHC -ddump-simpl -ddump-splices -ddump-to-file #-}

#define WORD_SIZE_IN_BITS 64

module BenchBitVector where

import Clash.Sized.Internal.BitVector
import Clash.Class.Num
import GHC.TypeLits                   (type (*))
import Criterion                      (Benchmark, env, bench, nf)
import Language.Haskell.TH.Syntax     (lift)

smallValue1 :: BitVector WORD_SIZE_IN_BITS
smallValue1 = $(lift (2^(16::Int)-10 :: BitVector WORD_SIZE_IN_BITS))
{-# INLINE smallValue1 #-}

smallValue2 :: BitVector WORD_SIZE_IN_BITS
smallValue2 = $(lift (2^(16::Int)-100 :: BitVector WORD_SIZE_IN_BITS))
{-# INLINE smallValue2 #-}

largeValue1 :: BitVector (3*WORD_SIZE_IN_BITS)
largeValue1 =  2^(2*WORD_SIZE_IN_BITS :: Int)-10 :: BitVector (3*WORD_SIZE_IN_BITS)
{-# INLINE largeValue1 #-}

largeValue2 :: BitVector (3*WORD_SIZE_IN_BITS)
largeValue2 =  2^(2*WORD_SIZE_IN_BITS :: Int)-100 :: BitVector (3*WORD_SIZE_IN_BITS)
{-# INLINE largeValue2 #-}

addBench :: Benchmark
addBench = env setup $ \m ->
  bench "+# WORD_SIZE_IN_BITS" $ nf (uncurry (+#)) m
  where
    setup = return (smallValue1,smallValue2)

negateBench :: Benchmark
negateBench = env setup $ \m ->
  bench "negate# WORD_SIZE_IN_BITS" $ nf negate# m
  where
    setup = return smallValue1

subBench :: Benchmark
subBench = env setup $ \m ->
  bench "-# WORD_SIZE_IN_BITS" $ nf (uncurry (-#)) m
  where
    setup = return (smallValue1,smallValue2)

multBench :: Benchmark
multBench = env setup $ \m ->
  bench "*# WORD_SIZE_IN_BITS" $ nf (uncurry (*#)) m
  where
    setup = return (smallValue1,smallValue2)

plusBench :: Benchmark
plusBench = env setup $ \m ->
  bench "plus# WORD_SIZE_IN_BITS" $ nf (uncurry (plus#)) m
  where
    setup = return (smallValue1,smallValue2)

minusBench :: Benchmark
minusBench = env setup $ \m ->
  bench "minus# WORD_SIZE_IN_BITS" $ nf (uncurry (minus#)) m
  where
    setup = return (smallValue1,smallValue2)

timesBench :: Benchmark
timesBench = env setup $ \m ->
  bench "times# WORD_SIZE_IN_BITS" $ nf (uncurry (times#)) m
  where
    setup = return (smallValue1,smallValue2)

boundedPlusBench :: Benchmark
boundedPlusBench = env setup $ \m ->
  bench "boundedPlus WORD_SIZE_IN_BITS" $ nf (uncurry (boundedPlus)) m
  where
    setup = return (smallValue1,smallValue2)

boundedMinBench :: Benchmark
boundedMinBench = env setup $ \m ->
  bench "boundedMin WORD_SIZE_IN_BITS" $ nf (uncurry (boundedMin)) m
  where
    setup = return (smallValue1,smallValue2)

boundedMultBench :: Benchmark
boundedMultBench = env setup $ \m ->
  bench "boundedMult WORD_SIZE_IN_BITS" $ nf (uncurry (boundedMult)) m
  where
    setup = return (smallValue1,smallValue2)

msbBench :: Benchmark
msbBench = env setup $ \m ->
  bench "msb# WORD_SIZE_IN_BITS" $ nf msb# m
  where
    setup = return smallValue1

msbBenchL :: Benchmark
msbBenchL = env setup $ \m ->
  bench "msb# (3*WORD_SIZE_IN_BITS)" $ nf msb# m
  where
    setup = return largeValue1

appendBench :: Benchmark
appendBench = env setup $ \m ->
  bench "++# WORD_SIZE_IN_BITS" $ nf (uncurry (++#)) m
  where
    setup = return (smallValue1,smallValue2)

appendBenchL :: Benchmark
appendBenchL = env setup $ \m ->
  bench "++# (3*WORD_SIZE_IN_BITS)" $ nf (uncurry (++#)) m
  where
    setup = return (largeValue1,largeValue2)

splitBench :: Benchmark
splitBench = env setup $ \m ->
  bench "split# WORD_SIZE_IN_BITS" $ nf (split# :: BitVector WORD_SIZE_IN_BITS -> (BitVector 18, BitVector 46)) m
  where
    setup = return smallValue1

splitBenchL :: Benchmark
splitBenchL = env setup $ \m ->
  bench "split# (3*WORD_SIZE_IN_BITS)" $ nf (split# :: BitVector (3*WORD_SIZE_IN_BITS) -> (BitVector 18, BitVector 174)) m
  where
    setup = return largeValue1
