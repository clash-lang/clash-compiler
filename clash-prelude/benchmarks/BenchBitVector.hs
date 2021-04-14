{-# LANGUAGE CPP, DataKinds, MagicHash, TypeOperators, TemplateHaskell #-}
{-# LANGUAGE NoStarIsType #-}

{-# OPTIONS_GHC -ddump-simpl -ddump-splices -ddump-to-file #-}

#define WORD_SIZE_IN_BITS 64

module BenchBitVector where

import Data.Bits
import Clash.Sized.BitVector
import Clash.Class.Num
import Clash.Prelude.BitIndex
import GHC.TypeLits                   (type (*))
import Criterion                      (Benchmark, env, bench, nf, bgroup)
import Language.Haskell.TH.Syntax     (lift)

import BenchCommon

bitVectorBench :: Benchmark
bitVectorBench = bgroup "BitVector"
  [ fromIntegerBench
  , addBench
  , addBenchL
  , negateBench
  , negateBenchL
  , subBench
  , subBenchL
  , multBench
  , multBenchL
  , plusBench
  , minusBench
  , timesBench
  , boundedAddBench
  , boundedSubBench
  , boundedMulBench
  , msbBench
  , msbBenchL
  , appendBench
  , appendBenchL
  , splitBench
  , splitBenchL
  , xorBench
  , xorBenchL
  , andBench
  , andBenchL
  , orBench
  , orBenchL
  , complementBench
  , complementBenchL
  ]

smallValueI :: Integer
smallValueI = $(lift (2^(16::Int)-10 :: Integer))
{-# INLINE smallValueI #-}

smallValue1 :: BitVector WORD_SIZE_IN_BITS
smallValue1 = $(lift (2^(16::Int)-10 :: BitVector WORD_SIZE_IN_BITS))
{-# INLINE smallValue1 #-}

smallValue2 :: BitVector WORD_SIZE_IN_BITS
smallValue2 = $(lift (2^(16::Int)-100 :: BitVector WORD_SIZE_IN_BITS))
{-# INLINE smallValue2 #-}

largeValue1 :: BitVector (3*WORD_SIZE_IN_BITS)
largeValue1 = $(lift (2^(2*WORD_SIZE_IN_BITS :: Int)-10 :: BitVector (3*WORD_SIZE_IN_BITS)))
{-# INLINE largeValue1 #-}

largeValue2 :: BitVector (3*WORD_SIZE_IN_BITS)
largeValue2 = $(lift (2^(2*WORD_SIZE_IN_BITS :: Int)-100 :: BitVector (3*WORD_SIZE_IN_BITS)))
{-# INLINE largeValue2 #-}

fromIntegerBench :: Benchmark
fromIntegerBench = env setup $ \m ->
  bench "fromInteger WORD_SIZE_IN_BITS" $
  nf (fromInteger :: Integer -> BitVector WORD_SIZE_IN_BITS) m
  where
    setup = return smallValueI

addBench :: Benchmark
addBench = env setup $ \m ->
  bench "+ WORD_SIZE_IN_BITS" $ nf (apSwapAp (+)) m
  where
    setup = return (smallValue1,smallValue2)

addBenchL :: Benchmark
addBenchL = env setup $ \m ->
  bench "+ 3*WORD_SIZE_IN_BITS" $ nf (apSwapAp (+)) m
  where
    setup = return (largeValue1,largeValue2)

negateBench :: Benchmark
negateBench = env setup $ \m ->
  bench "negate WORD_SIZE_IN_BITS" $ nf negate m
  where
    setup = return smallValue1

negateBenchL :: Benchmark
negateBenchL = env setup $ \m ->
  bench "negate 3*WORD_SIZE_IN_BITS" $ nf negate m
  where
    setup = return largeValue1

subBench :: Benchmark
subBench = env setup $ \m ->
  bench "- WORD_SIZE_IN_BITS" $ nf (apSwapAp (-)) m
  where
    setup = return (smallValue1,smallValue2)

subBenchL :: Benchmark
subBenchL = env setup $ \m ->
  bench "- 3*WORD_SIZE_IN_BITS" $ nf (apSwapAp (-)) m
  where
    setup = return (largeValue1,largeValue2)

multBench :: Benchmark
multBench = env setup $ \m ->
  bench "* WORD_SIZE_IN_BITS" $ nf (apSwapAp (*)) m
  where
    setup = return (smallValue1,smallValue2)

multBenchL :: Benchmark
multBenchL = env setup $ \m ->
  bench "* 3*WORD_SIZE_IN_BITS" $ nf (apSwapAp (*)) m
  where
    setup = return (largeValue1,largeValue2)

plusBench :: Benchmark
plusBench = env setup $ \m ->
  bench "plus WORD_SIZE_IN_BITS" $ nf (apSwapAp2 add add) m
  where
    setup = return (smallValue1,smallValue2)

minusBench :: Benchmark
minusBench = env setup $ \m ->
  bench "minus WORD_SIZE_IN_BITS" $ nf (apSwapAp2 sub sub) m
  where
    setup = return (smallValue1,smallValue2)

timesBench :: Benchmark
timesBench = env setup $ \m ->
  bench "times WORD_SIZE_IN_BITS" $ nf (apSwapAp2 mul mul) m
  where
    setup = return (smallValue1,smallValue2)

boundedAddBench :: Benchmark
boundedAddBench = env setup $ \m ->
  bench "boundedAdd WORD_SIZE_IN_BITS" $ nf (apSwapAp boundedAdd) m
  where
    setup = return (smallValue1,smallValue2)

boundedSubBench :: Benchmark
boundedSubBench = env setup $ \m ->
  bench "boundedSub WORD_SIZE_IN_BITS" $ nf (apSwapAp boundedSub) m
  where
    setup = return (smallValue1,smallValue2)

boundedMulBench :: Benchmark
boundedMulBench = env setup $ \m ->
  bench "boundedMul WORD_SIZE_IN_BITS" $ nf (apSwapAp boundedMul) m
  where
    setup = return (smallValue1,smallValue2)

msbBench :: Benchmark
msbBench = env setup $ \m ->
  bench "msb# WORD_SIZE_IN_BITS" $ nf msb m
  where
    setup = return smallValue1

msbBenchL :: Benchmark
msbBenchL = env setup $ \m ->
  bench "msb# (3*WORD_SIZE_IN_BITS)" $ nf msb m
  where
    setup = return largeValue1

appendBench :: Benchmark
appendBench = env setup $ \m ->
  bench "++# WORD_SIZE_IN_BITS" $ nf (apSwapAp2 (++#) (++#)) m
  where
    setup = return (smallValue1,smallValue2)

appendBenchL :: Benchmark
appendBenchL = env setup $ \m ->
  bench "++# (3*WORD_SIZE_IN_BITS)" $ nf (apSwapAp2 (++#) (++#)) m
  where
    setup = return (largeValue1,largeValue2)

splitBench :: Benchmark
splitBench = env setup $ \m ->
  bench "split# WORD_SIZE_IN_BITS" $ nf (split :: BitVector WORD_SIZE_IN_BITS -> (BitVector 18, BitVector 46)) m
  where
    setup = return smallValue1

splitBenchL :: Benchmark
splitBenchL = env setup $ \m ->
  bench "split# (3*WORD_SIZE_IN_BITS)" $ nf (split :: BitVector (3*WORD_SIZE_IN_BITS) -> (BitVector 18, BitVector 174)) m
  where
    setup = return largeValue1

xorBench :: Benchmark
xorBench = env setup $ \m ->
  bench "xor WORD_SIZE_IN_BITS" $ nf (apSwapAp xor) m
  where
    setup = return (smallValue1,smallValue2)

xorBenchL :: Benchmark
xorBenchL = env setup $ \m ->
  bench "xor 3*WORD_SIZE_IN_BITS" $ nf (apSwapAp xor) m
  where
    setup = return (largeValue1,largeValue2)

andBench :: Benchmark
andBench = env setup $ \m ->
  bench ".&. WORD_SIZE_IN_BITS" $ nf (apSwapAp (.&.)) m
  where
    setup = return (smallValue1,smallValue2)

andBenchL :: Benchmark
andBenchL = env setup $ \m ->
  bench ".&. 3*WORD_SIZE_IN_BITS" $ nf (apSwapAp (.&.)) m
  where
    setup = return (largeValue1,largeValue2)

orBench :: Benchmark
orBench = env setup $ \m ->
  bench ".|. WORD_SIZE_IN_BITS" $ nf (apSwapAp (.|.)) m
  where
    setup = return (smallValue1,smallValue2)

orBenchL :: Benchmark
orBenchL = env setup $ \m ->
  bench ".|. 3*WORD_SIZE_IN_BITS" $ nf (apSwapAp (.|.)) m
  where
    setup = return (largeValue1,largeValue2)

complementBench :: Benchmark
complementBench = env setup $ \m ->
  bench "complement WORD_SIZE_IN_BITS" $ nf complement m
  where
    setup = return smallValue1

complementBenchL :: Benchmark
complementBenchL = env setup $ \m ->
  bench "complement 3*WORD_SIZE_IN_BITS" $ nf complement m
  where
    setup = return largeValue1
