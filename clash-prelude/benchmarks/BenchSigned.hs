{-# LANGUAGE CPP, DataKinds, MagicHash, TypeOperators, TemplateHaskell #-}

{-# OPTIONS_GHC -ddump-simpl -ddump-splices -ddump-to-file #-}

#define WORD_SIZE_IN_BITS 64

module BenchSigned where

import Clash.Sized.BitVector
import Clash.Sized.Internal.Signed
import Criterion                   (Benchmark, env, bench, nf)
import Language.Haskell.TH.Syntax  (lift)

smallValueI_pos :: Integer
smallValueI_pos = $(lift (2^(16::Int)-10 :: Integer))
{-# INLINE smallValueI_pos #-}

smallValue_pos1 :: Signed WORD_SIZE_IN_BITS
smallValue_pos1 = $(lift (2^(16::Int)-100 :: Signed WORD_SIZE_IN_BITS))
{-# INLINE smallValue_pos1 #-}

smallValue_pos2 :: Signed WORD_SIZE_IN_BITS
smallValue_pos2 = $(lift (2^(16::Int)-100 :: Signed WORD_SIZE_IN_BITS))
{-# INLINE smallValue_pos2 #-}

smallValueBV :: BitVector WORD_SIZE_IN_BITS
smallValueBV = $(lift (2^(16::Int)-10 :: BitVector WORD_SIZE_IN_BITS))
{-# INLINE smallValueBV #-}

addBench :: Benchmark
addBench = env setup $ \m ->
  bench "+# WORD_SIZE_IN_BITS" $ nf (uncurry (+#)) m
  where
    setup = return (smallValue_pos1,smallValue_pos2)

negateBench :: Benchmark
negateBench = env setup $ \m ->
  bench "negate# WORD_SIZE_IN_BITS" $ nf negate# m
  where
    setup = return smallValue_pos1

absBench :: Benchmark
absBench = env setup $ \m ->
  bench "abs# WORD_SIZE_IN_BITS" $ nf abs# m
  where
    setup = return smallValue_pos1

subBench :: Benchmark
subBench = env setup $ \m ->
  bench "-# WORD_SIZE_IN_BITS" $ nf (uncurry (-#)) m
  where
    setup = return (smallValue_pos1,smallValue_pos2)

multBench :: Benchmark
multBench = env setup $ \m ->
  bench "*# WORD_SIZE_IN_BITS" $ nf (uncurry (*#)) m
  where
    setup = return (smallValue_pos1,smallValue_pos2)

plusBench :: Benchmark
plusBench = env setup $ \m ->
  bench "plus# WORD_SIZE_IN_BITS" $ nf (uncurry (plus#)) m
  where
    setup = return (smallValue_pos1,smallValue_pos2)

minusBench :: Benchmark
minusBench = env setup $ \m ->
  bench "minus# WORD_SIZE_IN_BITS" $ nf (uncurry (minus#)) m
  where
    setup = return (smallValue_pos1,smallValue_pos2)

timesBench :: Benchmark
timesBench = env setup $ \m ->
  bench "times# WORD_SIZE_IN_BITS" $ nf (uncurry (times#)) m
  where
    setup = return (smallValue_pos1,smallValue_pos2)

fromIntegerBench :: Benchmark
fromIntegerBench = env setup $ \m ->
  bench "fromInteger# WORD_SIZE_IN_BITS" $ nf (fromInteger# :: Integer -> Signed WORD_SIZE_IN_BITS) m
  where
    setup = return smallValueI_pos

packBench :: Benchmark
packBench = env setup $ \m ->
  bench "pack# WORD_SIZE_IN_BITS" $ nf pack# m
  where
    setup = return smallValue_pos1

unpackBench :: Benchmark
unpackBench = env setup $ \m ->
  bench "unpack# WORD_SIZE_IN_BITS" $ nf unpack# m
  where
    setup = return smallValueBV
