{-# LANGUAGE CPP, DataKinds, MagicHash, TypeOperators, TemplateHaskell #-}

{-# OPTIONS_GHC -ddump-simpl -ddump-splices -ddump-to-file #-}

#define WORD_SIZE_IN_BITS 64

module BenchFixed (fixedBench) where

import Clash.Class.Num
import Clash.Sized.Fixed
import Clash.Sized.Unsigned
import Criterion                   (Benchmark, env, bench, nf, bgroup)
import Language.Haskell.TH.Syntax  (lift)

fixedBench :: Benchmark
fixedBench = bgroup "Fixed"
  [ fromRationalBench
  , addBench
  , subBench
  , multBench
  , multBench_wrap
  ]

smallValueR_pos :: Rational
smallValueR_pos = $(lift (5126.889117 :: Rational))
{-# INLINE smallValueR_pos #-}

smallValueU1 :: UFixed 24 17
smallValueU1 = $(lift (5126.889117 :: UFixed 24 17))
{-# INLINE smallValueU1 #-}

smallValueU2 :: UFixed 24 17
smallValueU2 = $(lift (56.589117 :: UFixed 24 17))
{-# INLINE smallValueU2 #-}

fromRationalBench :: Benchmark
fromRationalBench = env setup $ \m ->
  bench "fromRational" $ nf (fromRational :: Rational -> UFixed 24 17) m
  where
    setup = return smallValueR_pos

addBench :: Benchmark
addBench = env setup $ \m ->
  bench "+" $ nf (uncurry (+)) m
  where
    setup = return (smallValueU1,smallValueU2)

subBench :: Benchmark
subBench = env setup $ \m ->
  bench "-" $ nf (uncurry (-)) m
  where
    setup = return (smallValueU1,smallValueU2)

multBench :: Benchmark
multBench = env setup $ \m ->
  bench "*" $ nf (uncurry (*)) m
  where
    setup = return (smallValueU1,smallValueU2)

multBench_wrap :: Benchmark
multBench_wrap = env setup $ \m ->
  bench "satMult SatWrap" $ nf (uncurry (satMul SatWrap)) m
  where
    setup = return (smallValueU1,smallValueU2)
