{-# LANGUAGE CPP, DataKinds, MagicHash, TypeOperators, TemplateHaskell #-}
{-# LANGUAGE NoStarIsType #-}

{-# OPTIONS_GHC -ddump-simpl -ddump-splices -ddump-to-file #-}

#define WORD_SIZE_IN_BITS 64

module BenchVector (vectorBench) where

import Clash.Class.BitPack
import Clash.Promoted.Nat.Literals
import Clash.Sized.BitVector
import Clash.Sized.Vector
import Criterion                   (Benchmark, env, bench, nf, bgroup)
import Language.Haskell.TH.Syntax  (lift)
import Prelude                     hiding (replicate)

vectorBench :: Benchmark
vectorBench = bgroup "Vector"
  [ vectorPackBench
  , vectorUnpackBench
  ]

smallValue1 :: Vec 8 (BitVector 24)
smallValue1 = $(lift (replicate d8 (2^(16::Int)-10 :: BitVector 24)))
{-# INLINE smallValue1 #-}

smallValue2 :: BitVector 192
smallValue2 = $(lift (maxBound :: BitVector 192))
{-# INLINE smallValue2 #-}

vectorPackBench :: Benchmark
vectorPackBench = env setup $ \m ->
  bench "pack" $
  nf (pack :: Vec 8 (BitVector 24) -> BitVector 192) m
  where
    setup = return smallValue1

vectorUnpackBench :: Benchmark
vectorUnpackBench = env setup $ \m ->
  bench "unpack" $
  nf (unpack :: BitVector 192 -> Vec 8 (BitVector 24)) m
  where
    setup = return smallValue2
