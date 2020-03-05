{-# LANGUAGE CPP, DataKinds, MagicHash, TypeOperators, TemplateHaskell #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

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
  ]

smallValue1 :: Vec 8 (BitVector 24)
smallValue1 = $(lift (replicate d8 (2^(16::Int)-10 :: BitVector 24)))
{-# INLINE smallValue1 #-}

vectorPackBench :: Benchmark
vectorPackBench = env setup $ \m ->
  bench "pack" $
  nf (pack :: Vec 8 (BitVector 24) -> BitVector 192) m
  where
    setup = return smallValue1
