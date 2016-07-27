{-# LANGUAGE CPP, DataKinds, MagicHash, TypeOperators, TemplateHaskell #-}

{-# OPTIONS_GHC -ddump-simpl -ddump-splices -ddump-to-file #-}

#define WORD_SIZE_IN_BITS 64

module BenchSigned where

import CLaSH.Sized.Internal.Signed
import GHC.TypeLits                   (type (*))
import Criterion                      (Benchmark, env, bench, nf)
import Language.Haskell.TH.Syntax     (lift)

smallValueI_pos :: Integer
smallValueI_pos = $(lift (2^(16::Int)-10 :: Integer))
{-# INLINE smallValueI #-}

fromIntegerBench :: Benchmark
fromIntegerBench = env setup $ \m ->
  bench "msb# WORD_SIZE_IN_BITS" $ nf (fromInteger# :: Integer -> Signed WORD_SIZE_IN_BITS) m
  where
    setup = return smallValueI_pos
