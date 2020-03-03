{-# LANGUAGE CPP, DataKinds, MagicHash, TypeOperators, TemplateHaskell #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

{-# OPTIONS_GHC -ddump-simpl -ddump-splices -ddump-to-file #-}

#define WORD_SIZE_IN_BITS 64

module BenchUnsigned (unsignedBench) where

import Data.Bits
import Data.Word
import Clash.Class.Num
import Clash.Class.BitPack
import Clash.Sized.BitVector
import Clash.Sized.Unsigned
import Criterion                   (Benchmark, env, bench, nf, bgroup)
import Language.Haskell.TH.Syntax  (lift)
import GHC.TypeLits                (type (*))

import BenchCommon

unsignedBench :: Benchmark
unsignedBench = bgroup "Unsigned"
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
  , packBench
  , unpackBench
  , xorBench
  , xorBenchL
  , andBench
  , andBenchL
  , orBench
  , orBenchL
  , complementBench
  , complementBenchL
  , unsigned8toWord8Bench
  , unsigned16toWord16Bench
  , unsigned32toWord32Bench
  , unsignedToWordBench
  ]

smallValueI :: Integer
smallValueI = $(lift (2^(16::Int)-10 :: Integer))
{-# INLINE smallValueI #-}

smallValue1 :: Unsigned WORD_SIZE_IN_BITS
smallValue1 = $(lift (2^(16::Int)-10 :: Unsigned WORD_SIZE_IN_BITS))
{-# INLINE smallValue1 #-}

smallValue2 :: Unsigned WORD_SIZE_IN_BITS
smallValue2 = $(lift (2^(16::Int)-100 :: Unsigned WORD_SIZE_IN_BITS))
{-# INLINE smallValue2 #-}

smallValueW8 :: Unsigned 8
smallValueW8 = $(lift (2^(4::Int)-10 :: Unsigned 8))
{-# INLINE smallValueW8 #-}

smallValueW16 :: Unsigned 16
smallValueW16 = $(lift (2^(8::Int)-10 :: Unsigned 16))
{-# INLINE smallValueW16 #-}

smallValueW32 :: Unsigned 32
smallValueW32 = $(lift (2^(16::Int)-10 :: Unsigned 32))
{-# INLINE smallValueW32 #-}

smallValueBV :: BitVector WORD_SIZE_IN_BITS
smallValueBV = $(lift (2^(16::Int)-10 :: BitVector WORD_SIZE_IN_BITS))
{-# INLINE smallValueBV #-}

largeValue1 :: Unsigned (3*WORD_SIZE_IN_BITS)
largeValue1 = $(lift (2^(2*WORD_SIZE_IN_BITS :: Int)-10 :: Unsigned (3*WORD_SIZE_IN_BITS)))
{-# INLINE largeValue1 #-}

largeValue2 :: Unsigned (3*WORD_SIZE_IN_BITS)
largeValue2 = $(lift (2^(2*WORD_SIZE_IN_BITS :: Int)-100 :: Unsigned (3*WORD_SIZE_IN_BITS)))
{-# INLINE largeValue2 #-}

fromIntegerBench :: Benchmark
fromIntegerBench = env setup $ \m ->
  bench "fromInteger WORD_SIZE_IN_BITS" $
  nf (fromInteger :: Integer -> Unsigned WORD_SIZE_IN_BITS) m
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

packBench :: Benchmark
packBench = env setup $ \m ->
  bench "pack WORD_SIZE_IN_BITS" $ nf pack m
  where
    setup = return smallValue1

unpackBench :: Benchmark
unpackBench = env setup $ \m ->
  bench "unpack WORD_SIZE_IN_BITS" $
  nf (unpack :: BitVector WORD_SIZE_IN_BITS -> Unsigned WORD_SIZE_IN_BITS) m
  where
    setup = return smallValueBV

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

unsigned8toWord8Bench :: Benchmark
unsigned8toWord8Bench = env setup $ \m ->
  bench "unsigned8toWord8 WORD_SIZE_IN_BITS" $ nf (bitCoerce :: Unsigned 8 -> Word8) m
  where
    setup = return smallValueW8

unsigned16toWord16Bench :: Benchmark
unsigned16toWord16Bench = env setup $ \m ->
  bench "unsigned16toWord16 WORD_SIZE_IN_BITS" $ nf (bitCoerce :: Unsigned 16 -> Word16) m
  where
    setup = return smallValueW16

unsigned32toWord32Bench :: Benchmark
unsigned32toWord32Bench = env setup $ \m ->
  bench "unsigned32toWord32 WORD_SIZE_IN_BITS" $ nf (bitCoerce :: Unsigned 32 -> Word32) m
  where
    setup = return smallValueW32

unsignedToWordBench :: Benchmark
unsignedToWordBench = env setup $ \m ->
  bench "unsignedToWord WORD_SIZE_IN_BITS" $ nf (bitCoerce :: Unsigned WORD_SIZE_IN_BITS -> Word) m
  where
    setup = return smallValue1
