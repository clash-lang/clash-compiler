{-# LANGUAGE CPP #-}

module ShiftRotateNegative where

#if MIN_VERSION_base(4,15,0)
import GHC.Num.Natural (Natural)
#else
import GHC.Natural (Natural)
#endif

import Clash.Prelude
import Clash.Explicit.Testbench
import Clash.Sized.Internal.BitVector

testBench :: Signal System Bool
testBench = done
 where
  done = expectedOutput (pack . topEntity <$> testInput)
  clk  = tbSystemClockGen (not <$> done)
  rst  = systemResetGen

  testInput =
    stimuliGenerator clk rst $
      (0b11110000111100001111000011110000, 1) :> Nil

  expectedOutput =
    outputVerifierBitVector' @1 clk rst (pack testOutput :> Nil)

  -- Integer and Natural are stored as BitVector 64: this is how they render in
  -- HDL, and they have no BitPack instance needed to use 'pack'.
  testOutput
    :: Vec 2
         ( Int
         , Word
#if MIN_VERSION_base(4,15,0)
         , BitVector 64
         , BitVector 64
#endif
         , Bit
         , BitVector 32
         , Signed 32
         , Unsigned 32
         )
  testOutput =
    -- Exceptions when shiftL / shiftR gets a negative count.
    ( deepErrorX "X"
    , deepErrorX "X"
#if MIN_VERSION_base(4,15,0)
    , deepErrorX "X"
    , deepErrorX "X"
#endif
    , 0
    , deepErrorX "X"
    , deepErrorX "X"
    , deepErrorX "X"
    ) :>
    -- shift accepts negative counts.
    ( 4042322160
    , 4042322160
#if MIN_VERSION_base(4,15,0)
    , 4042322160
    , 4042322160
#endif
    , 0
    , 0b11110000111100001111000011110000
    , (-252645135)
    , 4042322160
    ) :> Nil

{-# NOINLINE topEntity #-}
topEntity
  :: (BitVector 32, Int)
  -> Vec 2
       ( Int
       , Word
#if MIN_VERSION_base(4,15,0)
       , BitVector 64
       , BitVector 64
#endif
       , Bit
       , BitVector 32
       , Signed 32
       , Unsigned 32
       )
topEntity (x, i) =
  ( f (fromIntegral x) i
  , f (fromIntegral x) i
#if MIN_VERSION_base(4,15,0)
  , fromIntegral $! f (fromIntegral @_ @Integer x) i
  , BV 0 $! f (unsafeToNatural x) i
#endif
  , f (x!0) i
  , f x i
  , f (unpack x) i
  , f (unpack x) i
  ) :>
  ( g (fromIntegral x) i
  , g (fromIntegral x) i
#if MIN_VERSION_base(4,15,0)
  , fromIntegral $! g (fromIntegral @_ @Integer x) i
  , BV 0 $! g (unsafeToNatural x) i
#endif
  , g (x!0) i
  , g x i
  , g (unpack x) i
  , g (unpack x) i
  ) :>
  Nil

f :: (Bits a) => a -> Int -> a
f x i = x `shiftL` (-i) `rotateL` (-i) `shiftR` (-i) `rotateR` (-i)

g :: (Bits a) => a -> Int -> a
g x i = x `shift` (-i) `rotate` (-i) `shift` i `rotate` i
