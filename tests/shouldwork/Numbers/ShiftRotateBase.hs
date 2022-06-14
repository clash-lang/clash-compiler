{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module ShiftRotateBase where
import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Word
import Data.Int

testpattern = 0b1010011000

-- test with shift/rotate amounts: 0..16,200, and a really large
-- non-power-of-two.
amounts :: Vec _ Int
amounts = $(listToVecTH [0::Int ..16]) :< 200 :< (maxBound - 5) :< bit 30 :< bit 31

testall v i
  = ( testAs @(Unsigned 8)  v i
    , testAs @(Signed 8)    v i
    , testAs @(BitVector 8) v i
    , testAs @(Word8)       v i
    , testAs @(Int8)        v i
    , testAs @(Unsigned 70) v i
    )
{-# NOINLINE testall #-}

testAs
  :: (Num b, Bits b) => Integer -> Int -> Vec Ops b
testAs v i = map (\f -> f (fromInteger v) i) shiftsAndRots

type Ops = 4

shiftsAndRots :: Bits a => Vec Ops (a -> Int -> a)
shiftsAndRots = shiftL :> shiftR :> rotateL :> rotateR :> Nil
