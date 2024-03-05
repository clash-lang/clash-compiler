{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
#if __GLASGOW_HASKELL__ < 900
{-# LANGUAGE AllowAmbiguousTypes #-}
#endif

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-|
This checks that the methods of the Bits and FiniteBits typeclasses
do the same thing in HDL and Haskell.
The checks are split up into multiple parts to work around maximum
width issues in the $display function of verilator.
-}
module Bits where
import Clash.Prelude
import Clash.Explicit.Testbench
import Data.Int
import Data.Word
import Data.Bits

testBits :: forall a. (Num a, Bits a) => Integer -> Integer -> _
testBits x0@(fromInteger -> x) y0 =
  ( testBitsNoPopCount @a x0 y0
  , popCount @a x
  , bitSize @a x  -- deprecated and undefined for Integer
  )

testBitsNoPopCount :: forall a. (Num a, Bits a) => Integer -> Integer -> _
testBitsNoPopCount (fromInteger @a -> x) y0@(fromInteger @a -> y) =
  (
    ( (.&.) @a x y
    , (.|.) @a x y
    , xor @a x y
    , complement @a x
    , shift @a x yInt
    , rotate @a x yInt
    , zeroBits @a
    )
  , ( bit @a yInt
    , setBit @a x yInt
    , clearBit @a x yInt
    , complementBit @a x yInt
    , testBit @a x yInt
    , bitSizeMaybe @a x
    , isSigned @a x
    )
  , ( shiftL @a x yInt
    , unsafeShiftL @a x yInt
    , shiftR @a x yInt
    , unsafeShiftR @a x yInt
    , rotateL @a x yInt
    , rotateR @a x yInt
    )
  )
 where
  yInt :: Int
  yInt = (abs (fromInteger y0)) `mod` sz
  sz = case bitSizeMaybe x of
    Nothing -> 64
    Just x -> fromIntegral x

testFiniteBits :: forall a. (Num a, FiniteBits a) => Integer -> Integer -> _
testFiniteBits x0@(fromInteger -> x) y0@(fromInteger -> y) =
  ( testBits @a x0 y0
  , finiteBitSize @a x
  , countLeadingZeros @a x
  , countTrailingZeros @a x
  )

topEntity1 :: (Integer, Integer) -> _
topEntity1 (x,y) =
    ( testBitsNoPopCount @Integer x y
    , testFiniteBits @Int x y
    , testFiniteBits @Int8 x y
    , testFiniteBits @Int16 x y
    -- , testFiniteBits @Int32 x y -- TODO triggers GHDL bug #2618, re-enable after we get another GHDL
    , testFiniteBits @Int64 x y
    )

topEntity2 :: (Integer, Integer) -> _
topEntity2 (x,y) =
    ( testFiniteBits @Word x y
    , testFiniteBits @Word8 x y
    , testFiniteBits @Word16 x y
    , testFiniteBits @Word32 x y
    , testFiniteBits @Word64 x y
    )

topEntity3 :: (Integer, Integer) -> _
topEntity3 (x,y) =
    ( testFiniteBits @(Signed 66) x y
    , testFiniteBits @(Unsigned 66) (abs x) (abs y)
    , testFiniteBits @(BitVector 66) (abs x) (abs y)
    , testFiniteBits @(Index 128) (abs x `mod` 128) (abs y `mod` 128)
    )

inputs :: Vec _ (Integer,Integer)
inputs =
  $(let range = [0,42, (fromIntegral @Int64 maxBound), (fromIntegral @Int64 minBound)
                , -1, -123] :: [Integer]
    in listToVecTH [(a,b) | a <- range, b <- range])
