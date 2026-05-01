{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UnboxedTuples #-}

-- Prevent GHC constant folding
{-# OPTIONS_GHC -O0 #-}

module T3234 where

import Clash.Prelude
import Clash.Annotations.Primitive
import Clash.Explicit.Testbench

import Data.Int (Int8, Int16, Int32, Int64)
import Data.String.Interpolate (__i)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Exts
  ( Int(I#), Word(W#)
  , intToInt8#, intToInt16#, intToInt32#, intToInt64#
  , wordToWord8#, wordToWord16#, wordToWord32#, wordToWord64#
  , quotInt8#, remInt8#, quotRemInt8#
  , quotInt16#, remInt16#, quotRemInt16#
  , quotInt32#, remInt32#, quotRemInt32#
  , quotInt64#, remInt64#
  , quotWord8#, remWord8#, quotRemWord8#
  , quotWord16#, remWord16#, quotRemWord16#
  , quotWord32#, remWord32#, quotRemWord32#
  , quotWord64#, remWord64#
  )
import GHC.Int (Int8(I8#), Int16(I16#), Int32(I32#), Int64(I64#))
import GHC.Word (Word8(W8#), Word16(W16#), Word32(W32#), Word64(W64#))

import qualified Prelude as P

-- shift/rotate on BitVector/Signed/Unsigned with negative shift amount
bvShiftL, bvShiftR, bvRotateL, bvRotateR :: BitVector 15
bvShiftL  = (1 :: BitVector 15) `shiftL`  negate 1
bvShiftR  = (1 :: BitVector 15) `shiftR`  negate 1
bvRotateL = (1 :: BitVector 15) `rotateL` negate 1
bvRotateR = (1 :: BitVector 15) `rotateR` negate 1

sShiftL, sShiftR, sRotateL, sRotateR :: Signed 15
sShiftL  = (1 :: Signed 15) `shiftL`  negate 1
sShiftR  = (1 :: Signed 15) `shiftR`  negate 1
sRotateL = (1 :: Signed 15) `rotateL` negate 1
sRotateR = (1 :: Signed 15) `rotateR` negate 1

uShiftL, uShiftR, uRotateL, uRotateR :: Unsigned 15
uShiftL  = (1 :: Unsigned 15) `shiftL`  negate 1
uShiftR  = (1 :: Unsigned 15) `shiftR`  negate 1
uRotateL = (1 :: Unsigned 15) `rotateL` negate 1
uRotateR = (1 :: Unsigned 15) `rotateR` negate 1

-- Boxed Int div/quot/rem/mod (via GHC.Classes.divInt# and
-- GHC.Base.{quotInt,remInt,divInt,modInt})
intDivInt, intQuotInt, intRemInt, intDivInt2, intModInt :: Int
intDivInt  = 1 `div`  0
intQuotInt = 1 `quot` 0
intRemInt  = 1 `rem`  0
intDivInt2 = (-1) `div` 0
intModInt  = 1 `mod` 0

-- (^) with negative exponent: hits one of the GHC.Real.^_* specializations.
powInt :: Int
powInt = 2 P.^ (negate 1 :: Int)

-- Sized Int/Word quot/rem/quotRem, calling the unboxed GHC.Prim primitives
-- directly so the evaluator's matching arms actually fire (rather than going
-- through high-level `quot`/`rem` that GHC may rewrite).

i8 :: Int -> Int8
i8 (I# i) = I8# (intToInt8# i)

i16 :: Int -> Int16
i16 (I# i) = I16# (intToInt16# i)

i32 :: Int -> Int32
i32 (I# i) = I32# (intToInt32# i)

i64 :: Int -> Int64
i64 (I# i) = I64# (intToInt64# i)

w8 :: Word -> Word8
w8 (W# w) = W8# (wordToWord8# w)

w16 :: Word -> Word16
w16 (W# w) = W16# (wordToWord16# w)

w32 :: Word -> Word32
w32 (W# w) = W32# (wordToWord32# w)

w64 :: Word -> Word64
w64 (W# w) = W64# (wordToWord64# w)

i8q, i8r, i8qR, i8rR :: Int8
i8q  = I8# (quotInt8# (intToInt8# 1#) (intToInt8# 0#))
i8r  = I8# (remInt8#  (intToInt8# 1#) (intToInt8# 0#))
i8qR = case quotRemInt8# (intToInt8# 1#) (intToInt8# 0#) of (# q, _ #) -> I8# q
i8rR = case quotRemInt8# (intToInt8# 1#) (intToInt8# 0#) of (# _, r #) -> I8# r

i16q, i16r, i16qR, i16rR :: Int16
i16q  = I16# (quotInt16# (intToInt16# 1#) (intToInt16# 0#))
i16r  = I16# (remInt16#  (intToInt16# 1#) (intToInt16# 0#))
i16qR = case quotRemInt16# (intToInt16# 1#) (intToInt16# 0#) of (# q, _ #) -> I16# q
i16rR = case quotRemInt16# (intToInt16# 1#) (intToInt16# 0#) of (# _, r #) -> I16# r

i32q, i32r, i32qR, i32rR :: Int32
i32q  = I32# (quotInt32# (intToInt32# 1#) (intToInt32# 0#))
i32r  = I32# (remInt32#  (intToInt32# 1#) (intToInt32# 0#))
i32qR = case quotRemInt32# (intToInt32# 1#) (intToInt32# 0#) of (# q, _ #) -> I32# q
i32rR = case quotRemInt32# (intToInt32# 1#) (intToInt32# 0#) of (# _, r #) -> I32# r

i64q, i64r :: Int64
i64q = I64# (quotInt64# (intToInt64# 1#) (intToInt64# 0#))
i64r = I64# (remInt64#  (intToInt64# 1#) (intToInt64# 0#))

w8q, w8r, w8qR, w8rR :: Word8
w8q  = W8# (quotWord8# (wordToWord8# 1##) (wordToWord8# 0##))
w8r  = W8# (remWord8#  (wordToWord8# 1##) (wordToWord8# 0##))
w8qR = case quotRemWord8# (wordToWord8# 1##) (wordToWord8# 0##) of (# q, _ #) -> W8# q
w8rR = case quotRemWord8# (wordToWord8# 1##) (wordToWord8# 0##) of (# _, r #) -> W8# r

w16q, w16r, w16qR, w16rR :: Word16
w16q  = W16# (quotWord16# (wordToWord16# 1##) (wordToWord16# 0##))
w16r  = W16# (remWord16#  (wordToWord16# 1##) (wordToWord16# 0##))
w16qR = case quotRemWord16# (wordToWord16# 1##) (wordToWord16# 0##) of (# q, _ #) -> W16# q
w16rR = case quotRemWord16# (wordToWord16# 1##) (wordToWord16# 0##) of (# _, r #) -> W16# r

w32q, w32r, w32qR, w32rR :: Word32
w32q  = W32# (quotWord32# (wordToWord32# 1##) (wordToWord32# 0##))
w32r  = W32# (remWord32#  (wordToWord32# 1##) (wordToWord32# 0##))
w32qR = case quotRemWord32# (wordToWord32# 1##) (wordToWord32# 0##) of (# q, _ #) -> W32# q
w32rR = case quotRemWord32# (wordToWord32# 1##) (wordToWord32# 0##) of (# _, r #) -> W32# r

w64q, w64r :: Word64
w64q = W64# (quotWord64# (wordToWord64# 1##) (wordToWord64# 0##))
w64r = W64# (remWord64#  (wordToWord64# 1##) (wordToWord64# 0##))

ext :: forall a. (BitPack a, KnownNat (BitSize a)) => a -> BitVector 13
ext = resize . pack

results ::
  ( Vec 4 (BitVector 15)
  , Vec 4 (Signed 15)
  , Vec 4 (Unsigned 15)
  , Vec 34 (BitVector 13)
  )
results =
  ( bvShiftL :> bvShiftR :> bvRotateL :> bvRotateR :> Nil
  , sShiftL  :> sShiftR  :> sRotateL  :> sRotateR  :> Nil
  , uShiftL  :> uShiftR  :> uRotateL  :> uRotateR  :> Nil
  ,    ext intDivInt
    :> ext intQuotInt
    :> ext intRemInt
    :> ext intDivInt2
    :> ext intModInt
    :> ext powInt
    :> ext i8q  :> ext i8r  :> ext i8qR  :> ext i8rR
    :> ext i16q :> ext i16r :> ext i16qR :> ext i16rR
    :> ext i32q :> ext i32r :> ext i32qR :> ext i32rR
    :> ext i64q :> ext i64r
    :> ext w8q  :> ext w8r  :> ext w8qR  :> ext w8rR
    :> ext w16q :> ext w16r :> ext w16qR :> ext w16rR
    :> ext w32q :> ext w32r :> ext w32qR :> ext w32rR
    :> ext w64q :> ext w64r
    :> Nil
  )

topEntity :: Signal System (BitVector (12 * 15 + 34 * 13))
topEntity = pure (pack results)
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  done = checkAllDontCare clk topEntity
  clk  = tbSystemClockGen (not <$> done)
{-# OPAQUE testBench #-}
{-# ANN testBench (TestBench 'topEntity) #-}

-- Only call from an HDL test bench: the Haskell model is intentionally trivial,
-- while the Verilog primitive checks the simulated value with case equality.
checkAllDontCare
  :: KnownNat n
  => Clock System
  -> Signal System (BitVector n)
  -> Signal System Bool
checkAllDontCare _ _ = pure True
{-# OPAQUE checkAllDontCare #-}
{-# ANN checkAllDontCare (InlineYamlPrimitive [Verilog] [__i|
  BlackBox:
    name: T3234.checkAllDontCare
    kind: Declaration
    template: |-
      always @(posedge ~ARG[1]) begin
        if (~ARG[2] !== {~SIZE[~TYP[2]] {1'bx}}) begin
          $display("T3234: expected all dont-care bits, actual: %b", ~ARG[2]);
          $finish;
        end
      end

      assign ~RESULT = (~ARG[2] === {~SIZE[~TYP[2]] {1'bx}});
  |]) #-}
