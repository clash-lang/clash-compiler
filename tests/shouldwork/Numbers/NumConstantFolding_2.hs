{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- | Test constant folding of most primitives on "Num" types
--
-- Any number found in the HDL between [22000,22999] is considered unfolded
-- and is reported as an error.

module NumConstantFolding_2 (topEntity, module ConstantFoldingUtil) where
import Clash.Prelude
import ConstantFoldingUtil

import Data.Int
import Data.Word
import GHC.Natural

-----------------------------
-- Tests for specific classes
-----------------------------

cNum :: forall n. Num n => _
cNum = (rPlus,rMin,rTimes,rAbsP,rAbsZ, rSignumP,rSignumZ)
  where
    rPlus    = (+)    @n (lit 22101) (lit 22102)
    rMin     = (-)    @n (lit 22104) (lit 22103)
    rTimes   = (*)    @n (lit 22105) (lit 2)
    rAbsP    = abs    @n (lit 22106) + (lit 1000)
    rAbsZ    = (lit 22107 + abs    @n (lit 0)) + lit 1000
    -- rAbsN    = abs    @n (-22108) + 1000
    rSignumP = signum @n (lit 22109)
    rSignumZ = signum @n (lit 0) * (lit 22110)
    -- rSignumN = signum @n (-22111)
    -- fromInteger?

cEq :: forall n. (Num n, Eq n) => (Bool,Bool)
cEq = (eq,neq)
  where
    eq  = (==) @n (lit 22120) (lit 22121)
    neq = (/=) @n (lit 22122) (lit 22123)

cOrd :: forall n. (Num n, Ord n) => _
cOrd = (rCompare,rLt,rLe,rGt,rGe,rMin,rMax)
  where
    rCompare = compare @n (lit 22130) (lit 22131)
    rLt      = (<)     @n (lit 22132) (lit 22133)
    rLe      = (<=)    @n (lit 22134) (lit 22135)
    rGt      = (>)     @n (lit 22136) (lit 22137)
    rGe      = (>=)    @n (lit 22138) (lit 22139)
    rMin     = min     @n (lit 22140) (lit 1234)
    rMax     = max     @n (lit 22141) (lit 29931)

cIntegral :: forall n. (Num n, Integral n) => _
cIntegral = (rQuot,rRem,rDiv,rMod,rQuotRem,rDivMod)
  where
    rQuot    = quot    @n (lit 22150) (lit 41)
    rRem     = rem     @n (lit 22151) (lit 42)
    rDiv     = div     @n (lit 22152) (lit 43)
    rMod     = mod     @n (lit 22153) (lit 44)
    rQuotRem = quotRem @n (lit 22154) (lit 45)
    rDivMod  = divMod  @n (lit 22155) (lit 46)
    -- toInteger?

cBitsNoPopCount :: forall n. (Num n, Bits n) => _
-- the nested tuple here is so we can still 'show' the result
cBitsNoPopCount = ((r00,r01,r02,r03,r04,r05,r06,r07,r08,r09,r10),r11,r12,r13,r14,r15,r16,r17,r18,r19)
  where
    r00 = (.&.)         @n (lit 22160) (lit 51)
    r01 = (.|.)         @n (lit 22161) (lit 6144)
    r02 = xor           @n (lit 22162) (lit 22181)
    r03 = complement    @n (lit 22163) + (lit 1000)
    r04 = shift         @n (lit 22164) (lit 3)
    r05 = rotate        @n (lit 22165) (lit 4)
    r06 = (22156 + zeroBits @n) + (lit 1000)
    r07 = bit           @n (lit 10) + (lit 22167)
    r08 = setBit        @n (lit 22168) (lit 11)
    r09 = clearBit      @n (lit 22169) (lit 14)
    r10 = complementBit @n (lit 22170) (lit 12)
    r11 = testBit       @n (lit 22171) (lit 1)
    r12 = bitSizeMaybe  @n (lit 22172)
    r13 = isSigned      @n (lit 22173)
    r14 = shiftL        @n (lit 22174) (lit 5)
    r15 = unsafeShiftL  @n (lit 22175) (lit 7)
    r16 = shiftR        @n (lit 22176) (lit 9)
    r17 = unsafeShiftR  @n (lit 22177) (lit 11)
    r18 = rotateL       @n (lit 22178) (lit 13)
    r19 = rotateR       @n (lit 22179) (lit 15)

cBits :: forall n. (Num n, Bits n) => _
cBits = (cBitsNoPopCount @n, r20)
  where
    r20 = popCount      @n (lit 22180)


cFiniteBits :: forall n. (Num n, FiniteBits n) => _
cFiniteBits = (r1,r2,r3)
  where
    r1 = finiteBitSize      @n (lit 22190)
    r2 = countLeadingZeros  @n (lit 22191)
    r3 = countTrailingZeros @n (lit 22192)
    -- TODO countLeadingZeros and countTrailingZeros for all clash types
    -- are implemented with folds and bv2v both of which don't constantfold


cExtendingNum :: forall a b. (Num a, Num b, ExtendingNum a b) => _
cExtendingNum = (r1,r2,r3)
  where
    r1 = add @a @b (lit 22200) (lit 22201)
    r2 = sub @a @b (lit 22203) (lit 22202)
    r3 = mul @a @b (lit 22204) (lit 2)

cIndex1 :: _
cIndex1 = (r1a,r1b,r1c,r1d, r2a,r2b,r2c,r2d, r3a,r3b,r3c,r3d)
  where
    f :: Int -> Index 1 -> Int
    f a = \x -> case x of {0 -> 0; 1 -> a}
    r1a = f 22270 (satAdd SatWrap      (lit 0) (lit 0))
    r2a = f 22271 (satSub SatWrap      (lit 0) (lit 0))
    r3a = f 22272 (satMul SatWrap      (lit 0) (lit 0))
    r1b = f 22273 (satAdd SatBound     (lit 0) (lit 0))
    r2b = f 22274 (satSub SatBound     (lit 0) (lit 0))
    r3b = f 22275 (satMul SatBound     (lit 0) (lit 0))
    r1c = f 22276 (satAdd SatZero      (lit 0) (lit 0))
    r2c = f 22277 (satSub SatZero      (lit 0) (lit 0))
    r3c = f 22278 (satMul SatZero      (lit 0) (lit 0))
    r1d = f 22279 (satAdd SatSymmetric (lit 0) (lit 0))
    r2d = f 22280 (satSub SatSymmetric (lit 0) (lit 0))
    r3d = f 22281 (satMul SatSymmetric (lit 0) (lit 0))

cSaturatingNum :: forall n. (Num n, SaturatingNum n) => _
cSaturatingNum = (r1a,r1b,r1c,r1d, r2a,r2b,r2c,r2d, r3a,r3b,r3c,r3d)
  where
    r1a = satAdd @n SatWrap      (lit 22210) (lit 22211)
    r2a = satSub @n SatWrap      (lit 22212) (lit 22213)
    r3a = satMul @n SatWrap      (lit 22214) (lit 22215)
    r1b = satAdd @n SatBound     (lit 22220) (lit 22221)
    r2b = satSub @n SatBound     (lit 22222) (lit 22223)
    r3b = satMul @n SatBound     (lit 22224) (lit 22225)
    r1c = satAdd @n SatZero      (lit 22230) (lit 22231)
    r2c = satSub @n SatZero      (lit 22232) (lit 22233)
    r3c = satMul @n SatZero      (lit 22234) (lit 22235)
    r1d = satAdd @n SatSymmetric (lit 22240) (lit 22241)
    r2d = satSub @n SatSymmetric (lit 22242) (lit 22243)
    r3d = satMul @n SatSymmetric (lit 22244) (lit 22245)

cBitPack :: forall n. (Num n, BitPack n) => _
cBitPack = (r1,r2)
  where
    r1 = pack @n (lit 22250) + (lit 1000)
    r2 = unpack @n (lit 22251) + (lit 1000)

cResize :: forall ty rep size. (ty ~ rep size, Resize rep, KnownNat size, Num (rep size), Num (rep (1+size)), Num (rep (size+1))) => _
cResize = (r1,r2,r3,r4,r5,r6)
  where
    r1 = resize     @rep @size @(size+1) (lit 22260) + (lit 1000)
    r2 = resize     @rep @(size+1) @size (lit 22261) + (lit 1000)
    r3 = extend     @rep @size @1        (lit 22262) + (lit 1000)
    r4 = zeroExtend @rep @size @1        (lit 22263) + (lit 1000)
    r5 = signExtend @rep @size @1        (lit 22264) + (lit 1000)
    r6 = truncateB  @rep @size @1        (lit 22265) + (lit 1000)

-- TODO classes
-- Real?
-- Enum?

-- functions from module Clash.Prelude.BitIndex
--


--------------------------------
-- Group certain classes together
---------------------------------
csGenericHaskell :: forall n. (Num n, Ord n, Integral n, Bits n, FiniteBits n) => _
csGenericHaskell
  = ( cNum        @n
    , cEq         @n
    , cOrd        @n
    , cIntegral   @n
    , cBits       @n
    , cFiniteBits @n
    )

csClashSpecific :: forall n. (Num n, BitPack n, ExtendingNum n n, SaturatingNum n) => _
csClashSpecific = (cBitPack @n, cExtendingNum @n @n, cSaturatingNum @n)


------------------------
-- Test individual types
------------------------

bvSpecific = (r1,r2,r3,r4)
  where
    r1 = (lit 22001 :: BitVector 16) ++# (lit 22002 :: BitVector 16)
    r2 = lit 22003 - size# (lit 22004::BitVector 16)
    r3 = lit 22005 - maxIndex# (lit 22006::BitVector 16)
    r4 = (lit 22007 :: BitVector 16) ! 0

fromIntegralConversions
  = ( ( convertTo @Integer
      , convertTo @Int
      , convertTo @Int8
      , convertTo @Int16
      , convertTo @Int32
      , convertTo @Int64
      )
    , ( convertTo @Word
      , convertTo @Word8
      , convertTo @Word16
      , convertTo @Word32
      , convertTo @Word64
      )
    , ( convertTo @(Signed 16)
      , convertTo @(Unsigned 16)
      , convertTo @(BitVector 16)
      , convertTo @(Index 30000)
      )
    )
    where
      convertTo :: forall b. Num b => _
      convertTo = ((r00,r01,r02,r03,r04,r05,r06,r07,r08,r09,r10),r11,r12,r13,r14)
        where
          r00 = lit 22010 - fromIntegral @Integer        @b (lit 100)
          r01 = lit 22011 - fromIntegral @Int            @b (lit 100)
          r02 = lit 22012 - fromIntegral @Int8           @b (lit 100)
          r03 = lit 22013 - fromIntegral @Int16          @b (lit 100)
          r04 = lit 22014 - fromIntegral @Int32          @b (lit 100)
          r05 = lit 22015 - fromIntegral @Int64          @b (lit 100)
          r06 = lit 22016 - fromIntegral @Word           @b (lit 100)
          r07 = lit 22017 - fromIntegral @Word8          @b (lit 100)
          r08 = lit 22018 - fromIntegral @Word16         @b (lit 100)
          r09 = lit 22019 - fromIntegral @Word32         @b (lit 100)
          r10 = lit 22020 - fromIntegral @Word64         @b (lit 100)
          r11 = lit 22021 - fromIntegral @(Signed 17)    @b (lit 100)
          r12 = lit 22022 - fromIntegral @(Unsigned 17)  @b (lit 100)
          r13 = lit 22023 - fromIntegral @(BitVector 17) @b (lit 100)
          r14 = lit 22024 - fromIntegral @(Index 30000)  @b (lit 100)

tInteger
  = ( cNum            @Integer
    , cEq             @Integer
    , cOrd            @Integer
    , cIntegral       @Integer
    , cBitsNoPopCount @Integer -- popCount @Integer is just crazy
    -- no FiniteBits
    )

tNatural
  = ( cNum        @Natural
    , cEq         @Natural
    , cOrd        @Natural
    , cIntegral   @Natural
    -- , cBits       @Natural -- broken
    -- no FiniteBits
    )

tUFixed
  = ( cNum            @(UFixed 16 0)
    , cEq             @(UFixed 16 0)
    , cOrd            @(UFixed 16 0)
    -- no Integral
    , cBits           @(UFixed 16 0)
    -- , cFiniteBits     @(UFixed 16 0) -- broken
    , csClashSpecific @(UFixed 16 0)
    )

-- TODO Types
-- Bit?

tInt         = csGenericHaskell @Int
tInt16       = csGenericHaskell @Int16
tWord16      = csGenericHaskell @Word16

topEntity
 = ( bvSpecific
   , fromIntegralConversions
   , tInteger
   , tNatural
   , tUFixed
   , tInt
   , tInt16
   , tWord16
   )
{-# NOINLINE topEntity #-}

-- Lift for 8-15 tuples
deriving instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h)
      => Lift (a,b,c,d,e,f,g,h)
deriving instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h, Lift i)
      => Lift (a,b,c,d,e,f,g,h,i)
deriving instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h, Lift i, Lift j)
      => Lift (a,b,c,d,e,f,g,h,i,j)
deriving instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h, Lift i, Lift j, Lift k)
      => Lift (a,b,c,d,e,f,g,h,i,j,k)
deriving instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h, Lift i, Lift j, Lift k, Lift l)
      => Lift (a,b,c,d,e,f,g,h,i,j,k,l)

deriving instance Lift Ordering

