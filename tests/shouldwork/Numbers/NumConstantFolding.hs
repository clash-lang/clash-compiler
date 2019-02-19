{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE StandaloneDeriving    #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

-- | Test constant folding of most primitives on "Num" types
--
-- Any number found in the HDL between [22000,22999] in considered unfolded
-- and produces an error.

module NumConstantFolding where
import Clash.Prelude

import Data.Semigroup ((<>))
import Data.Int
import Data.Word
import GHC.Natural

import Control.Monad (when)
import Data.Char (toLower,toUpper)
import qualified Data.List as L
import Data.Maybe
import System.Environment (getArgs)
import System.Exit
import System.IO
import Text.Parser.Combinators
import Text.Trifecta
import Text.Trifecta.Delta

-----------------------------
-- Tests for specific classes
-----------------------------

cNum :: forall n. Num n => _
cNum = (rPlus,rMin,rTimes,rAbsP,rAbsZ, rSignumP,rSignumZ)
  where
    rPlus    = (+)    @n 22101 22102
    rMin     = (-)    @n 22104 22103
    rTimes   = (*)    @n 22105 2
    rAbsP    = abs    @n 22106 + 1000
    rAbsZ    = (22107 + abs    @n 0) + 1000
    -- rAbsN    = abs    @n (-22108) + 1000
    rSignumP = signum @n 22109
    rSignumZ = signum @n 0 * 22110
    -- rSignumN = signum @n (-22111)
    -- fromInteger?

cEq :: forall n. (Num n, Eq n) => (Bool,Bool)
cEq = (eq,neq)
  where
    eq  = (==) @n 22120 22121
    neq = (/=) @n 22122 22123

cOrd :: forall n. (Num n, Ord n) => _
cOrd = (rCompare,rLt,rLe,rGt,rGe,rMin,rMax)
  where
    rCompare = compare @n 22130 22131
    rLt      = (<)     @n 22132 22133
    rLe      = (<=)    @n 22134 22135
    rGt      = (>)     @n 22136 22137
    rGe      = (>=)    @n 22138 22139
    rMin     = min     @n 22140 1234
    rMax     = max     @n 22141 29931

cIntegral :: forall n. (Num n, Integral n) => _
cIntegral = (rQuot,rRem,rDiv,rMod,rQuotRem,rDivMod)
  where
    rQuot    = quot    @n 22150 41
    rRem     = rem     @n 22151 42
    rDiv     = div     @n 22152 43
    rMod     = mod     @n 22153 44
    rQuotRem = quotRem @n 22154 45
    rDivMod  = divMod  @n 22155 46
    -- toInteger?

cBitsNoPopCount :: forall n. (Num n, Bits n) => _
-- the nested tuple here is so we can still 'show' the result
cBitsNoPopCount = ((r00,r01,r02,r03,r04,r05,r06,r07,r08,r09,r10),r11,r12,r13,r14,r15,r16,r17,r18,r19)
  where
    r00 = (.&.)         @n 22160 51
    r01 = (.|.)         @n 22161 6144
    r02 = xor           @n 22162 22153
    r03 = complement    @n 22163 + 1000
    r04 = shift         @n 22164 3
    r05 = rotate        @n 22165 4
    r06 = (22156 + zeroBits @n) + 1000
    r07 = bit           @n 10 + 22167
    r08 = setBit        @n 22168 11
    r09 = clearBit      @n 22169 14
    r10 = complementBit @n 22170 12
    r11 = testBit       @n 22171 1
    r12 = bitSizeMaybe  @n 22172
    r13 = isSigned      @n 22173
    r14 = shiftL        @n 22174 5
    r15 = unsafeShiftL  @n 22175 7
    r16 = shiftR        @n 22176 9
    r17 = unsafeShiftR  @n 22177 11
    r18 = rotateL       @n 22178 13
    r19 = rotateR       @n 22179 15

cBits :: forall n. (Num n, Bits n) => _
cBits = (cBitsNoPopCount @n, r20)
  where
    r20 = popCount      @n 22180


cFiniteBits :: forall n. (Num n, FiniteBits n) => _
cFiniteBits = (r1,r2,r3)
  where
    r1 = finiteBitSize      @n 22190
    r2 = countLeadingZeros  @n 22191
    r3 = countTrailingZeros @n 22192
    -- TODO countLeadingZeros and countTrailingZeros for all clash types
    -- are implemented with folds and bv2v both of which don't constantfold


cExtendingNum :: forall a b. (Num a, Num b, ExtendingNum a b) => _
cExtendingNum = (r1,r2,r3)
  where
    r1 = add @a @b 22200 22201
    r2 = sub @a @b 22203 22202
    r3 = mul @a @b 22204 2

cSaturatingNum :: forall n. (Num n, SaturatingNum n) => _
cSaturatingNum = (r1a,r1b,r1c,r1d, r2a,r2b,r2c,r2d, r3a,r3b,r3c,r3d)
  where
    r1a = satAdd @n SatWrap      22210 22211
    r2a = satSub @n SatWrap      22212 22213
    r3a = satMul @n SatWrap      22214 22215
    r1b = satAdd @n SatBound     22220 22221
    r2b = satSub @n SatBound     22222 22223
    r3b = satMul @n SatBound     22224 22225
    r1c = satAdd @n SatZero      22230 22231
    r2c = satSub @n SatZero      22232 22233
    r3c = satMul @n SatZero      22234 22235
    r1d = satAdd @n SatSymmetric 22240 22241
    r2d = satSub @n SatSymmetric 22242 22243
    r3d = satMul @n SatSymmetric 22244 22245

cBitPack :: forall n. (Num n, BitPack n, KnownNat (BitSize n)) => _
cBitPack = (r1,r2)
  where
    r1 = pack @n 22250 + 1000
    r2 = unpack @n 22251 + 1000

cResize :: forall ty rep size. (ty ~ rep size, Resize rep, KnownNat size, Num (rep size), Num (rep (1+size)), Num (rep (size+1))) => _
cResize = (r1,r2,r3,r4,r5,r6)
  where
    r1 = resize     @rep @size @(size+1) 22260 + 1000
    r2 = resize     @rep @(size+1) @size 22261 + 1000
    r3 = extend     @rep @size @1        22262 + 1000
    r4 = zeroExtend @rep @size @1        22263 + 1000
    r5 = signExtend @rep @size @1        22264 + 1000
    r6 = truncateB  @rep @size @1        22265 + 1000

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

csClashSpecific :: forall n. (Num n, BitPack n, KnownNat (BitSize n), ExtendingNum n n, SaturatingNum n) => _
csClashSpecific = (cBitPack @n, cExtendingNum @n @n, cSaturatingNum @n)


------------------------
-- Test individual types
------------------------
tUnsigned16
  = ( cNum            @(Unsigned 16)
    , cEq             @(Unsigned 16)
    , cOrd            @(Unsigned 16)
    , cIntegral       @(Unsigned 16)
    , cBits           @(Unsigned 16)
    -- , cFiniteBits  @(Unsigned 16) -- broken
    , csClashSpecific @(Unsigned 16)
    , cResize         @(Unsigned 16)
    )

tSigned16
  = ( cNum            @(Signed 16)
    , cEq             @(Signed 16)
    , cOrd            @(Signed 16)
    , cIntegral       @(Signed 16)
    , cBits           @(Signed 16)
    -- , cFiniteBits  @(Signed 16) -- broken
    , csClashSpecific @(Signed 16)
    , cResize         @(Signed 16)
    )

tBitVector16
  = ( cNum            @(BitVector 16)
    , cEq             @(BitVector 16)
    , cOrd            @(BitVector 16)
    , cIntegral       @(BitVector 16)
    , cBits           @(BitVector 16)
    -- , cFiniteBits  @(BitVector 16) -- broken
    , csClashSpecific @(BitVector 16)
    , cResize         @(BitVector 16)
    )

tIndex
  = ( cNum        @(Index 50000)
    , cEq         @(Index 50000)
    , cOrd        @(Index 50000)
    , cIntegral   @(Index 50000)
    -- no Bits, FiniteBits
    , csClashSpecific @(Index 50000)
    , cResize         @(Index 50000)
    )
tSFixed
  = ( cNum            @(SFixed 16 0)
    , cEq             @(SFixed 16 0)
    , cOrd            @(SFixed 16 0)
    -- no Integral
    , cBits           @(SFixed 16 0)
    -- , cFiniteBits     @(SFixed 16 0) -- broken
    , csClashSpecific @(SFixed 16 0)
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



bvSpecific = (r1,r2,r3,r4)
  where
    r1 = (22001 :: BitVector 16) ++# (22002 :: BitVector 16)
    r2 = 22003 - size# (22004::BitVector 16)
    r3 = 22005 - maxIndex# (22006::BitVector 16)
    r4 = (22007 :: BitVector 16) ! 0


fromIntegralConversions
  = ( convertTo @Integer
    , convertTo @Int
    , convertTo @Int8
    , convertTo @Int16
    , convertTo @Int32
    , convertTo @Int64
    , convertTo @Word
    , convertTo @Word8
    , convertTo @Word16
    , convertTo @Word32
    , convertTo @Word64
    , convertTo @(Signed 16)
    , convertTo @(Unsigned 16)
    , convertTo @(BitVector 16)
    , convertTo @(Index 30000)
    )
    where
      convertTo :: forall b. Num b => _
      convertTo = ((r00,r01,r02,r03,r04,r05,r06,r07,r08,r09,r10),r11,r12,r13,r14)
        where
          r00 = 22010 - fromIntegral @Integer        @b 100
          r01 = 22011 - fromIntegral @Int            @b 100
          r02 = 22012 - fromIntegral @Int8           @b 100
          r03 = 22013 - fromIntegral @Int16          @b 100
          r04 = 22014 - fromIntegral @Int32          @b 100
          r05 = 22015 - fromIntegral @Int64          @b 100
          r06 = 22016 - fromIntegral @Word           @b 100
          r07 = 22017 - fromIntegral @Word8          @b 100
          r08 = 22018 - fromIntegral @Word16         @b 100
          r09 = 22019 - fromIntegral @Word32         @b 100
          r10 = 22020 - fromIntegral @Word64         @b 100
          r11 = 22021 - fromIntegral @(Signed 17)    @b 100
          r12 = 22022 - fromIntegral @(Unsigned 17)  @b 100
          r13 = 22023 - fromIntegral @(BitVector 17) @b 100
          r14 = 22024 - fromIntegral @(Index 30000)  @b 100


topEntity
 = ( tUnsigned16
   , tSigned16
   , tBitVector16
   , tIndex
   , tSFixed
   , tUFixed
   , tInt
   , tInt16
   , tWord16
   , tInteger
   , tNatural
   , bvSpecific
   , fromIntegralConversions
   )
{-# NOINLINE topEntity #-}


---------------
-- output tests
---------------
mainVHDL = checkForUnfolded vhdlNr
mainVerilog = checkForUnfolded verilogNr
mainSystemVerilog = checkForUnfolded verilogNr

checkForUnfolded nrParser = do
  [topFile] <- getArgs
  content <- readFile topFile
  case parseString (allNrs nrParser) mempty content of
    Failure err -> die ("Parsing failed with: " <> show err)
    Success nrs -> case filter isUnfolded nrs of
                     [] -> hPutStrLn stderr ("Checked: " <> show (L.length nrs) <> " literals")
                     unfolded -> die ("Error: found unfolded constants:" <> show unfolded)

readDec,readHex,readBin :: String -> Integer
readDec = read
readHex = read . ("0x" <>)
readBin = go 0
  where
    go x [] = x
    go x ('0':bs) = go (x*2) bs
    go x ('1':bs) = go (x*2+1) bs

data Nr = Dec String
        | Hex String
        | Bin String
        deriving Show

nrValue :: Nr -> Integer
nrValue nr = case nr of
  Bin ds -> readBin ds
  Dec ds -> readDec ds
  Hex ds -> readHex ds

decNr = Dec <$> some digit

binDigit = satisfyRange '0' '1'

verilogBinNr = char '\'' *> skipOptional (chari 's') *> chari 'b' *> (Bin <$> some binDigit)
verilogHexNr = char '\'' *> skipOptional (chari 's') *> chari 'h' *> (Hex <$> some hexDigit)
verilogNr = choice [verilogBinNr, verilogHexNr, decNr]

vhdlHexNr = chari 'x' *> char '"' *> (Hex <$> some hexDigit) <* char '"'
vhdlBinNr = char '"' *> (Hex <$> some binDigit) <* char '"'
vhdlNr = choice [vhdlHexNr, vhdlBinNr, decNr]

-- | Parse char case-insensitive
chari c = oneOf [toLower c, toUpper c]

isUnfolded :: Nr -> Bool
isUnfolded (nrValue -> n) = 22000 <= n && n < 23000

allNrs :: CharParsing m => m Nr -> m [Nr]
allNrs nrParser = catMaybes <$> many maybeNr <* eof
  where
    maybeNr = Just <$> try nrParser
              <|> const Nothing <$> anyChar




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
deriving instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h, Lift i, Lift j, Lift k, Lift l, Lift m)
      => Lift (a,b,c,d,e,f,g,h,i,j,k,l,m)
deriving instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h, Lift i, Lift j, Lift k, Lift l, Lift m, Lift n)
      => Lift (a,b,c,d,e,f,g,h,i,j,k,l,m,n)
deriving instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g, Lift h, Lift i, Lift j, Lift k, Lift l, Lift m, Lift n, Lift o)
      => Lift (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
deriving instance Lift Ordering
