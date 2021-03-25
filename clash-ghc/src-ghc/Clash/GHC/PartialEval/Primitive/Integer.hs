{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.PartialEval.Primitive.Integer
  ( integerPrims
  ) where

#include "MachDeps.h"

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)

#if MIN_VERSION_base(4,15,0)
import Data.Primitive.ByteArray (ByteArray(..))
import GHC.Num.Integer
#else
import GHC.Integer.GMP.Internals
import GHC.Integer.Logarithms
#endif

import GHC.Prim
import GHC.Types

import Clash.GHC.PartialEval.Primitive.Strategy
import Clash.GHC.PartialEval.Primitive.Unboxed

integerPrims :: HashMap Text PrimImpl
integerPrims = HashMap.fromList
#if MIN_VERSION_base(4,15,0)
  [ ("GHC.Num.Integer.IN", primIN)
  , ("GHC.Num.Integer.IP", primIP)
  , ("GHC.Num.Integer.IS", primIS)
  , ("GHC.Num.Integer.integerAbs", liftUnary integerAbs)
  , ("GHC.Num.Integer.integerAdd", liftBinary integerAdd)
  , ("GHC.Num.Integer.integerAnd", liftBinary integerAnd)
  , ("GHC.Num.Integer.integerBit#", primBit)
  , ("GHC.Num.Integer.integerCompare", liftBinary integerCompare)
  , ("GHC.Num.Integer.integerComplement", liftUnary integerComplement)
  , ("GHC.Num.Integer.integerDiv", liftBinary integerDiv)
  , ("GHC.Num.Integer.integerDivMod#", primDivMod)
  , ("GHC.Num.Integer.integerEncodeDouble#", primEncodeDouble)
  , ("GHC.Num.Integer.integerEncodeFloat#", primEncodeFloat)
  , ("GHC.Num.Integer.integerEq", coreUnfolding)
  , ("GHC.Num.Integer.integerEq#", primComparison integerEq#)
  , ("GHC.Num.Integer.integerFromNatural", liftUnary integerFromNatural)
  , ("GHC.Num.Integer.integerFromWord#", primFromWord)
  , ("GHC.Num.Integer.integerGcd", liftBinary integerGcd)
  , ("GHC.Num.Integer.integerGe#", primComparison integerGe#)
  , ("GHC.Num.Integer.integerGt#", primComparison integerGt#)
  , ("GHC.Num.Integer.integerLcm", liftBinary integerLcm)
  , ("GHC.Num.Integer.integerLe#", primComparison integerLe#)
  , ("GHC.Num.Integer.integerLogBase#", coreUnfolding)
  , ("GHC.Num.Integer.integerLt#", primComparison integerLt#)
  , ("GHC.Num.Integer.integerMod", liftBinary integerMod)
  , ("GHC.Num.Integer.integerMul", liftBinary integerMul)
  , ("GHC.Num.Integer.integerNe", coreUnfolding)
  , ("GHC.Num.Integer.integerNe#", primComparison integerNe#)
  , ("GHC.Num.Integer.integerNegate", liftUnary integerNegate)
  , ("GHC.Num.Integer.integerOr", liftBinary integerOr)
  , ("GHC.Num.Integer.integerPopCount#", primPopCount)
  , ("GHC.Num.Integer.integerQuot", liftBinary integerQuot)
  , ("GHC.Num.Integer.integerQuotRem#", primQuotRem)
  , ("GHC.Num.Integer.integerRem", liftBinary integerRem)
  , ("GHC.Num.Integer.integerShiftL#", primShift integerShiftL#)
  , ("GHC.Num.Integer.integerShiftR#", primShift integerShiftR#)
  , ("GHC.Num.Integer.integerSignum", liftUnary integerSignum)
  , ("GHC.Num.Integer.integerSignum#", primSignum)
  , ("GHC.Num.Integer.integerSub", liftBinary integerSub)
  , ("GHC.Num.Integer.integerTestBit#", primTestBit)
  , ("GHC.Num.Integer.integerToDouble#", primToDouble)
  , ("GHC.Num.Integer.integerToFloat#", primToFloat)
  , ("GHC.Num.Integer.integerToInt#", primToInt)
  , ("GHC.Num.Integer.integerToNatural", liftUnary integerToNatural)
  , ("GHC.Num.Integer.integerToNaturalClamp", liftUnary integerToNaturalClamp)
  , ("GHC.Num.Integer.integerToNaturalThrow", liftUnary integerToNaturalThrow)
  , ("GHC.Num.Integer.integerToWord#", primToWord)
  , ("GHC.Num.Integer.integerXor", liftBinary integerXor)

#if WORD_SIZE_IN_BITS == 32
  , ("GHC.Num.Integer.integerFromInt64#", error "TODO: UInt64")
  , ("GHC.Num.Integer.integerFromWord64#", error "TODO: UWord64")
  , ("GHC.Num.Integer.integerToInt64#", error "TODO: UInt64")
  , ("GHC.Num.Integer.integerToWord64#", error "TODO: UWord64")
#endif

  , ("GHC.Num.Integer.$wintegerSignum", primSignum)
  ]
#else
  [ ("GHC.Integer.Logarithms.integerLogBase#", primIntegerLogBase)
  , ("GHC.Integer.Type.$wsignumInteger", primWSignum)
  , ("GHC.Integer.Type.absInteger", liftUnary absInteger)
  , ("GHC.Integer.Type.andInteger", liftBinary andInteger)
  , ("GHC.Integer.Type.bitInteger", primBitInteger)
  , ("GHC.Integer.Type.compareInteger", liftBinary compareInteger)
  , ("GHC.Integer.Type.complementInteger", liftUnary complementInteger)
  , ("GHC.Integer.Type.czeroBigNat", liftNullary (wordToBigNat (not# 0##)))
  , ("GHC.Integer.Type.decodeDoubleInteger", primDecodeDoubleInteger)
  , ("GHC.Integer.Type.divInteger", liftBinary divInteger)
  , ("GHC.Integer.Type.divModInteger", primDivModInteger)
  , ("GHC.Integer.Type.doubleFromInteger", primDoubleFromInteger)
  , ("GHC.Integer.Type.encodeDoubleInteger", primEncodeDoubleInteger)
  , ("GHC.Integer.Type.encodeFloatInteger", primEncodeFloatInteger)
  , ("GHC.Integer.Type.eqInteger", liftBinary eqInteger)
  , ("GHC.Integer.Type.eqInteger#", integerComparison eqInteger#)
  , ("GHC.Integer.Type.floatFromInteger", primFloatFromInteger)
  , ("GHC.Integer.Type.geInteger", liftBinary geInteger)
  , ("GHC.Integer.Type.geInteger#", integerComparison geInteger#)
  , ("GHC.Integer.Type.gtInteger", liftBinary gtInteger)
  , ("GHC.Integer.Type.gtInteger#", integerComparison gtInteger#)
  , ("GHC.Integer.Type.hashInteger", primHashInteger)
  , ("GHC.Integer.Type.integerToInt", primIntegerToInt)
  , ("GHC.Integer.Type.integerToWord", primIntegerToWord)
  , ("GHC.Integer.Type.leInteger", liftBinary leInteger)
  , ("GHC.Integer.Type.leInteger#", integerComparison leInteger#)
  , ("GHC.Integer.Type.ltInteger", liftBinary ltInteger)
  , ("GHC.Integer.Type.ltInteger#", integerComparison ltInteger#)
  , ("GHC.Integer.Type.minusInteger", liftBinary minusInteger)
  , ("GHC.Integer.Type.modInteger", liftBinary modInteger)
  , ("GHC.Integer.Type.negateInteger", liftUnary negateInteger)
  , ("GHC.Integer.Type.neqInteger", liftBinary neqInteger)
  , ("GHC.Integer.Type.neqInteger#", integerComparison neqInteger#)
  , ("GHC.Integer.Type.nullBigNat", liftUndefined)
  , ("GHC.Integer.Type.oneBigNat", liftNullary oneBigNat)
  , ("GHC.Integer.Type.orInteger", liftBinary orInteger)
  , ("GHC.Integer.Type.plusInteger", liftBinary plusInteger)
  , ("GHC.Integer.Type.quotInteger", liftBinary quotInteger)
  , ("GHC.Integer.Type.quotRemInteger", primQuotRemInteger)
  , ("GHC.Integer.Type.remInteger", liftBinary remInteger)
  , ("GHC.Integer.Type.shiftLInteger", primShiftLInteger)
  , ("GHC.Integer.Type.shiftRInteger", primShiftRInteger)
  , ("GHC.Integer.Type.signumInteger", liftUnary signumInteger)
  , ("GHC.Integer.Type.smallInteger", primSmallInteger)
  , ("GHC.Integer.Type.testBitInteger", primTestBitInteger)
  , ("GHC.Integer.Type.timesInteger", liftBinary timesInteger)
  , ("GHC.Integer.Type.wordToInteger", primWordToInteger)
  , ("GHC.Integer.Type.xorInteger", liftBinary xorInteger)
  , ("GHC.Integer.Type.zeroBigNat", liftNullary zeroBigNat)
  , ("GHC.Integer.bitInteger", primBitInteger)
  ]
#endif

#if MIN_VERSION_base(4,15,0)

primIN :: PrimImpl
primIN =
  liftUnary $ \x ->
    let !(UByteArray (ByteArray a)) = x
     in IN a

primIP :: PrimImpl
primIP =
  liftUnary $ \x ->
    let !(UByteArray (ByteArray a)) = x
     in IP a

primIS :: PrimImpl
primIS =
  liftUnary $ \x ->
    let !(UInt (I# a)) = x
     in IS a

primFromWord :: PrimImpl
primFromWord =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x in integerFromWord# a

primBit :: PrimImpl
primBit =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x in integerBit# a

primDivMod :: PrimImpl
primDivMod =
  liftBinary $ \x y ->
    let (# a, b #) = integerDivMod# x y
     in UTuple2 (a, b)

primEncodeDouble :: PrimImpl
primEncodeDouble =
  liftBinary $ \x y ->
    let !(I# a) = y
     in UDouble (D# (integerEncodeDouble# x a))

primEncodeFloat :: PrimImpl
primEncodeFloat =
  liftBinary $ \x y ->
    let !(I# a) = y
     in UFloat (F# (integerEncodeFloat# x a))

primComparison :: (Integer -> Integer -> Int#) -> PrimImpl
primComparison f =
  liftBinary $ \x y -> UInt (I# (f x y))

-- TODO fromInt64#, fromWord64

primPopCount :: PrimImpl
primPopCount =
  liftUnary $ \x -> UInt (I# (integerPopCount# x))

primQuotRem :: PrimImpl
primQuotRem =
  liftBinary $ \x y ->
    let (# a, b #) = integerQuotRem# x y
     in UTuple2 (a, b)

primShift :: (Integer -> Word# -> Integer) -> PrimImpl
primShift f =
  liftBinary $ \x y ->
    let !(UWord (W# a)) = y
     in f x a

primSignum :: PrimImpl
primSignum =
  liftUnary $ \x -> UInt (I# (integerSignum# x))

primTestBit :: PrimImpl
primTestBit =
  liftBinary $ \x y ->
    let !(UWord (W# a)) = y
     in UInt (I# (integerTestBit# x a))

primToDouble :: PrimImpl
primToDouble =
  liftUnary $ \x -> UDouble (D# (integerToDouble# x))

primToFloat :: PrimImpl
primToFloat =
  liftUnary $ \x -> UFloat (F# (integerToFloat# x))

primToInt :: PrimImpl
primToInt =
  liftUnary $ \x -> UInt (I# (integerToInt# x))

primToWord :: PrimImpl
primToWord =
  liftUnary $ \x -> UWord (W# (integerToWord# x))

#else
primWSignum :: PrimImpl
primWSignum =
  liftUnary $ \x -> UInt (fromInteger (signumInteger x))

primIntegerLogBase :: PrimImpl
primIntegerLogBase =
  liftBinary $ \x y -> UInt (I# (integerLogBase# x y))

primSmallInteger :: PrimImpl
primSmallInteger =
  liftUnary $ \x ->
    let !(UInt (I# a)) = x in smallInteger a

primWordToInteger :: PrimImpl
primWordToInteger =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x in wordToInteger a

primIntegerToWord :: PrimImpl
primIntegerToWord =
  liftUnary $ \x -> UWord (W# (integerToWord x))

primIntegerToInt :: PrimImpl
primIntegerToInt =
  liftUnary $ \x -> UInt (I# (integerToInt x))

primEncodeFloatInteger :: PrimImpl
primEncodeFloatInteger =
  liftBinary $ \x y ->
    let !(UInt (I# a)) = y in UFloat (F# (encodeFloatInteger x a))

primFloatFromInteger :: PrimImpl
primFloatFromInteger =
  liftUnary $ \x -> UFloat (F# (floatFromInteger x))

primEncodeDoubleInteger :: PrimImpl
primEncodeDoubleInteger =
  liftBinary $ \x y ->
    let !(UInt (I# a)) = y in UDouble (D# (encodeDoubleInteger x a))

primDecodeDoubleInteger :: PrimImpl
primDecodeDoubleInteger =
  liftUnary $ \x ->
    let !(UDouble (D# a)) = x
        !(# b, c #) = decodeDoubleInteger a
     in UTuple2 (b, UInt (I# c))

primDoubleFromInteger :: PrimImpl
primDoubleFromInteger =
  liftUnary $ \x -> UDouble (D# (doubleFromInteger x))

primQuotRemInteger :: PrimImpl
primQuotRemInteger =
  liftBinary $ \x y ->
    let !(# a, b #) = quotRemInteger x y
     in UTuple2 (a, b)

primDivModInteger :: PrimImpl
primDivModInteger =
  liftBinary $ \x y ->
    let !(# a, b #) = divModInteger x y
     in UTuple2 (a, b)

primBitInteger :: PrimImpl
primBitInteger =
  liftUnary $ \x ->
    let !(UInt (I# a)) = x in bitInteger a

primShiftLInteger :: PrimImpl
primShiftLInteger =
  liftBinary $ \x y ->
    let !(UInt (I# a)) = y in shiftLInteger x a

primShiftRInteger :: PrimImpl
primShiftRInteger =
  liftBinary $ \x y ->
    let !(UInt (I# a)) = y in shiftRInteger x a

primTestBitInteger :: PrimImpl
primTestBitInteger =
  liftBinary $ \x y ->
    let !(UInt (I# a)) = y in testBitInteger x a

primHashInteger :: PrimImpl
primHashInteger =
  liftUnary $ \x -> UInt (I# (hashInteger x))

integerComparison :: (Integer -> Integer -> Int#) -> PrimImpl
integerComparison f =
  liftBinary (\x y -> UInt (I# (f x y)))
#endif
