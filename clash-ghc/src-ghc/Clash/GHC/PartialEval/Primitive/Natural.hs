{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.PartialEval.Primitive.Natural
  ( naturalPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)

#if MIN_VERSION_base(4,15,0)
import Data.Primitive.ByteArray (ByteArray(..))
import GHC.Num.Natural
import GHC.Prim
#else
import GHC.Natural
#endif

import GHC.Types

import Clash.GHC.PartialEval.Primitive.Strategy
import Clash.GHC.PartialEval.Primitive.Unboxed

naturalPrims :: HashMap Text PrimImpl
naturalPrims = HashMap.fromList
#if MIN_VERSION_base(4,15,0)
  [ ("GHC.Num.Natural.NB", primNB)
  , ("GHC.Num.Natural.NS", primNS)
  , ("GHC.Num.Natural.naturalAdd", liftBinary naturalAdd)
  , ("GHC.Num.Natural.naturalAnd", liftBinary naturalAnd)
  , ("GHC.Num.Natural.naturalAndNot", liftBinary naturalAndNot)
  , ("GHC.Num.Natural.naturalBit#", primBit)
  , ("GHC.Num.Natural.naturalCompare", liftBinary naturalCompare)
  , ("GHC.Num.Natural.naturalEq#", primComparison naturalEq#)
--, ("GHC.Num.Natural.naturalFromAddr#", error "IO prim")
--, ("GHC.Num.Natural.naturalFromByteArray#", error "IO prim")
  , ("GHC.Num.Natural.naturalGcd", liftBinary naturalGcd)
  , ("GHC.Num.Natural.naturalGe#", primComparison naturalGe#)
  , ("GHC.Num.Natural.naturalGt#", primComparison naturalGt#)
  , ("GHC.Num.Natural.naturalLcm", liftBinary naturalLcm)
  , ("GHC.Num.Natural.naturalLe#", primComparison naturalLe#)
  , ("GHC.Num.Natural.naturalLog2#", primLog2)
  , ("GHC.Num.Natural.naturalLogBase#", primLogBase)
  , ("GHC.Num.Natural.naturalLogBaseWord#", primLogBaseWord)
  , ("GHC.Num.Natural.naturalLt#", primComparison naturalLt#)
  , ("GHC.Num.Natural.naturalMul", liftBinary naturalMul)
  , ("GHC.Num.Natural.naturalNe#", primComparison naturalNe#)
  , ("GHC.Num.Natural.naturalNegate", liftUnary naturalNegate)
  , ("GHC.Num.Natural.naturalOr", liftBinary naturalOr)
  , ("GHC.Num.Natural.naturalPopCount#", primPopCount)
--, ("GHC.Num.Natural.naturalPowMod", error "liftFun4")
  , ("GHC.Num.Natural.naturalQuot", liftBinary naturalQuot)
  , ("GHC.Num.Natural.naturalQuotRem#", primQuotRem)
  , ("GHC.Num.Natural.naturalRem", liftBinary naturalRem)
  , ("GHC.Num.Natural.naturalShiftL#", primShift naturalShiftL#)
  , ("GHC.Num.Natural.naturalShiftR#", primShift naturalShiftR#)
  , ("GHC.Num.Natural.naturalSignum", liftUnary naturalSignum)
  , ("GHC.Num.Natural.naturalSizeInBase#", primSizeInBase)
--, ("GHC.Num.Natural.naturalSub", error "Unboxed sum")
  , ("GHC.Num.Natural.naturalSubThrow", liftBinary naturalSubThrow)
  , ("GHC.Num.Natural.naturalSubUnsafe", liftBinary naturalSubUnsafe)
  , ("GHC.Num.Natural.naturalTestBit#", primTestBit)
--, ("GHC.Num.Natural.naturalToAddr#", error "IO prim")
--, ("GHC.Num.Natural.naturalToMutableByteArray#", error "IO prim")
  , ("GHC.Num.Natural.naturalToWord#", primToWord)
  , ("GHC.Num.Natural.naturalToWordClamp", primToWordClamp)
  , ("GHC.Num.Natural.naturalXor", liftBinary naturalXor)
  , ("GHC.Num.Natural.$wnaturalNegate", primWNegate)
  , ("GHC.Num.Natural.$wnaturalSignum", primWSignum)
  ]
#else
  [ ("GHC.Natural.naturalToInteger", liftUnary naturalToInteger)
  , ("GHC.Natural.naturalFromInteger", liftUnary naturalFromInteger)
  , ("GHC.Natural.plusNatural", liftBinary plusNatural)
  , ("GHC.Natural.timesNatural", liftBinary timesNatural)
  , ("GHC.Natural.minusNatural", liftBinary minusNatural)
  , ("GHC.Natural.wordToNatural#", primWordToNatural)
  , ("GHC.Natural.gcdNatural", liftBinary gcdNatural)
--, ("GHC.Natural.$wshiftLNatural", _)
  , ("GHC.Natural.NatS#", primNatS)
  ]
#endif

#if MIN_VERSION_base(4,15,0)

primNB :: PrimImpl
primNB =
  liftUnary $ \x ->
    let !(UByteArray (ByteArray a)) = x
     in NB a

primNS :: PrimImpl
primNS =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x
     in NS a

primBit :: PrimImpl
primBit =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x
     in naturalBit# a

primComparison :: (Natural -> Natural -> Int#) -> PrimImpl
primComparison f =
  liftBinary $ \x y -> UInt (I# (f x y))

primLog2 :: PrimImpl
primLog2 =
  liftUnary $ \x -> UWord (W# (naturalLog2# x))

primLogBase :: PrimImpl
primLogBase =
  liftBinary $ \x y -> UWord (W# (naturalLogBase# x y))

primLogBaseWord :: PrimImpl
primLogBaseWord =
  liftBinary $ \x y ->
    let !(UWord (W# a)) = x
     in UWord (W# (naturalLogBaseWord# a y))

primPopCount :: PrimImpl
primPopCount =
  liftUnary $ \x -> UWord (W# (naturalPopCount# x))

primQuotRem :: PrimImpl
primQuotRem =
  liftBinary $ \x y ->
    let (# a, b #) = naturalQuotRem# x y
     in UTuple2 (a, b)

primShift :: (Natural -> Word# -> Natural) -> PrimImpl
primShift f =
  liftBinary $ \x y ->
    let !(UWord (W# a)) = y
     in f x a

primSizeInBase :: PrimImpl
primSizeInBase =
  liftBinary $ \x y ->
    let !(UWord (W# a)) = x
     in UWord (W# (naturalSizeInBase# a y))

primTestBit :: PrimImpl
primTestBit =
  liftBinary $ \x y ->
    let !(UWord (W# a)) = y
     in UInt (I# (naturalTestBit# x a))

primToWord :: PrimImpl
primToWord =
  liftUnary $ \x -> UWord (W# (naturalToWord# x))

primToWordClamp :: PrimImpl
primToWordClamp =
  liftUnary $ \x -> UWord (W# (naturalToWordClamp# x))

primWNegate :: PrimImpl
primWNegate =
  liftUnary $ \x ->
    let !(NS a) = naturalNegate x
     in UWord (W# a)

primWSignum :: PrimImpl
primWSignum =
  liftUnary $ \x ->
    let !(NS a) = naturalSignum x
     in UWord (W# a)

#else

primWordToNatural :: PrimImpl
primWordToNatural =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x in wordToNatural# a

primNatS :: PrimImpl
primNatS =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x in NatS# a
#endif
