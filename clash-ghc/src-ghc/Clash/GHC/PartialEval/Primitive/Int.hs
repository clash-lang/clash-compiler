{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.PartialEval.Primitive.Int
  ( intPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Text (Text)
import GHC.Int
import GHC.Prim
import GHC.Types hiding (Type)

import Clash.GHC.PartialEval.Primitive.Strategy
import Clash.GHC.PartialEval.Primitive.ToAst
import Clash.GHC.PartialEval.Primitive.Unboxed

intPrims :: HashMap Text PrimImpl
intPrims = HashMap.fromList
  [ ("GHC.Prim.+#", liftBinary# (+#))
  , ("GHC.Prim.-#", liftBinary# (-#))
  , ("GHC.Prim.*#", liftBinary# (*#))
  , ("GHC.Prim.mulIntMayOflo#", liftBinary# mulIntMayOflo#)
  , ("GHC.Prim.quotInt#", liftBinary# quotInt#)
  , ("GHC.Prim.remInt#", liftBinary# remInt#)
  , ("GHC.Prim.quotRemInt#", liftI_I_II quotRemInt#)
  , ("GHC.Prim.andI#", liftBinary# andI#)
  , ("GHC.Prim.orI#", liftBinary# orI#)
  , ("GHC.Prim.xorI#", liftBinary# xorI#)
  , ("GHC.Prim.notI#", liftUnary# notI#)
  , ("GHC.Prim.negateInt#", liftUnary# negateInt#)
  , ("GHC.Prim.addIntC#", liftI_I_II addIntC#)
  , ("GHC.Prim.subIntC#", liftI_I_II subIntC#)
  , ("GHC.Prim.>#", liftBinary# (>#))
  , ("GHC.Prim.>=#", liftBinary# (>=#))
  , ("GHC.Prim.==#", liftBinary# (==#))
  , ("GHC.Prim./=#", liftBinary# (/=#))
  , ("GHC.Prim.<#", liftBinary# (<#))
  , ("GHC.Prim.<=#", liftBinary# (<=#))
  , ("GHC.Prim.chr#", primChr)
  , ("GHC.Prim.int2Word#", primInt2Word)
  , ("GHC.Prim.int2Float#", primInt2Float)
  , ("GHC.Prim.int2Double#", primInt2Double)
  , ("GHC.Prim.word2Float#", primWord2Float)
  , ("GHC.Prim.word2Double#", primWord2Double)
  , ("GHC.Prim.uncheckedIShiftL#", liftBinary# uncheckedIShiftL#)
  , ("GHC.Prim.uncheckedIShiftRA#", liftBinary# uncheckedIShiftRA#)
  , ("GHC.Prim.uncheckedIShiftRL#", liftBinary# uncheckedIShiftRL#)
  , ("GHC.Types.I#", liftId)
  , ("GHC.Int.I8#", liftFixedWidth I8#)
  , ("GHC.Int.I16#", liftFixedWidth I16#)
  , ("GHC.Int.I32#", liftFixedWidth I32#)
  , ("GHC.Int.I64#", liftFixedWidth I64#)
  ]

primChr :: PrimImpl
primChr =
  liftUnary $ \x ->
    let !(UInt (I# a)) = x in UChar (C# (chr# a))

primInt2Word :: PrimImpl
primInt2Word =
  liftUnary $ \x ->
    let !(UInt (I# a)) = x in UWord (W# (int2Word# a))

primInt2Float :: PrimImpl
primInt2Float =
  liftUnary $ \x ->
    let !(UInt (I# a)) = x in UFloat (F# (int2Float# a))

primInt2Double :: PrimImpl
primInt2Double =
  liftUnary $ \x ->
    let !(UInt (I# a)) = x in UDouble (D# (int2Double# a))

primWord2Float :: PrimImpl
primWord2Float =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x in UFloat (F# (word2Float# a))

primWord2Double :: PrimImpl
primWord2Double =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x in UDouble (D# (word2Double# a))

liftUnary# :: (Int# -> Int#) -> PrimImpl
liftUnary# f =
  liftUnary $ \x ->
    let !(UInt (I# a)) = x in UInt (I# (f a))

liftBinary# :: (Int# -> Int# -> Int#) -> PrimImpl
liftBinary# f =
  liftBinary $ \x y ->
    let !(UInt (I# a)) = x
        !(UInt (I# b)) = y
     in UInt (I# (f a b))

liftI_I_II :: (Int# -> Int# -> (# Int#, Int# #)) -> PrimImpl
liftI_I_II f =
  liftBinary $ \x y ->
    let !(UInt (I# a)) = x
        !(UInt (I# b)) = y
        !(# c, d #) = f a b
     in UTuple2 (UInt (I# c), UInt (I# d))

liftFixedWidth :: (ToAst a) => (Int# -> a) -> PrimImpl
liftFixedWidth f =
  liftUnary $ \x ->
    let !(UInt (I# a)) = x in f a
