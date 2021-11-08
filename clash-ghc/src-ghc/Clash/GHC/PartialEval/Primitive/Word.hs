{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnboxedTuples #-}

module Clash.GHC.PartialEval.Primitive.Word
  ( wordPrims
  ) where

import Control.Monad.Catch (throwM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import GHC.Prim
import GHC.Types
import GHC.Word

import Clash.Core.PartialEval.Monad

import Clash.GHC.PartialEval.Primitive.FromAst
import Clash.GHC.PartialEval.Primitive.Info
import Clash.GHC.PartialEval.Primitive.Strategy
import Clash.GHC.PartialEval.Primitive.ToAst
import Clash.GHC.PartialEval.Primitive.Unboxed

-- | Primitive Operatations defined on Word# and related
-- fixed-width types (Word{8,16,32,64})
--
wordPrims :: HashMap Text PrimImpl
wordPrims = HashMap.fromList
  [ ("GHC.Prim.plusWord#", liftBinary# plusWord#)
  , ("GHC.Prim.addWordC#", liftW_W_WI addWordC#)
  , ("GHC.Prim.subWordC#", liftW_W_WI subWordC#)
  , ("GHC.Prim.plusWord2#", liftW_W_WW plusWord2#)
  , ("GHC.Prim.minusWord#", liftBinary# minusWord#)
  , ("GHC.Prim.timesWord#", liftBinary# timesWord#)
  , ("GHC.Prim.timesWord2#", liftW_W_WW timesWord2#)
  , ("GHC.Prim.quotWord#", liftBinary# quotWord#)
  , ("GHC.Prim.remWord#", liftBinary# remWord#)
  , ("GHC.Prim.quotRemWord#", liftW_W_WW quotRemWord#)
  , ("GHC.Prim.quotRemWord2#", primQuotRemWord2)
  , ("GHC.Prim.and#", liftBinary# and#)
  , ("GHC.Prim.or#", liftBinary# or#)
  , ("GHC.Prim.xor#", liftBinary# xor#)
  , ("GHC.Prim.not#", liftUnary# not#)
  , ("GHC.Prim.uncheckedShiftL#", primUncheckedShiftL)
  , ("GHC.Prim.uncheckedShiftRL#", primUncheckedShiftRL)
  , ("GHC.Prim.word2Int#", primWord2Int)
  , ("GHC.Prim.gtWord#", wordComparison gtWord#)
  , ("GHC.Prim.geWord#", wordComparison geWord#)
  , ("GHC.Prim.eqWord#", wordComparison eqWord#)
  , ("GHC.Prim.neWord#", wordComparison neWord#)
  , ("GHC.Prim.ltWord#", wordComparison ltWord#)
  , ("GHC.Prim.leWord#", wordComparison leWord#)
  , ("GHC.Prim.popCnt8#", liftUnary# popCnt8#)
  , ("GHC.Prim.popCnt16#", liftUnary# popCnt16#)
  , ("GHC.Prim.popCnt32#", liftUnary# popCnt32#)
  , ("GHC.Prim.popCnt64#", liftUnary# popCnt64#)
  , ("GHC.Prim.popCnt#", liftUnary# popCnt#)
  , ("GHC.Prim.pdep8#", liftBinary# pdep8#)
  , ("GHC.Prim.pdep16#", liftBinary# pdep16#)
  , ("GHC.Prim.pdep32#", liftBinary# pdep32#)
  , ("GHC.Prim.pdep64#", liftBinary# pdep64#)
  , ("GHC.Prim.pdep", liftBinary# pdep#)
  , ("GHC.Prim.pext8#", liftBinary# pext8#)
  , ("GHC.Prim.pext16#", liftBinary# pext16#)
  , ("GHC.Prim.pext32#", liftBinary# pext32#)
  , ("GHC.Prim.pext64#", liftBinary# pext64#)
  , ("GHC.Prim.pext", liftBinary# pext#)
  , ("GHC.Prim.clz8#", liftUnary# clz8#)
  , ("GHC.Prim.clz16#", liftUnary# clz16#)
  , ("GHC.Prim.clz32#", liftUnary# clz32#)
  , ("GHC.Prim.clz64#", liftUnary# clz64#)
  , ("GHC.Prim.clz#", liftUnary# clz#)
  , ("GHC.Prim.ctz8#", liftUnary# ctz8#)
  , ("GHC.Prim.ctz16#", liftUnary# ctz16#)
  , ("GHC.Prim.ctz32#", liftUnary# ctz32#)
  , ("GHC.Prim.ctz64#", liftUnary# ctz64#)
  , ("GHC.Prim.ctz#", liftUnary# ctz#)
  , ("GHC.Prim.byteSwap16#", liftUnary# byteSwap16#)
  , ("GHC.Prim.byteSwap32#", liftUnary# byteSwap32#)
  , ("GHC.Prim.byteSwap64#", liftUnary# byteSwap64#)
  , ("GHC.Prim.byteSwap#", liftUnary# byteSwap#)

#if MIN_VERSION_base(4,14,0)
  , ("GHC.Prim.bitReverse8#", liftUnary# bitReverse8#)
  , ("GHC.Prim.bitReverse16#", liftUnary# bitReverse16#)
  , ("GHC.Prim.bitReverse32#", liftUnary# bitReverse32#)
  , ("GHC.Prim.bitReverse64#", liftUnary# bitReverse64#)
  , ("GHC.Prim.bitReverse#", liftUnary# bitReverse#)
#endif

  , ("GHC.Types.W#", liftId)
  , ("GHC.Word.W8#", liftFixedWidth W8#)
  , ("GHC.Word.W16#", liftFixedWidth W16#)
  , ("GHC.Word.W32#", liftFixedWidth W32#)
  , ("GHC.Word.W64#", liftFixedWidth W64#)
  ]

primQuotRemWord2 :: PrimImpl
primQuotRemWord2 pr args
  | [Left x, Left y, Left z] <- args
  = do !(UWord (W# a)) <- fromValueForce x
       !(UWord (W# b)) <- fromValueForce y
       !(UWord (W# c)) <- fromValueForce z

       let !(# d, e #) = quotRemWord2# a b c
       resTy <- resultType pr args
       toValue (UTuple2 (UWord (W# d), UWord (W# e))) resTy

  | otherwise
  = throwM (UnexpectedArgs pr args)

primUncheckedShiftL :: PrimImpl
primUncheckedShiftL =
  liftBinary $ \x y ->
    let !(UWord (W# a)) = x
        !(UInt (I# b)) = y
     in UWord (W# (uncheckedShiftL# a b))

primUncheckedShiftRL :: PrimImpl
primUncheckedShiftRL =
  liftBinary $ \x y ->
    let !(UWord (W# a)) = x
        !(UInt (I# b)) = y
     in UWord (W# (uncheckedShiftRL# a b))

primWord2Int :: PrimImpl
primWord2Int =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x in UInt (I# (word2Int# a))

liftUnary# :: (Word# -> Word#) -> PrimImpl
liftUnary# f =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x in UWord (W# (f a))

liftBinary# :: (Word# -> Word# -> Word#) -> PrimImpl
liftBinary# f =
  liftBinary $ \x y ->
    let !(UWord (W# a)) = x
        !(UWord (W# b)) = y
     in UWord (W# (f a b))

liftW_W_WI :: (Word# -> Word# -> (# Word#, Int# #)) -> PrimImpl
liftW_W_WI f =
  liftBinary $ \x y ->
    let !(UWord (W# a)) = x
        !(UWord (W# b)) = y
        !(# c, d #) = f a b
     in UTuple2 (UWord (W# c), UInt (I# d))

liftW_W_WW :: (Word# -> Word# -> (# Word#, Word# #)) -> PrimImpl
liftW_W_WW f =
  liftBinary $ \x y ->
    let !(UWord (W# a)) = x
        !(UWord (W# b)) = y
        !(# c, d #) = f a b
     in UTuple2 (UWord (W# c), UWord (W# d))

wordComparison :: (Word# -> Word# -> Int#) -> PrimImpl
wordComparison f =
  liftBinary $ \x y ->
    let !(UWord (W# a)) = x
        !(UWord (W# b)) = y
     in UInt (I# (f a b))

liftFixedWidth :: (ToAst a) => (Word# -> a) -> PrimImpl
liftFixedWidth f =
  liftUnary $ \x ->
    let !(UWord (W# a)) = x in f a
