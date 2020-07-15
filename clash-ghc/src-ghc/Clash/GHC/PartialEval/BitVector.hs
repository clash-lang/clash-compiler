{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.GHC.PartialEval.BitVector
  ( bitVectorPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Proxy
import Data.Reflection
import Data.Text (Text)
import GHC.TypeLits
import GHC.TypeLits.Extra

import Clash.Promoted.Nat
import Clash.Promoted.Nat.Unsafe
import Clash.Sized.Internal.BitVector

import Clash.Core.Evaluator.Models
import Clash.Core.Term (Term(..))

import Clash.GHC.PartialEval.Internal
import Clash.GHC.PartialEval.Internal.Sized

bitVectorPrims :: HashMap Text PrimImpl
bitVectorPrims = HashMap.fromList
  [ ("Clash.Sized.Internal.BitVector.fromInteger#", liftId)
  , ("Clash.Sized.Internal.BitVector.BV", primBV)
  , ("Clash.Sized.Internal.BitVector.size#", infoBitVector size#)
  , ("Clash.Sized.Internal.BitVector.maxIndex#", infoBitVector maxIndex#)
  , ("Clash.Sized.Internal.BitVector.undefined#", liftNullarySized undefined#)
  , ("Clash.Sized.Internal.BitVector.pack#", liftUnary pack#)
  , ("Clash.Sized.Internal.BitVector.unpack#", liftUnary unpack#)
  , ("Clash.Sized.Internal.BitVector.++#", primConcat)
  , ("Clash.Sized.Internal.BitVector.reduceAnd#", reduceBitVector reduceAnd#)
  , ("Clash.Sized.Internal.BitVector.reduceOr#", reduceBitVector reduceOr#)
  , ("Clash.Sized.Internal.BitVector.reduceXor#", reduceBitVector reduceXor#)
  , ("Clash.Sized.Internal.BitVector.index#", primIndex)
  , ("Clash.Sized.Internal.BitVector.replaceBit#", primReplaceBit)
  , ("Clash.Sized.Internal.BitVector.setSlice#", primSetSlice)
  , ("Clash.Sized.Internal.BitVector.slice#", primSlice)
  , ("Clash.Sized.Internal.BitVector.split#", primSplit)
  , ("Clash.Sized.Internal.BitVector.msb#", reduceBitVector msb#)
  , ("Clash.Sized.Internal.BitVector.lsb#", reduceBitVector lsb#)
  , ("Clash.Sized.Internal.BitVector.eq#", liftComparison eq#)
  , ("Clash.Sized.Internal.BitVector.neq#", liftComparison neq#)
  , ("Clash.Sized.Internal.BitVector.ge#", liftComparison ge#)
  , ("Clash.Sized.Internal.BitVector.gt#", liftComparison gt#)
  , ("Clash.Sized.Internal.BitVector.le#", liftComparison le#)
  , ("Clash.Sized.Internal.BitVector.lt#", liftComparison lt#)
  , ("Clash.Sized.Internal.BitVector.minBound#", liftNullarySized minBound#)
  , ("Clash.Sized.Internal.BitVector.maxBound#", liftNullarySized maxBound#)
  , ("Clash.Sized.Internal.BitVector.+#", liftBinarySized (+#))
  , ("Clash.Sized.Internal.BitVector.-#", liftBinarySized (-#))
  , ("Clash.Sized.Internal.BitVector.*#", liftBinarySized (*#))
  , ("Clash.Sized.Internal.BitVector.negate#", liftUnarySized negate#)
  , ("Clash.Sized.Internal.BitVector.plus#", extendingNumABitVector plus#)
  , ("Clash.Sized.Internal.BitVector.minus#", extendingNumABitVector minus#)
  , ("Clahs.Sized.Internal.BitVector.times#", primTimes)
  , ("Clash.Sized.Internal.BitVector.quot#", liftBinarySized quot#)
  , ("Clash.Sized.Internal.BitVector.rem#", liftBinarySized rem#)
  , ("Clash.Sized.Internal.BitVector.toInteger#", primToInteger)
  , ("Clash.Sized.Internal.BitVector.and#", liftBinarySized and#)
  , ("Clash.Sized.Internal.BitVector.or#", liftBinarySized or#)
  , ("Clash.Sized.Internal.BitVector.xor#", liftBinarySized xor#)
  , ("Clash.Sized.Internal.BitVector.complement#", liftUnarySized complement#)
  , ("Clash.Sized.Internal.BitVector.shiftL#", shiftRotateBitVector shiftL#)
  , ("Clash.Sized.Internal.BitVector.shiftR#", shiftRotateBitVector shiftR#)
  , ("Clash.Sized.Internal.BitVector.rotateL#", shiftRotateBitVector rotateL#)
  , ("Clash.Sized.Internal.BitVector.rotateR#", shiftRotateBitVector rotateR#)
  , ("Clash.Sized.Internal.BitVector.truncateB#", primTruncateB)
  ]

primBV :: PrimImpl
primBV e p args
  | [Right n, Left x, Left y] <- args
  = do sz <- typeSize n Nothing
       reifyNat sz (\proxy -> go proxy x y)

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall n. (KnownNat n) => Proxy n -> Term -> Term -> PrimEval Value
  go Proxy x y = do
    a <- fromTermOrValue e x
    b <- fromTermOrValue e y
    resTy <- resultType p args
    toValue @(BitVector n) (BV a b) resTy

infoBitVector
  :: (forall n. (KnownNat n) => BitVector n -> Int)
  -> PrimImpl
infoBitVector f e p args
  | [Right n, Left knN, Left x] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz (\proxy -> go proxy x)

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> PrimEval Value
  go Proxy x = do
    a <- fromTermOrValue @(BitVector m) e x
    resTy <- resultType p args
    toValue (f a) resTy

primConcat :: PrimImpl
primConcat e p args
  | [Right m, Right n, Left knM, Left x, Left y] <- args
  = do szM <- typeSize m (Just knM)
       szN <- typeSize n Nothing
       reifyNat szM (\pm -> reifyNat szN (\pn -> go pm pn x y))

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall m n. (KnownNat m, KnownNat n)
     => Proxy m -> Proxy n -> Term -> Term -> PrimEval Value
  go Proxy Proxy x y = do
    a <- fromTermOrValue @(BitVector n) e x
    b <- fromTermOrValue @(BitVector m) e y
    resTy <- resultType p args
    toValue @(BitVector (n + m)) (a ++# b) resTy

reduceBitVector
  :: (forall n. (KnownNat n) => BitVector n -> Bit)
  -> PrimImpl
reduceBitVector f e p args
  | [Right n, Left x] <- args
  = do sz <- typeSize n Nothing
       reifyNat sz (\proxy -> go proxy x)

  | [Right n, Left knN, Left x] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz (\proxy -> go proxy x)

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> PrimEval Value
  go Proxy x = do
    a <- fromTermOrValue @(BitVector m) e x
    resTy <- resultType p args
    toValue (f a) resTy

primIndex :: PrimImpl
primIndex e p args
  | [Right n, Left knN, Left x, Left y] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz (\proxy -> go proxy x y)

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> Term -> PrimEval Value
  go Proxy x y = do
    a <- fromTermOrValue @(BitVector m) e x
    b <- fromTermOrValue e y
    resTy <- resultType p args
    toValue (index# a b) resTy

primReplaceBit :: PrimImpl
primReplaceBit e p args
  | [Right n, Left knN, Left x, Left y, Left z] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz (\proxy -> go proxy x y z)

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall m. (KnownNat m)
     => Proxy m -> Term -> Term -> Term -> PrimEval Value
  go Proxy x y z = do
    a <- fromTermOrValue e x
    b <- fromTermOrValue e y
    c <- fromTermOrValue e z
    resTy <- resultType p args
    toValue @(BitVector m) (replaceBit# a b c) resTy

primSetSlice :: PrimImpl
primSetSlice e p args
  | [Right m, Right i, Right n, Left x, Left _m, Left _n, Left y] <- args
  = do szM <- typeSize m Nothing
       szI <- typeSize i Nothing
       szN <- typeSize n Nothing
       reifyNat szM (\pM -> reifyNat szI (\pI -> reifyNat szN (\pN -> go pM pI pN x y)))

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall m i n. (KnownNat m, KnownNat i, KnownNat n)
     => Proxy m -> Proxy i -> Proxy n -> Term -> Term -> PrimEval Value
  go pM pI pN x y = do
    a <- fromTermOrValue @(BitVector (m + 1 + i)) e x
    b <- fromTermOrValue @(BitVector (m + 1 - n)) e y
    let sM   = snatProxy pM
        sN   = snatProxy pN
        sM1I = addSNat (addSNat sM (unsafeSNat 1)) (snatProxy pI)
    resTy <- resultType p args
    toValue @(BitVector (m + 1 + i)) (setSlice# sM1I a sM sN b) resTy

primSlice :: PrimImpl
primSlice e p args
  -- Note: We recreate the SNat values in 'go'.
  | [Right m, Right i, Right n, Left x, Left _m, Left _n] <- args
  = do szM <- typeSize m Nothing
       szI <- typeSize i Nothing
       szN <- typeSize n Nothing
       reifyNat szM (\pM -> reifyNat szI (\pI -> reifyNat szN (\pN -> go pM pI pN x)))

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall m i n. (KnownNat m, KnownNat i, KnownNat n)
     => Proxy m -> Proxy i -> Proxy n -> Term -> PrimEval Value
  go pM Proxy pN x = do
    a <- fromTermOrValue @(BitVector (m + 1 + i)) e x
    let sM = snatProxy pM
        sN = snatProxy pN
    resTy <- resultType p args
    toValue @(BitVector (m + 1 - n)) (slice# a sM sN) resTy

primSplit :: PrimImpl
primSplit e p args
  | [Right n, Right m, Left knN, Left x] <- args
  = do szN <- typeSize n (Just knN)
       szM <- typeSize m Nothing
       reifyNat szN (\pN -> reifyNat szM (\pM -> go pN pM x))

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall n m. (KnownNat n, KnownNat m)
     => Proxy n -> Proxy m -> Term -> PrimEval Value
  go Proxy Proxy x = do
    a <- fromTermOrValue @(BitVector (m + n)) e x
    resTy <- resultType p args
    toValue @(BitVector m, BitVector n) (split# a) resTy

primToInteger :: PrimImpl
primToInteger e p args
  | [Right n, Left knN, Left x] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz (\proxy -> go proxy x)

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> PrimEval Value
  go Proxy x = do
    a <- fromTermOrValue @(BitVector m) e x
    resTy <- resultType p args
    toValue (toInteger# a) resTy

extendingNumABitVector
  :: (forall m n. (KnownNat m, KnownNat n)
        => BitVector m -> BitVector n -> BitVector (Max m n + 1))
  -> PrimImpl
extendingNumABitVector f e p args
  | [Right m, Right n, Left knM, Left knN, Left x, Left y] <- args
  = do szM <- typeSize m (Just knM)
       szN <- typeSize n (Just knN)
       reifyNat szM (\pm -> reifyNat szN (\pn -> go pm pn x y))

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall m n. (KnownNat m, KnownNat n)
     => Proxy m -> Proxy n -> Term -> Term -> PrimEval Value
  go Proxy Proxy x y = do
    a <- fromTermOrValue @(BitVector m) e x
    b <- fromTermOrValue @(BitVector n) e y
    resTy <- resultType p args
    toValue @(BitVector (Max m n + 1)) (f a b) resTy

primTimes :: PrimImpl
primTimes e p args
  | [Right m, Right n, Left knM, Left knN, Left x, Left y] <- args
  = do szM <- typeSize m (Just knM)
       szN <- typeSize n (Just knN)
       reifyNat szM (\pm -> reifyNat szN (\pn -> go pm pn x y))

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall m n. (KnownNat m, KnownNat n)
     => Proxy m -> Proxy n -> Term -> Term -> PrimEval Value
  go Proxy Proxy x y = do
    a <- fromTermOrValue @(BitVector m) e x
    b <- fromTermOrValue @(BitVector n) e y
    resTy <- resultType p args
    toValue @(BitVector (m + n)) (times# a b) resTy

shiftRotateBitVector
  :: (forall n. (KnownNat n) => BitVector n -> Int -> BitVector n)
  -> PrimImpl
shiftRotateBitVector f e p args
  | [Right n, Left knN, Left x, Left y] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz (\proxy -> go proxy x y)

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> Term -> PrimEval Value
  go Proxy x y = do
    a <- fromTermOrValue @(BitVector m) e x
    b <- fromTermOrValue e y
    resTy <- resultType p args
    toValue @(BitVector m) (f a b) resTy

primTruncateB :: PrimImpl
primTruncateB e p args
  | [Right n, Right m, Left knN, Left x] <- args
  = do szN <- typeSize n (Just knN)
       szM <- typeSize m Nothing
       reifyNat szN (\pN -> reifyNat szM (\pM -> go pN pM x))

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall n m. (KnownNat n, KnownNat m)
     => Proxy n -> Proxy m -> Term -> PrimEval Value
  go Proxy Proxy x = do
    a <- fromTermOrValue @(BitVector (m + n)) e x
    resTy <- resultType p args
    toValue @(BitVector m) (truncateB# a) resTy

