{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.GHC.PartialEval.Signed
  ( signedPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Proxy
import Data.Reflection
import Data.Text (Text)
import GHC.TypeLits
import GHC.TypeLits.Extra

import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Signed

import Clash.Core.Evaluator.Models
import Clash.Core.Term (Term)

import Clash.GHC.PartialEval.Internal
import Clash.GHC.PartialEval.Internal.Sized

signedPrims :: HashMap Text PrimImpl
signedPrims = HashMap.fromList
  [ ("Clash.Sized.Internal.Signed.fromInteger#", liftId)
  , ("Clash.Sized.Internal.Signed.size#", primSize)
  , ("Clash.Sized.Internal.Signed.pack#", primPack)
  , ("Clash.Sized.Internal.Signed.unpack#", primUnpack)
  , ("Clash.Sized.Internal.Signed.eq#", liftComparison eq#)
  , ("Clash.Sized.Internal.Signed.neq#", liftComparison neq#)
  , ("Clash.Sized.Internal.Signed.ge#", liftComparison ge#)
  , ("Clash.Sized.Internal.Signed.gt#", liftComparison gt#)
  , ("Clash.Sized.Internal.Signed.le#", liftComparison le#)
  , ("Clash.Sized.Internal.Signed.lt#", liftComparison lt#)
  , ("Clash.Sized.Internal.Signed.minBound#", liftNullarySized minBound#)
  , ("Clash.Sized.Internal.Signed.maxBound#", liftNullarySized maxBound#)
  , ("Clash.Sized.Internal.Signed.+#", liftBinarySized (+#))
  , ("Clash.Sized.Internal.Signed.-#", liftBinarySized (-#))
  , ("Clash.Sized.Internal.Signed.*#", liftBinarySized (*#))
  , ("Clash.Sized.Internal.Signed.negate#", liftUnarySized negate#)
  , ("Clash.Sized.Internal.Signed.abs#", liftUnarySized abs#)
  , ("Clash.Sized.Internal.Signed.plus#", extendingNumASigned plus#)
  , ("Clash.Sized.Internal.Signed.minus#", extendingNumASigned minus#)
  , ("Clash.Sized.Internal.Signed.times#", primTimes)
  , ("Clash.Sized.Internal.Signed.quot#", liftBinarySized quot#)
  , ("Clash.Sized.Internal.Signed.rem#", liftBinarySized rem#)
  , ("Clash.Sized.Internal.Signed.div#", liftBinarySized div#)
  , ("Clash.Sized.Internal.Signed.mod#", liftBinarySized mod#)
  , ("Clash.Sized.Internal.Signed.toInteger#", primToInteger)
  , ("Clash.Sized.Internal.Signed.and#", liftBinarySized and#)
  , ("Clash.Sized.Internal.Signed.or#", liftBinarySized or#)
  , ("Clash.Sized.Internal.Signed.xor#", liftBinarySized xor#)
  , ("Clash.Sized.Internal.Signed.complement#", liftUnarySized complement#)
  , ("Clash.Sized.Internal.Signed.shiftL#", shiftRotateSigned shiftL#)
  , ("Clash.Sized.Internal.Signed.shiftR#", shiftRotateSigned shiftR#)
  , ("Clash.Sized.Internal.Signed.rotateL#", shiftRotateSigned rotateL#)
  , ("Clash.Sized.Internal.Signed.rotateR#", shiftRotateSigned rotateR#)
  , ("Clash.Sized.Internal.Signed.resize#", primResize)
  , ("Clash.Sized.Internal.Signed.truncateB#", primTruncateB)
  ]

primSize :: PrimImpl
primSize e p args
  | [Right n, Left knN, Left x] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz (\proxy -> go proxy x)

  | otherwise
  = empty
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> PrimEval Value
  go Proxy x = do
    a <- fromTermOrValue @(Signed m) e x
    resTy <- resultType p args
    toValue (size# a) resTy

primPack :: PrimImpl
primPack e p args
  | [Right n, Left knN, Left x] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz (\proxy -> go proxy x)

  | otherwise
  = empty
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> PrimEval Value
  go Proxy x = do
    a <- fromTermOrValue @(Signed m) e x
    resTy <- resultType p args
    toValue @(BitVector m) (pack# a) resTy

primUnpack :: PrimImpl
primUnpack e p args
  | [Right n, Left knN, Left x] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz (\proxy -> go proxy x)

  | otherwise
  = empty
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> PrimEval Value
  go Proxy x = do
    a <- fromTermOrValue @(BitVector m) e x
    resTy <- resultType p args
    toValue @(Signed m) (unpack# a) resTy

extendingNumASigned
  :: (forall m n. (KnownNat m, KnownNat n)
        => Signed m -> Signed n -> Signed (Max m n + 1))
  -> PrimImpl
extendingNumASigned f e p args
  | [Right m, Right n, Left x, Left y] <- args
  = do szM <- typeSize m Nothing
       szN <- typeSize n Nothing
       reifyNat szM (\pm -> reifyNat szN (\pn -> go pm pn x y))

  | otherwise
  = empty
 where
  go :: forall m n. (KnownNat m, KnownNat n)
     => Proxy m -> Proxy n -> Term -> Term -> PrimEval Value
  go Proxy Proxy x y = do
    a <- fromTermOrValue @(Signed m) e x
    b <- fromTermOrValue @(Signed n) e y
    resTy <- resultType p args
    toValue @(Signed (Max m n + 1)) (f a b) resTy

primTimes :: PrimImpl
primTimes e p args
  | [Right m, Right n, Left x, Left y] <- args
  = do szM <- typeSize m Nothing
       szN <- typeSize n Nothing
       reifyNat szM (\pm -> reifyNat szN (\pn -> go pm pn x y))

  | otherwise
  = empty
 where
  go :: forall m n. (KnownNat m, KnownNat n)
     => Proxy m -> Proxy n -> Term -> Term -> PrimEval Value
  go Proxy Proxy x y = do
    a <- fromTermOrValue @(Signed m) e x
    b <- fromTermOrValue @(Signed n) e y
    resTy <- resultType p args
    toValue @(Signed (m + n)) (times# a b) resTy

primToInteger :: PrimImpl
primToInteger e p args
  | [Right n, Left x] <- args
  = do sz <- typeSize n Nothing
       reifyNat sz (\proxy -> go proxy x)

  | otherwise
  = empty
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> PrimEval Value
  go Proxy x = do
    a <- fromTermOrValue @(Signed m) e x
    resTy <- resultType p args
    toValue (toInteger# a) resTy

shiftRotateSigned
  :: (forall n. (KnownNat n) => Signed n -> Int -> Signed n)
  -> PrimImpl
shiftRotateSigned f e p args
  | [Right n, Left knN, Left x, Left y] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz (\proxy -> go proxy x y)

  | otherwise
  = empty
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> Term -> PrimEval Value
  go Proxy x y = do
    a <- fromTermOrValue @(Signed m) e x
    b <- fromTermOrValue e y
    resTy <- resultType p args
    toValue @(Signed m) (f a b) resTy

primResize :: PrimImpl
primResize e p args
  | [Right m, Right n, Left knN, Left knM, Left x] <- args
  = do szM <- typeSize m (Just knM)
       szN <- typeSize n (Just knN)
       reifyNat szM (\pM -> reifyNat szN (\pN -> go pM pN x))

  | otherwise
  = empty
 where
  go :: forall m n. (KnownNat m, KnownNat n)
     => Proxy m -> Proxy n -> Term -> PrimEval Value
  go Proxy Proxy x = do
    a <- fromTermOrValue @(Signed m) e x
    resTy <- resultType p args
    toValue @(Signed n) (resize# a) resTy

primTruncateB :: PrimImpl
primTruncateB e p args
  | [Right n, Right m, Left knM, Left x] <- args
  = do szN <- typeSize n Nothing
       szM <- typeSize m (Just knM)
       reifyNat szN (\pN -> reifyNat szM (\pM -> go pN pM x))

  | otherwise
  = empty
 where
  go :: forall n m. (KnownNat n, KnownNat m)
     => Proxy n -> Proxy m -> Term -> PrimEval Value
  go Proxy Proxy x = do
    a <- fromTermOrValue @(Signed (m + n)) e x
    resTy <- resultType p args
    toValue @(Signed m) (truncateB# a) resTy

