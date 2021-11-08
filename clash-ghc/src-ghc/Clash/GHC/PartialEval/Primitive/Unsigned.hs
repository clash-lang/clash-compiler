{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.GHC.PartialEval.Primitive.Unsigned
  ( unsignedPrims
  ) where

import Control.Monad.Catch (throwM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Proxy
import Data.Reflection
import Data.Text (Text)
import GHC.TypeLits
import GHC.TypeLits.Extra

import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Unsigned

import Clash.Core.PartialEval.Monad
import Clash.Core.PartialEval.NormalForm
import Clash.Core.Literal

import Clash.GHC.PartialEval.Primitive.FromAst
import Clash.GHC.PartialEval.Primitive.Info
import Clash.GHC.PartialEval.Primitive.Strategy
import Clash.GHC.PartialEval.Primitive.ToAst
import Clash.GHC.PartialEval.Primitive.Internal.Sized

unsignedPrims :: HashMap Text PrimImpl
unsignedPrims = HashMap.fromList
  [ ("Clash.Class.Exp.expUnsigned#", liftId)
  , ("Clash.Sized.Internal.Unsigned.*#", liftBinarySized (*#))
  , ("Clash.Sized.Internal.Unsigned.+#", liftBinarySized (+#))
  , ("Clash.Sized.Internal.Unsigned.-#", liftBinarySized (-#))
  , ("Clash.Sized.Internal.Unsigned.and#", liftBinarySized and#)
  , ("Clash.Sized.Internal.Unsigned.complement#", liftUnarySized complement#)
  , ("Clash.Sized.Internal.Unsigned.eq#", liftComparison eq#)
  , ("Clash.Sized.Internal.Unsigned.fromInteger#", liftId)
  , ("Clash.Sized.Internal.Unsigned.ge#", liftComparison ge#)
  , ("Clash.Sized.Internal.Unsigned.gt#", liftComparison gt#)
  , ("Clash.Sized.Internal.Unsigned.le#", liftComparison le#)
  , ("Clash.Sized.Internal.Unsigned.lt#", liftComparison lt#)
  , ("Clash.Sized.Internal.Unsigned.maxBound#", liftNullarySized maxBound#)
  , ("Clash.Sized.Internal.Unsigned.minBound#", liftNullarySized minBound#)
  , ("Clash.Sized.Internal.Unsigned.minus#", extendingNumAUnsigned minus#)
  , ("Clash.Sized.Internal.Unsigned.negate#", liftUnarySized negate#)
  , ("Clash.Sized.Internal.Unsigned.neq#", liftComparison neq#)
  , ("Clash.Sized.Internal.Unsigned.or#", liftBinarySized or#)
  , ("Clash.Sized.Internal.Unsigned.pack#", primPack)
  , ("Clash.Sized.Internal.Unsigned.plus#", extendingNumAUnsigned plus#)
  , ("Clash.Sized.Internal.Unsigned.quot#", liftBinarySized quot#)
  , ("Clash.Sized.Internal.Unsigned.rem#", liftBinarySized rem#)
  , ("Clash.Sized.Internal.Unsigned.resize#", primResize)
  , ("Clash.Sized.Internal.Unsigned.rotateL#", shiftRotateUnsigned rotateL#)
  , ("Clash.Sized.Internal.Unsigned.rotateR#", shiftRotateUnsigned rotateR#)
  , ("Clash.Sized.Internal.Unsigned.shiftL#", shiftRotateUnsigned shiftL#)
  , ("Clash.Sized.Internal.Unsigned.shiftR#", shiftRotateUnsigned shiftR#)
  , ("Clash.Sized.Internal.Unsigned.size#", primSize)
  , ("Clash.Sized.Internal.Unsigned.times#", primTimes)
  , ("Clash.Sized.Internal.Unsigned.toInteger#", primToInteger)
  , ("Clash.Sized.Internal.Unsigned.unpack#", primUnpack)
  , ("Clash.Sized.Internal.Unsigned.unsigned16toWord16", convertUnsigned unsigned16toWord16)
  , ("Clash.Sized.Internal.Unsigned.unsigned32toWord32", convertUnsigned unsigned32toWord32)
  , ("Clash.Sized.Internal.Unsigned.unsigned8toWord8", convertUnsigned unsigned8toWord8)
  , ("Clash.Sized.Internal.Unsigned.unsignedtoWord", convertUnsigned unsignedToWord)
  , ("Clash.Sized.Internal.Unsigned.xor#", liftBinarySized xor#)
  ]

primSize :: PrimImpl
primSize pr args
  | [Right n, Left knN, Left x] <- args
  = do szN <- typeSize n (Just knN)
       reifyNat szN (\pN -> go pN x)

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall m. (KnownNat m) => Proxy m -> Value -> Eval Value
  go Proxy x = do
    a <- fromValueForce @(Unsigned m) x
    resTy <- resultType pr args
    toValue (size# a) resTy

primPack :: PrimImpl
primPack pr args
  | [Right n, Left x] <- args
  = do szN <- typeSize n Nothing
       reifyNat szN (\pN -> go pN x)

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall m. (KnownNat m) => Proxy m -> Value -> Eval Value
  go Proxy x = do
    a <- fromValueForce @(Unsigned m) x
    resTy <- resultType pr args
    toValue @(BitVector m) (pack# a) resTy

primUnpack :: PrimImpl
primUnpack pr args
  | [Right n, Left knN, Left x] <- args
  = do szN <- typeSize n (Just knN)
       reifyNat szN (\pN -> go pN x)

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall m. (KnownNat m) => Proxy m -> Value -> Eval Value
  go Proxy x = do
    resTy <- resultType pr args
    a <- fromValueForce @(BitVector m) x
    toValue @(Unsigned m) (unpack# a) resTy

extendingNumAUnsigned
  :: (forall m n. (KnownNat m, KnownNat n)
        => Unsigned m -> Unsigned n -> Unsigned (Max m n + 1))
  -> PrimImpl
extendingNumAUnsigned f pr args
  | [Right m, Right n, Left x, Left y] <- args
  = do szM <- typeSize m Nothing
       szN <- typeSize n Nothing
       reifyNat szM (\pM -> reifyNat szN (\pN -> go pM pN x y))

  | [Right m, Right n, Left knM, Left knN, Left x, Left y] <- args
  = do szM <- typeSize m (Just knM)
       szN <- typeSize n (Just knN)
       reifyNat szM (\pM -> reifyNat szN (\pN -> go pM pN x y))

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall m n. (KnownNat m, KnownNat n)
     => Proxy m -> Proxy n -> Value -> Value -> Eval Value
  go Proxy Proxy x y = do
    a <- fromValueForce @(Unsigned m) x
    b <- fromValueForce @(Unsigned n) y
    resTy <- resultType pr args
    toValue @(Unsigned (Max m n + 1)) (f a b) resTy

primTimes :: PrimImpl
primTimes pr args
  | [Right m, Right n, Left x, Left y] <- args
  = do szM <- typeSize m Nothing
       szN <- typeSize n Nothing
       reifyNat szM (\pM -> reifyNat szN (\pN -> go pM pN x y))

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall m n. (KnownNat m, KnownNat n)
     => Proxy m -> Proxy n -> Value -> Value -> Eval Value
  go Proxy Proxy x y = do
    a <- fromValueForce @(Unsigned m) x
    b <- fromValueForce @(Unsigned n) y
    resTy <- resultType pr args
    toValue @(Unsigned (m + n)) (times# a b) resTy

primToInteger :: PrimImpl
primToInteger pr args
  | [Right n, Left x] <- args
  = do szN <- typeSize n Nothing
       reifyNat szN (\pN -> go pN x)

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall m. (KnownNat m) => Proxy m -> Value -> Eval Value
  go Proxy x = do
    a <- fromValueForce @(Unsigned m) x
    resTy <- resultType pr args
    toValue (toInteger# a) resTy

shiftRotateUnsigned
  :: (forall n. (KnownNat n) => Unsigned n -> Int -> Unsigned n)
  -> PrimImpl
shiftRotateUnsigned f pr args
  | [Right n, Left knN, Left x, Left y] <- args
  = do szN <- typeSize n (Just knN)
       reifyNat szN (\pN -> go pN x y)

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall m. (KnownNat m) => Proxy m -> Value -> Value -> Eval Value
  go Proxy x y = do
    a <- fromValueForce @(Unsigned m) x
    b <- fromValueForce y
    resTy <- resultType pr args
    toValue @(Unsigned m) (f a b) resTy

primResize :: PrimImpl
primResize pr args
  | [Right n, Right m, Left knM, Left x] <- args
  = do szN <- typeSize n Nothing
       szM <- typeSize m (Just knM)
       reifyNat szN (\pN -> reifyNat szM (\pM -> go pN pM x))

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall n m. (KnownNat n, KnownNat m)
     => Proxy n -> Proxy m -> Value -> Eval Value
  go Proxy Proxy x = do
    a <- fromValueForce @(Unsigned n) x
    resTy <- resultType pr args
    toValue @(Unsigned m) (resize# a) resTy

-- TODO
convertUnsigned
  :: forall n a
   . (KnownNat n, Integral a)
  => (Unsigned n -> a)
  -> PrimImpl
convertUnsigned f pr args
  | [Left x] <- args
  = do a <- fromValueForce @(Unsigned n) x
       env <- getLocalEnv
       resTy <- resultType pr args
       [boxDc] <- resultDataCons resTy
       let w = toInteger (f a)

       pure (VData boxDc [Left (VLiteral (WordLiteral w))] env)

  | otherwise
  = throwM (UnexpectedArgs pr args)
