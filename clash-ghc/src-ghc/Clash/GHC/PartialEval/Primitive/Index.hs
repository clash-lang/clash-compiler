{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.GHC.PartialEval.Primitive.Index
  ( indexPrims
  ) where

import Control.Monad.Catch (throwM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Proxy
import Data.Reflection
import Data.Text (Text)
import Data.Type.Equality
import GHC.TypeLits
import GHC.TypeLits.Extra (CLog)
import Unsafe.Coerce

import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Index

import Clash.Core.PartialEval.Monad
import Clash.Core.PartialEval.NormalForm

import Clash.GHC.PartialEval.Primitive.FromAst
import Clash.GHC.PartialEval.Primitive.Info
import Clash.GHC.PartialEval.Primitive.Strategy
import Clash.GHC.PartialEval.Primitive.ToAst
import Clash.GHC.PartialEval.Primitive.Internal.Sized

indexPrims :: HashMap Text PrimImpl
indexPrims = HashMap.fromList
  [ ("Clash.Class.Exp.expIndex#", liftId)
  , ("Clash.Sized.Internal.Index.*#", liftBinarySized (*#))
  , ("Clash.Sized.Internal.Index.+#", liftBinarySized (+#))
  , ("Clash.Sized.Internal.Index.-#", liftBinarySized (-#))
  , ("Clash.Sized.Internal.Index.eq#", liftComparison eq#)
  , ("Clash.Sized.Internal.Index.fromInteger#", liftId)
  , ("Clash.Sized.Internal.Index.ge#", liftComparison ge#)
  , ("Clash.Sized.Internal.Index.gt#", liftComparison gt#)
  , ("Clash.Sized.Internal.Index.le#", liftComparison le#)
  , ("Clash.Sized.Internal.Index.lt#", liftComparison lt#)
  , ("Clash.Sized.Internal.Index.maxBound#", liftNullarySized maxBound#)
  , ("Clash.Sized.Internal.Index.minus#", extendingNumAIndex minus#)
  , ("Clash.Sized.Internal.Index.neq#", liftComparison neq#)
  , ("Clash.Sized.Internal.Index.pack#", liftUnary pack#)
  , ("Clash.Sized.Internal.Index.plus#", extendingNumAIndex plus#)
  , ("Clash.Sized.Internal.Index.quot#", liftBinarySized quot#)
  , ("Clash.Sized.Internal.Index.rem#", liftBinarySized rem#)
  , ("Clash.Sized.Internal.Index.resize#", primResize)
  , ("Clash.Sized.Internal.Index.times#", primTimes)
  , ("Clash.Sized.Internal.Index.toInteger#", primToInteger)
  , ("Clash.Sized.Internal.Index.unpack#", primUnpack)
  ]

extendingNumAIndex
  :: (forall m n. (KnownNat m, KnownNat n)
        => Index m -> Index n -> Index ((m + n) - 1))
  -> PrimImpl
extendingNumAIndex f pr args
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
    a <- fromValueForce @(Index m) x
    b <- fromValueForce @(Index n) y
    resTy <- resultType pr args
    toValue @(Index ((m + n) - 1)) (f a b) resTy

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
    a <- fromValueForce @(Index m) x
    b <- fromValueForce @(Index n) y
    resTy <- resultType pr args
    toValue @(Index ((m - 1) * (n - 1) + 1)) (times# a b) resTy

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
    a <- fromValueForce @(Index m) x
    resTy <- resultType pr args
    toValue (toInteger# a) resTy

primResize :: PrimImpl
primResize pr args
  | [Right m, Right n, Left knM, Left x] <- args
  = do szM <- typeSize m (Just knM)
       szN <- typeSize n Nothing
       reifyNat szM (\pM -> reifyNat szN (\pN -> go pM pN x))

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall m n. (KnownNat m, KnownNat n)
     => Proxy m -> Proxy n -> Value -> Eval Value
  go Proxy Proxy x = do
    a <- fromValueForce @(Index m) x
    resTy <- resultType pr args
    toValue @(Index n) (resize# a) resTy

primUnpack :: PrimImpl
primUnpack pr args
  | [Right n, Left _, Left knN, Left x] <- args
  = do szN <- typeSize n (Just knN)
       reifyNat szN (\pN -> go pN x)

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall m. (KnownNat m) => Proxy m -> Value -> Eval Value
  go proxy x = do
    a <- fromValueForce @(BitVector (CLog 2 m)) x
    resTy <- resultType pr args

    case sizeProof proxy of
      Refl -> toValue @(Index m) (unpack# a) resTy

  sizeProof :: Proxy m -> ((1 <=? m) :~: 'True)
  sizeProof = unsafeCoerce Refl
