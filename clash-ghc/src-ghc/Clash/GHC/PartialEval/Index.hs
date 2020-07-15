{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.GHC.PartialEval.Index
  ( indexPrims
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Proxy
import Data.Reflection
import Data.Text (Text)
import GHC.TypeLits

import Clash.Sized.Internal.Index

import Clash.Core.Evaluator.Models
import Clash.Core.Term (Term)

import Clash.GHC.PartialEval.Internal
import Clash.GHC.PartialEval.Internal.Sized

indexPrims :: HashMap Text PrimImpl
indexPrims = HashMap.fromList
  [ ("Clash.Sized.Internal.Index.fromInteger#", liftId)
  , ("Clash.Sized.Internal.Index.pack#", liftUnary pack#)
--, ("Clash.Sized.Internal.Index.unpack#", liftUnary unpack#)
  , ("Clash.Sized.Internal.Index.eq#", liftComparison eq#)
  , ("Clash.Sized.Internal.Index.neq#", liftComparison neq#)
  , ("Clash.Sized.Internal.Index.ge#", liftComparison ge#)
  , ("Clash.Sized.Internal.Index.gt#", liftComparison gt#)
  , ("Clash.Sized.Internal.Index.le#", liftComparison le#)
  , ("Clash.Sized.Internal.Index.lt#", liftComparison lt#)
  , ("Clash.Sized.Internal.Index.maxBound#", liftNullarySized maxBound#)
  , ("Clash.Sized.Internal.Index.+#", liftBinarySized (+#))
  , ("Clash.Sized.Internal.Index.-#", liftBinarySized (-#))
  , ("Clash.Sized.Internal.Index.*#", liftBinarySized (*#))
  , ("Clash.Sized.Internal.Index.plus#", extendingNumAIndex plus#)
  , ("Clash.Sized.Internal.Index.minus#", extendingNumAIndex minus#)
  , ("Clash.Sized.Internal.Index.times#", primTimes)
  , ("Clash.Sized.Internal.Index.quot#", liftBinarySized quot#)
  , ("Clash.Sized.Internal.Index.rem#", liftBinarySized rem#)
  , ("Clash.Sized.Internal.Index.toInteger#", primToInteger)
  , ("Clash.Sized.Internal.Index.resize#", primResize)
  ]

extendingNumAIndex
  :: (forall m n. (KnownNat m, KnownNat n)
        => Index m -> Index n -> Index ((m + n) - 1))
  -> PrimImpl
extendingNumAIndex f e p args
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
    a <- fromTermOrValue @(Index m) e x
    b <- fromTermOrValue @(Index n) e y
    resTy <- resultType p args
    toValue @(Index ((m + n) - 1)) (f a b) resTy

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
    a <- fromTermOrValue @(Index m) e x
    b <- fromTermOrValue @(Index n) e y
    resTy <- resultType p args
    toValue @(Index ((m - 1) * (n - 1) + 1)) (times# a b) resTy

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
    a <- fromTermOrValue @(Index m) e x
    resTy <- resultType p args
    toValue (toInteger# a) resTy

primResize :: PrimImpl
primResize e p args
  | [Right m, Right n, Left knM, Left x] <- args
  = do szM <- typeSize m (Just knM)
       szN <- typeSize n Nothing
       reifyNat szM (\pM -> reifyNat szN (\pN -> go pM pN x))

  | otherwise
  = empty
 where
  go :: forall m n. (KnownNat m, KnownNat n)
     => Proxy m -> Proxy n -> Term -> PrimEval Value
  go Proxy Proxy x = do
    a <- fromTermOrValue @(Index m) e x
    resTy <- resultType p args
    toValue @(Index n) (resize# a) resTy

