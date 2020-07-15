{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}

module Clash.GHC.PartialEval.Internal.Sized where

import Data.Proxy
import Data.Reflection (reifyNat)
import GHC.TypeLits (KnownNat)

import Clash.Core.Evaluator.Models
import Clash.Core.Term (Term)

import Clash.GHC.PartialEval.Internal

-- | Lift a primitive that represents some constant value in a sized type.
-- Examples of this are undefined#, or minBound# / maxBound#.
--
liftNullarySized
  :: forall f
   . (forall size. (KnownNat size) => ToAst (f size))
  => (forall n. (KnownNat n) => f n)
  -> PrimImpl
liftNullarySized f _ p args
  | [Right n] <- args
  = do sz <- typeSize n Nothing
       reifyNat sz go

  | [Right n, Left knN] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz go

  | otherwise
  = empty
 where
  go :: forall m. (KnownNat m) => Proxy m -> PrimEval Value
  go Proxy = resultType p args >>= toValue @(f m) f

-- | Lift a primitive that represents some unary function over a sized type.
-- Examples of this are complement# and negate#.
--
liftUnarySized
  :: forall f
   . ( forall size. FromAst (f size)
     , forall size. ToAst (f size)
     )
  => (forall n. (KnownNat n) => f n -> f n)
  -> PrimImpl
liftUnarySized f e p args
  | [Right n, Left x] <- args
  = do sz <- typeSize n Nothing
       reifyNat sz (\proxy -> go proxy x)

  | [Right n, Left knN, Left x] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz (\proxy -> go proxy x)

  | otherwise
  = empty
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> PrimEval Value
  go Proxy x = do
    a <- fromTermOrValue @(f m) e x
    resTy <- resultType p args
    toValue @(f m) (f a) resTy

-- | Lift a primitive that represents some binary function over a sized type.
-- Examples of this are (+), (*) and xor.
--
liftBinarySized
  :: forall f
   . ( forall size. FromAst (f size)
     , forall size. ToAst (f size)
     )
  => (forall n. (KnownNat n) => f n -> f n -> f n)
  -> PrimImpl
liftBinarySized f e p args
  | [Right n, Left x, Left y] <- args
  = do sz <- typeSize n Nothing
       reifyNat sz (\proxy -> go proxy x y)

  | [Right n, Left knN, Left x, Left y] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz (\proxy -> go proxy x y)

  | otherwise
  = empty
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> Term -> PrimEval Value
  go Proxy x y = do
    a <- fromTermOrValue @(f m) e x
    b <- fromTermOrValue @(f m) e y
    resTy <- resultType p args
    toValue @(f m) (f a b) resTy

liftComparison
  :: forall f
   . (forall size. FromAst (f size))
  => (forall n. (KnownNat n) => f n -> f n -> Bool)
  -> PrimImpl
liftComparison f e p args
  | [Right n, Left x, Left y] <- args
  = do sz <- typeSize n Nothing
       reifyNat sz (\proxy -> go proxy x y)

  | [Right n, Left knN, Left x, Left y] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz (\proxy -> go proxy x y)

  | otherwise
  = empty
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> Term -> PrimEval Value
  go Proxy x y = do
    a <- fromTermOrValue @(f m) e x
    b <- fromTermOrValue @(f m) e y
    resTy <- resultType p args
    toValue (f a b) resTy

