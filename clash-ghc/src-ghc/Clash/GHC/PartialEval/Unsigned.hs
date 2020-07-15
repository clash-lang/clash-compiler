{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.GHC.PartialEval.Unsigned
  ( unsignedPrims
  ) where

import Control.Monad.Trans.Class
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Proxy
import Data.Reflection
import Data.Text (Text)
import GHC.TypeLits
import GHC.TypeLits.Extra

import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Unsigned

import Clash.Core.Evaluator.Models
import Clash.Core.Literal
import Clash.Core.Term (Term(..))

import Clash.GHC.PartialEval.Internal
import Clash.GHC.PartialEval.Internal.Sized

unsignedPrims :: HashMap Text PrimImpl
unsignedPrims = HashMap.fromList
  [ ("Clash.Sized.Internal.Unsigned.fromInteger#", liftId)
  , ("Clash.Sized.Internal.Unsigned.size#", primSize)
  , ("Clash.Sized.Internal.Unsigned.pack#", primPack)
  , ("Clash.Sized.Internal.Unsigned.unpack#", primUnpack)
  , ("Clash.Sized.Internal.Unsigned.eq#", liftComparison eq#)
  , ("Clash.Sized.Internal.Unsigned.neq#", liftComparison neq#)
  , ("Clash.Sized.Internal.Unsigned.ge#", liftComparison ge#)
  , ("Clash.Sized.Internal.Unsigned.gt#", liftComparison gt#)
  , ("Clash.Sized.Internal.Unsigned.le#", liftComparison le#)
  , ("Clash.Sized.Internal.Unsigned.lt#", liftComparison lt#)
  , ("Clash.Sized.Internal.Unsigned.minBound#", liftNullarySized minBound#)
  , ("Clash.Sized.Internal.Unsigned.maxBound#", liftNullarySized maxBound#)
  , ("Clash.Sized.Internal.Unsigned.+#", liftBinarySized (+#))
  , ("Clash.Sized.Internal.Unsigned.-#", liftBinarySized (-#))
  , ("Clash.Sized.Internal.Unsigned.*#", liftBinarySized (*#))
  , ("Clash.Sized.Internal.Unsigned.negate#", liftUnarySized negate#)
  , ("Clash.Sized.Internal.Unsigned.plus#", extendingNumAUnsigned plus#)
  , ("Clash.Sized.Internal.Unsigned.minus#", extendingNumAUnsigned minus#)
  , ("Clash.Sized.Internal.Unsigned.times#", primTimes)
  , ("Clash.Sized.Internal.Unsigned.quot#", liftBinarySized quot#)
  , ("Clash.Sized.Internal.Unsigned.rem#", liftBinarySized rem#)
  , ("Clash.Sized.Internal.Unsigned.toInteger#", primToInteger)
  , ("Clash.Sized.Internal.Unsigned.and#", liftBinarySized and#)
  , ("Clash.Sized.Internal.Unsigned.or#", liftBinarySized or#)
  , ("Clash.Sized.Internal.Unsigned.xor#", liftBinarySized xor#)
  , ("Clash.Sized.Internal.Unsigned.complement#", liftUnarySized complement#)
  , ("Clash.Sized.Internal.Unsigned.shiftL#", shiftRotateUnsigned shiftL#)
  , ("Clash.Sized.Internal.Unsigned.shiftR#", shiftRotateUnsigned shiftR#)
  , ("Clash.Sized.Internal.Unsigned.rotateL#", shiftRotateUnsigned rotateL#)
  , ("Clash.Sized.Internal.Unsigned.rotateR#", shiftRotateUnsigned rotateR#)
  , ("Clash.Sized.Internal.Unsigned.resize#", primResize)
  , ("Clash.Sized.Internal.Unsigned.unsigned8toWord8", convertUnsigned unsigned8toWord8)
  , ("Clash.Sized.Internal.Unsigned.unsigned16toWord16", convertUnsigned unsigned16toWord16)
  , ("Clash.Sized.Internal.Unsigned.unsigned32toWord32", convertUnsigned unsigned32toWord32)
  , ("Clash.Sized.Internal.Unsigned.unsignedtoWord", convertUnsigned unsignedToWord)
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
    a <- fromTermOrValue @(Unsigned m) e x
    resTy <- resultType p args
    toValue (size# a) resTy

primPack :: PrimImpl
primPack e p args
  | [Right n, Left x] <- args
  = do sz <- typeSize n Nothing
       reifyNat sz (\proxy -> go proxy x)

  | otherwise
  = empty
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> PrimEval Value
  go Proxy x = do
    a <- fromTermOrValue @(Unsigned m) e x
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
    resTy <- resultType p args
    a <- fromTermOrValue @(BitVector m) e x
    toValue @(Unsigned m) (unpack# a) resTy

extendingNumAUnsigned
  :: (forall m n. (KnownNat m, KnownNat n)
        => Unsigned m -> Unsigned n -> Unsigned (Max m n + 1))
  -> PrimImpl
extendingNumAUnsigned f e p args
  | [Right m, Right n, Left x, Left y] <- args
  = do szM <- typeSize m Nothing
       szN <- typeSize n Nothing
       reifyNat szM (\pm -> reifyNat szN (\pn -> go pm pn x y))

  | [Right m, Right n, Left knM, Left knN, Left x, Left y] <- args
  = do szM <- typeSize m (Just knM)
       szN <- typeSize n (Just knN)
       reifyNat szM (\pm -> reifyNat szN (\pn -> go pm pn x y))

  | otherwise
  = empty
 where
  go :: forall m n. (KnownNat m, KnownNat n)
     => Proxy m -> Proxy n -> Term -> Term -> PrimEval Value
  go Proxy Proxy x y = do
    a <- fromTermOrValue @(Unsigned m) e x
    b <- fromTermOrValue @(Unsigned n) e y
    resTy <- resultType p args
    toValue @(Unsigned (Max m n + 1)) (f a b) resTy

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
    a <- fromTermOrValue @(Unsigned m) e x
    b <- fromTermOrValue @(Unsigned n) e y
    resTy <- resultType p args
    toValue @(Unsigned (m + n)) (times# a b) resTy

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
    a <- fromTermOrValue @(Unsigned m) e x
    resTy <- resultType p args
    toValue (toInteger# a) resTy

shiftRotateUnsigned
  :: (forall n. (KnownNat n) => Unsigned n -> Int -> Unsigned n)
  -> PrimImpl
shiftRotateUnsigned f e p args
  | [Right n, Left knN, Left x, Left y] <- args
  = do sz <- typeSize n (Just knN)
       reifyNat sz (\proxy -> go proxy x y)

  | otherwise
  = empty
 where
  go :: forall m. (KnownNat m) => Proxy m -> Term -> Term -> PrimEval Value
  go Proxy x y = do
    a <- fromTermOrValue @(Unsigned m) e x
    b <- fromTermOrValue e y
    resTy <- resultType p args
    toValue @(Unsigned m) (f a b) resTy

primResize :: PrimImpl
primResize e p args
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
    a <- fromTermOrValue @(Unsigned n) e x
    resTy <- resultType p args
    toValue @(Unsigned m) (resize# a) resTy

convertUnsigned
  :: forall n a
   . (KnownNat n, Integral a)
  => (Unsigned n -> a)
  -> PrimImpl
convertUnsigned f e p args
  | [Left x] <- args
  = do a <- fromTermOrValue @(Unsigned n) e x
       env <- lift getLocalEnv
       resTy <- resultType p args
       [boxDc] <- typeDcs resTy
       let w = toInteger (f a)

       pure (VData boxDc [Left (Literal (WordLiteral w))] env)

  | otherwise
  = empty

