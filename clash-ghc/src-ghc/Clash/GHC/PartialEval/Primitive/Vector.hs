{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Primitive.Vector
  ( vectorPrims
  ) where

import Control.Monad (when)
import Control.Monad.Catch (throwM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Proxy
import Data.Reflection (reifyNat)
import Data.Text (Text)
import GHC.TypeLits (KnownNat, type (*))

import Clash.Promoted.Nat (snatProxy)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Vector (Vec)
import qualified Clash.Sized.Vector as Vec

import Clash.Core.DataCon
import Clash.Core.Literal
import Clash.Core.Name
import Clash.Core.PartialEval.Monad
import Clash.Core.PartialEval.NormalForm
import Clash.Core.Type
import Clash.Core.TysPrim (intPrimTy)

import {-# SOURCE #-} Clash.GHC.PartialEval.Eval
import Clash.GHC.PartialEval.Primitive.FromAst
import Clash.GHC.PartialEval.Primitive.Info
import Clash.GHC.PartialEval.Primitive.Strategy
import Clash.GHC.PartialEval.Primitive.ToAst

vectorPrims :: HashMap Text PrimImpl
vectorPrims = HashMap.fromList
  [ ("Clash.Sized.Vector.++", coreUnfolding)
  , ("Clash.Sized.Vector.concat", coreUnfolding)
  , ("Clash.Sized.Vector.concatBitVector#", primConcatBitVector)
  , ("Clash.Sized.Vector.dfold", coreUnfolding)
  , ("Clash.Sized.Vector.dtfold", coreUnfolding)
  , ("Clash.Sized.Vector.fold", primFold)
  , ("Clash.Sized.Vector.foldr", coreUnfolding)
  , ("Clash.Sized.Vector.head", coreUnfolding)
  , ("Clash.Sized.Vector.imap", coreUnfolding)
  , ("Clash.Sized.Vector.index_int", primIndexInt)
  , ("Clash.Sized.Vector.init", coreUnfolding)
  , ("Clash.Sized.Vector.iterateI", coreUnfolding)
  , ("Clash.Sized.Vector.last", coreUnfolding)
  , ("Clash.Sized.Vector.lazyV", coreUnfolding)
  , ("Clash.Sized.Vector.length", coreUnfolding)
  , ("Clash.Sized.Vector.map", coreUnfolding)
  , ("Clash.Sized.Vector.replace_int", primReplaceInt)
  , ("Clash.Sized.Vector.replicate", primReplicate)
  , ("Clash.Sized.Vector.reverse", coreUnfolding)
  , ("Clash.Sized.Vector.rotateLeftS", coreUnfolding)
  , ("Clash.Sized.Vector.rotateRightS", coreUnfolding)
  -- TODO select
  , ("Clash.Sized.Vector.splitAt", primSplitAt)
  , ("Clash.Sized.Vector.tail", coreUnfolding)
  , ("Clash.Sized.Vector.transpose", coreUnfolding)
  , ("Clash.Sized.Vector.traverse#", coreUnfolding)
  -- TODO unconcat
  , ("Clash.Sized.Vector.unconcatBitVector#", primUnconcatBitVector)
  , ("Clash.Sized.Vector.zipWith", coreUnfolding)
  ]

primIndexInt :: PrimImpl
primIndexInt pr args
  | [Right n, Right a, Left knN, Left x, Left y] <- args
  = do szN <- typeSize n (Just knN)
       ix  <- fromValueForce @Int y
       when (ix < 0) (throwM ResultUndefined)

       forceEval x >>= \case
         VData dc dcArgs _
           | nameOcc (dcName dc) == "Clash.Sized.Vector.Vec.Nil" ->
               throwM ResultUndefined

           | nameOcc (dcName dc) == "Clash.Sized.Vector.Vec.Cons"
           , [_, _, _, _, Left el, Left rest] <- dcArgs ->
               if ix == 0 then forceEval el else do
                 ix' <- toValue (ix - 1) intPrimTy
                 let n'   = LitTy (NumTy (szN - 1))
                     knN' = VLiteral (NaturalLiteral (szN - 1))

                 primIndexInt pr
                   [Right n', Right a, Left knN', Left rest, Left ix']

         -- Not unexpected: the argument might be neutral, e.g. x :: Vec n a
         _ -> empty

  | otherwise
  = throwM (UnexpectedArgs pr args)

primReplaceInt :: PrimImpl
primReplaceInt pr args
  | [Right n, Right a, Left knN, Left x, Left y, new] <- args
  = do szN <- typeSize n (Just knN)
       ix  <- fromValueForce @Int y
       when (ix < 0 || szN <= toInteger ix) (throwM ResultUndefined)

       forceEval x >>= \case
         VData dc dcArgs env
           | nameOcc (dcName dc) == "Clash.Sized.Vector.Vec.Nil" ->
               throwM ResultUndefined

           | nameOcc (dcName dc) == "Clash.Sized.Vector.Vec.Cons"
           , [nTy, aTy, mTy, co, el, rest] <- dcArgs ->
               if ix == 0 then
                 pure (VData dc [nTy, aTy, mTy, co, new, rest] env)
               else do
                 ix' <- toValue (ix - 1) intPrimTy
                 let n'   = LitTy (NumTy (szN - 1))
                     knN' = VLiteral (NaturalLiteral (szN - 1))

                 newRest <- primReplaceInt pr [Right n', Right a, Left knN', rest, Left ix', new]
                 pure (VData dc [nTy, aTy, mTy, co, el, Left newRest] env)

         -- Not unexpected: the argument might be neutral, e.g. x :: Vec n a
         _ -> empty

  | otherwise
  = throwM (UnexpectedArgs pr args)

primReplicate :: PrimImpl
primReplicate pr args
  | [Right nTy, Right _, Left _, Left y] <- args
  = do szN <- typeSize nTy Nothing
       reifyNat szN (\pN -> go pN y)

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall n. (KnownNat n) => Proxy n -> Value -> Eval Value
  go pN x = do
    resTy <- resultType pr args
    let sN = snatProxy pN

    toValue @(Vec n Value) (Vec.replicate sN x) resTy

primSplitAt :: PrimImpl
primSplitAt pr args
  | [Right m, Right _, Right _, Left _, Left x] <- args
  = do szM <- typeSize m Nothing
       reifyNat szM (\pM -> go pM szM [] x)

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall m. (KnownNat m)
          => Proxy m -> Integer -> [Value] -> Value -> Eval Value
  go Proxy 0 acc rest = do
    resTy <- resultType pr args
    let lhs = Vec.unsafeFromList @m (reverse acc)

    toValue (lhs, rest) resTy

  go pM i acc rest =
    forceEval rest >>= \case
      VData dc dcArgs _
        |  nameOcc (dcName dc) == ""
        -> throwM ResultUndefined

        |  nameOcc (dcName dc) == ""
        ,  [_, _, _, Left x, Left xs] <- dcArgs
        -> go pM (i - 1) (x:acc) xs

      _ -> empty

primConcatBitVector :: PrimImpl
primConcatBitVector pr args
  | [Right n, Right m, Left knN, Left knM, Left x] <- args
  = do szN <- typeSize n (Just knN)
       szM <- typeSize m (Just knM)
       reifyNat szN (\pN -> reifyNat szM (\pM -> go pN pM x))

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall n m. (KnownNat n, KnownNat m)
     => Proxy n -> Proxy m -> Value -> Eval Value
  go Proxy Proxy x = do
    a <- fromValueForce @(Vec n (BitVector m)) x
    resTy <- resultType pr args

    toValue @(BitVector (n * m)) (Vec.concatBitVector# @n @m a) resTy

primUnconcatBitVector :: PrimImpl
primUnconcatBitVector pr args
  | [Right n, Right m, Left knN, Left knM, Left x] <- args
  = do szN <- typeSize n (Just knN)
       szM <- typeSize m (Just knM)
       reifyNat szN (\pN -> reifyNat szM (\pM -> go pN pM x))

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall n m. (KnownNat n, KnownNat m)
     => Proxy n -> Proxy m -> Value -> Eval Value
  go Proxy Proxy x = do
    a <- fromValueForce @(BitVector (n * m)) x
    resTy <- resultType pr args

    toValue @(Vec n (BitVector m)) (Vec.unconcatBitVector# @n @m a) resTy

primFold :: PrimImpl
primFold pr args
  | [Right n, Right a, Left x, Left y] <- args
  = do szN <- typeSize n Nothing
       reifyNat szN (\pN -> go pN a x y)

  | otherwise
  = throwM (UnexpectedArgs pr args)
 where
  go :: forall n. (KnownNat n) => Proxy n -> Type -> Value -> Value -> Eval Value
  go Proxy resTy f x = do
    vec <- fromValueForce @(Vec n Value) x
    result <- goList f (Vec.toList vec)
    toValue result resTy

  goList f = \case
    []  -> throwM ResultUndefined
    [x] -> pure x
    xs  -> do
      let (ys, zs) = splitAt (length xs `div` 2) xs
      fYs <- goList f ys
      fZs <- goList f zs

      (f `apply` fYs) >>= (`apply` fZs)
