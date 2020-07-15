{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Vector
  ( vectorPrims
  ) where

import Prelude hiding (replicate)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Proxy
import Data.Reflection (reifyNat)
import Data.Text (Text)
import GHC.TypeLits (KnownNat, type (*))

import Clash.Promoted.Nat
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Vector (Vec, replicate, unconcatBitVector#)

import Clash.Core.Evaluator.Models
import Clash.Core.Literal
import Clash.Core.Term (Term(..), mkApps)
import Clash.Core.Type

import Clash.GHC.PartialEval.Internal

vectorPrims :: HashMap Text PrimImpl
vectorPrims = HashMap.fromList
  [ ("Clash.Sized.Vector.length", primLength)
  , ("Clash.Sized.Vector.index_int", primIndexInt)
  , ("Clash.Sized.Vector.map", primMap)
  , ("Clash.Sized.Vector.reverse", primReverse)
  , ("Clash.Sized.Vector.replicate", primReplicate)
  , ("Clash.Sized.Vector.lazyV", primLazyV)
  , ("Clash.Sized.Vector.unconcatBitVector#", primUnconcatBitVector)
  ]

primLength :: PrimImpl
primLength _ p args
  | [Right n, Right _a, Left knN, Left _xs] <- args
  = do sz <- typeSize n (Just knN)
       resTy <- resultType p args
       toValue @Int (fromInteger sz) resTy

  | otherwise
  = empty

primIndexInt :: PrimImpl
primIndexInt e _p args
  | [Right nTy, Right _aTy, Left knN, Left x, Left y] <- args
  = do sz  <- typeSize nTy (Just knN)
       vec <- fromTermOrValue e x
       ix  <- fromTermOrValue e y

       if ix < fromInteger sz
         then do
           a <- go ix vec
           lift (evaluateWhnf e a)

         else empty

  | otherwise
  = error ("primIndexInt: " <> show args)
 where
  go :: Int -> LVec Term -> PrimEval Term
  go i = \case
    LNil -> empty -- TODO make this undefined?
    LCons x xs
      | i == 0 -> pure x
      | otherwise -> do
          vec <- fromTermOrValue e xs
          go (i - 1) vec

primMap :: PrimImpl
primMap e p args
  | [Right aTy, Right bTy, Right nTy, Left x, Left y] <- args
  = do sz <- typeSize nTy Nothing
       a <- fromTermOrValue @(LVec Term) e y
       resTy <- resultType p args

       case a of
         LNil ->
           toValue a resTy

         LCons z zs -> do
            let pnTy = LitTy (NumTy (sz - 1))
                tl = mkApps (Prim p) [Right aTy, Right bTy, Right pnTy, Left x, Left zs]

            hd <- lift $ evaluateWhnf e (App x z)
            toValue (LCons hd tl) resTy

  | otherwise
  = error ("primMap: " <> show args)

primReverse :: PrimImpl
primReverse e p args
  | [Right _nTy, Right aTy, Left x] <- args
  = do resTy <- resultType p args
       (vecTcn, _, _) <- typeInfo resTy
       let vecTy i = mkTyConApp vecTcn [LitTy (NumTy i), aTy]
       acc <- toTerm @(LVec Term) LNil (vecTy 0)
       go vecTy (0, acc) x

  | otherwise
  = error ("primReverse: " <> show args)
 where
  go :: (Integer -> Type) -> (Integer, Term) -> Term -> PrimEval Value
  go vecTy (i, acc) x = do
    a <- fromTermOrValue @(LVec Term) e x

    case a of 
      LNil -> lift (evaluateWhnf e acc)
      LCons y ys -> do
        tl <- toTerm (LCons y acc) (vecTy (i + 1))
        go vecTy (i + 1, tl) ys

primReplicate :: PrimImpl
primReplicate e p args
  | [Right nTy, Right _aTy, Left _x, Left y] <- args
  = do sz <- typeSize nTy Nothing
       reifyNat sz (\pN -> go pN y)

  | otherwise
  = error ("primReplicate: " <> show args)
 where
  go :: forall n. (KnownNat n) => Proxy n -> Term -> PrimEval Value
  go pN x = do
    resTy <- resultType p args
    let sN = snatProxy pN

    toValue @(Vec n Term) (replicate sN x) resTy
    

primLazyV :: PrimImpl
primLazyV e p args
  | [Right nTy, Right aTy, Left knN, Left x] <- args
  = do sz <- typeSize nTy (Just knN)
       a <- fromTermOrValue @(LVec Term) e x
       resTy <- resultType p args

       case a of
         LNil ->
           toValue a resTy

         LCons y ys ->
           let pnTy = LitTy (NumTy (sz - 1))
               pnKn = Literal (NaturalLiteral (sz - 1))
               tl = mkApps (Prim p) [Right pnTy, Right aTy, Left pnKn, Left ys]
            in toValue (LCons y tl) resTy

  | otherwise
  = error ("primLazyV: " <> show args)

primUnconcatBitVector :: PrimImpl
primUnconcatBitVector e p args
  | [Right n, Right m, Left knN, Left knM, Left x] <- args
  = do szN <- typeSize n (Just knN)
       szM <- typeSize m (Just knM)
       reifyNat szN (\pN -> reifyNat szM (\pM -> go pN pM x))

  | otherwise
  = error "ARG MISMATCH"
 where
  go :: forall n m. (KnownNat n, KnownNat m)
     => Proxy n -> Proxy m -> Term -> PrimEval Value
  go Proxy Proxy x = do
    a <- fromTermOrValue @(BitVector (n * m)) e x
    resTy <- resultType p args

    toValue @(Vec n (BitVector m)) (unconcatBitVector# @n @m a) resTy

