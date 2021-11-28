{-|
Copyright  :  (C) 2021, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

This module exists purely to speed up doctests of "Clash.Sized.Vector". It should
never be imported by user code.
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Clash.Sized.Internal.Vector where

import Prelude ()
import Clash.Prelude
import Data.Singletons (Proxy(..), Apply, TyFun)

compareSwapL :: Ord b => b -> b -> (b, b)
compareSwapL a b = if a < b then (a,b) else (b,a)

sortV :: Ord a => Vec ((n + 1) + 1) a -> Vec ((n + 1) + 1) a
sortV xs = map fst sorted :< (snd (last sorted))
 where
  lefts  = head xs :> map snd (init sorted)
  rights = tail xs
  sorted = zipWith compareSwapL lefts rights

sortVL :: (Ord a, KnownNat (n + 1)) => Vec ((n + 1) + 1) a -> Vec ((n + 1) + 1) a
sortVL xs = map fst sorted :< (snd (last sorted))
 where
  lefts  = head xs :> map snd (init sorted)
  rights = tail xs
  sorted = zipWith compareSwapL (lazyV lefts) rights

sortV_flip :: Ord a => Vec ((n + 1) + 1) a -> Vec ((n + 1) + 1) a
sortV_flip xs = map fst sorted :< (snd (last sorted))
 where
  lefts  = head xs :> map snd (init sorted)
  rights = tail xs
  sorted = zipWith (flip compareSwapL) rights lefts

data Append (m :: Nat) (a :: Type) (f :: TyFun Nat Type) :: Type
type instance Apply (Append m a) l = Vec (l + m) a

append' :: KnownNat k => Vec k a -> Vec m a -> Vec (k + m) a
append' xs ys = dfold (Proxy :: Proxy (Append m a)) (const (:>)) ys xs

compareSwap :: Ord b => b -> b -> (b, b)
compareSwap a b = if a > b then (a,b) else (b,a)

insert :: Ord a => a -> Vec n a -> Vec (n + 1) a
insert y xs = let (y',xs') = mapAccumL compareSwap y xs in xs' :< y'

insertionSort :: (Ord a, KnownNat n) => Vec n a -> Vec n a
insertionSort = vfold (const insert)

data IIndex (f :: TyFun Nat Type) :: Type
type instance Apply IIndex l = Index ((2^l)+1)

populationCount' :: (KnownNat k, KnownNat (2^k)) => BitVector (2^k) -> Index ((2^k)+1)
populationCount' bv = dtfold (Proxy @IIndex) fromIntegral (\_ x y -> add x y) (bv2v bv)
