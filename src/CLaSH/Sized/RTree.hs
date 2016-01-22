{-|
Copyright  :  (C) 2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DataKinds, TypeOperators, GADTs, ScopedTypeVariables,
             KindSignatures, RankNTypes #-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module CLaSH.Sized.RTree where

import Data.Singletons.Prelude     (TyFun,type ($))
import Data.Proxy                  (Proxy (..))
import GHC.TypeLits                (KnownNat, Nat, type (+))

import CLaSH.Promoted.Nat          (SNat, snat, subSNat)
import CLaSH.Promoted.Nat.Literals (d1)

data RTree :: Nat -> * -> * where
  LR :: a -> RTree 0 a
  BR :: RTree n a -> RTree n a -> RTree (n+1) a

tfold :: forall p k a . KnownNat k
      => Proxy (p :: TyFun Nat * -> *)
      -> (a -> (p $ 0))
      -> (forall l . SNat l -> (p $ l) -> (p $ l) -> (p $ (l+1)))
      -> RTree k a
      -> (p $ k)
tfold _ f g = go snat
  where
    go :: SNat m -> RTree m a -> (p $ m)
    go _  (LR a)   = f a
    go sn (BR l r) = let sn' = sn `subSNat` d1
                     in  g sn' (go sn' l) (go sn' r)
