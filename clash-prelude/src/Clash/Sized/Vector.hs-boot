{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module Clash.Sized.Vector where

import {-# SOURCE #-} Clash.Sized.Internal.BitVector (Bit, BitVector)
import Data.Kind (Type)
import GHC.TypeLits (KnownNat, Nat)

type role Vec nominal representational

data Vec :: Nat -> Type -> Type

instance (KnownNat n) => Foldable (Vec n)

bv2v :: (KnownNat n) => BitVector n -> Vec n Bit
map :: (a -> b) -> Vec n a -> Vec n b
foldr :: (a -> b -> b) -> b -> Vec n a -> b
foldl :: (b -> a -> b) -> b -> Vec n a -> b
