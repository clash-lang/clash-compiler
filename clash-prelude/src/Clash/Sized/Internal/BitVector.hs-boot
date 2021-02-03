{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}
module Clash.Sized.Internal.BitVector where

import GHC.TypeLits (KnownNat,Nat)
import GHC.Stack    (HasCallStack)
import Data.Kind    (Type)

type role BitVector nominal
data BitVector :: Nat -> Type
data Bit

undefError :: (HasCallStack, KnownNat n) => String -> [BitVector n] -> a
