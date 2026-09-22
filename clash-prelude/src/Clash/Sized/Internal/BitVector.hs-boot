{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RoleAnnotations #-}

{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module Clash.Sized.Internal.BitVector where

import Data.Kind (Type)
import GHC.TypeLits (KnownNat, Nat)

type role BitVector nominal

data BitVector :: Nat -> Type

data Bit

undefError :: (KnownNat n) => String -> [BitVector n] -> a
