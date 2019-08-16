{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
module Clash.Sized.Internal.Index where

import Data.Kind (Type)
import GHC.TypeLits (KnownNat, Nat, type (<=))

type role SatIndex phantom phantom
data SatIndex :: SaturationMode -> Nat -> Type

instance (KnownSatMode sat, KnownNat n, 1 <= n) => Num (SatIndex sat n)
toInteger# :: SatIndex sat n -> Integer
