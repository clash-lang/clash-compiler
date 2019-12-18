{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE RoleAnnotations #-}
module Clash.Sized.Internal.Index where

import Data.Kind (Type)
import GHC.TypeLits (KnownNat, Nat)

type role Index phantom
data Index :: Nat -> Type

instance KnownNat n => Num (Index n)
toInteger# :: Index n -> Integer
