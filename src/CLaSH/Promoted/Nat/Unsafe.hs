{-# LANGUAGE Unsafe #-}

{-|
Copyright  :  (C) 2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}
module CLaSH.Promoted.Nat.Unsafe
  (unsafeSNat)
where

import Data.Reflection    (reifyNat)
import Unsafe.Coerce      (unsafeCoerce)

import CLaSH.Promoted.Nat (SNat (..))

-- | I hope you know what you're doing
unsafeSNat :: Integer -> SNat k
unsafeSNat i = reifyNat i (unsafeCoerce . SNat)
{-# NOINLINE unsafeSNat #-}
