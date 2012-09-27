{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeOperators         #-}
module CLaSH.Sized.Index where

import Unsafe.Coerce(unsafeCoerce)
import GHC.TypeLits

import CLaSH.Sized.Unsigned

data Index :: Nat -> * where
  O :: Index (s + 1)
  S :: Index s -> Index (s + 1)

u2i :: Unsigned n -> Index (2^n)
u2i (U 0) = unsafeCoerce O
u2i (U n) = unsafeCoerce $ S (u2i (U $ n-1))
