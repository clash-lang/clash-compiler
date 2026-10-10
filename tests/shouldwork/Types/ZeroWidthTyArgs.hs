-- | An argument of a zero-width type with a type argument. 'zeroWidthSpec'
-- substitutes the only value of such a type for the argument, but constructed
-- it without applying the type arguments, failing with "Unexpected
-- application".
module ZeroWidthTyArgs where

import Clash.Prelude
import Data.Proxy

data T (n :: Nat) = T (Proxy n)

f :: T n -> Signed 8 -> Signed 8
f t x = case t of T _ -> x + 1
{-# OPAQUE f #-}

topEntity :: T 3 -> Signed 8 -> Signed 8
topEntity = f
