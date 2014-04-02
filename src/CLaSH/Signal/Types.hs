{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
module CLaSH.Signal.Types where

import GHC.TypeLits       (Nat)
import CLaSH.Promoted.Nat (SNat)

infixr 5 :-
-- | A synchronized signal with elements of type @a@, synchronized to the
-- relative clock @clk@
data Signal a = a :- Signal a

infixr 5 ::-
-- | A synchronized signal with elements of type @a@, synchronized to the
-- relative clock @clk@
data CSignal (clk :: Nat) a = a ::- CSignal clk a

-- | Explicit Clock with relative period @clk@
newtype Clock (clk :: Nat) = Clock (SNat clk)
