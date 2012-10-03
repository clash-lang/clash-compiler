{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module CLaSH.Promoted.Ord where

import GHC.TypeLits

import CLaSH.Promoted.Bool

type family Min (x :: Nat) (y :: Nat) :: Nat
type instance Min x y = If (x <=? y) x y

type family Max (x :: Nat) (y :: Nat) :: Nat
type instance Max x y = If (x <=? y) y x
