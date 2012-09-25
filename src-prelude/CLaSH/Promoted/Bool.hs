{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE PolyKinds      #-}
module CLaSH.Promoted.Bool where

import GHC.TypeLits

type family If (x :: Bool) (y :: k) (z :: k) :: k

type instance If True  y z = y
type instance If False y z = z
