{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE PolyKinds      #-}
module CLaSH.Promoted.Bool where

-- | Type-level if-then-else
type family If (x :: Bool) (y :: k) (z :: k) :: k
  where
    If True  y z = y
    If False y z = z
