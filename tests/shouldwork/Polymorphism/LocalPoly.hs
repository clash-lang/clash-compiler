{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, Rank2Types #-}
module LocalPoly where

import CLaSH.Prelude

topEntity = f

f (x :: Bool) = (second x False, second x high)
  where
    second :: Bool -> (forall a . a -> a)
    second a b = b
