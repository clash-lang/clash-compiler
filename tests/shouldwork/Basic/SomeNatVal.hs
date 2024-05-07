{-# LANGUAGE ScopedTypeVariables #-}
module SomeNatVal where

import Clash.Prelude
import GHC.TypeNats (SomeNat(..), sameNat, someNatVal)
import GHC.Natural

topEntity ::  Bool
topEntity = case (someNatVal 1, someNatVal 2) of
  (SomeNat y, SomeNat z) -> case sameNat y z of
    Just _ -> True
    _ -> False
