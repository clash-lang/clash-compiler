{-# LANGUAGE ScopedTypeVariables #-}
module SomeNatVal where

import Clash.Prelude hiding (someNatVal)
import GHC.TypeNats (someNatVal)
import GHC.Natural

topEntity ::  Bool
topEntity = case (someNatVal 1, someNatVal 2) of
  (SomeNat y, SomeNat z) -> case sameNat y z of
    Just _ -> True
    _ -> False
