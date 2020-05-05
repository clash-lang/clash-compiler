{-# LANGUAGE GADTs #-}
module T1311 where

import Clash.Prelude

data SomeSNat where
  SomeSNat :: forall m . SNat m -> SomeSNat

topEntity :: SomeSNat -> Bool
topEntity = const True
