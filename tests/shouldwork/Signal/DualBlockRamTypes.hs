{-# OPTIONS_GHC -Wno-orphans #-}

module DualBlockRamTypes where
import Clash.Explicit.Prelude

data ThisOrThat
  = This (BitVector 32)
  | That (BitVector 17)
  deriving (Generic, BitPack, Show, ShowX, NFDataX, Lift, Eq)

type Addr = Index 73
