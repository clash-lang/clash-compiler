module DualBlockRamTypes where

import Clash.Explicit.Prelude

data ThisOrThat
  = This (BitVector 32)
  | That (BitVector 17)
  deriving (Generic, BitPack, Show, ShowX, NFDataX, Lift, Eq)

type Addr = Index 73

type TdpRam (domA :: Domain) (domB :: Domain) =
  -- Clocks
  Clock  domA ->
  Clock  domB ->

  --Operations
  Signal domA (RamOp 73 ThisOrThat) ->
  Signal domB (RamOp 73 ThisOrThat) ->

  --Output
  ( Signal domA ThisOrThat
  , Signal domB ThisOrThat )
