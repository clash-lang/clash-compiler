{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin=GHC.TypeLits.Normalise #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
module DualBlockRamTypes where
import Clash.Explicit.Prelude
import Control.DeepSeq
data ThisOrThat
  = This (BitVector 32)
  | That (BitVector 17)
  deriving (Generic, BitPack, Show, ShowX, NFDataX, Lift, Eq)

type Addr = Index 30

instance NFData ThisOrThat where
  rnf (This x) = rnf x
  rnf (That y) = rnf y
