{-# OPTIONS_GHC -Wno-orphans #-}

module DualBlockRamDefinitions where

import qualified Prelude as P

import Clash.Explicit.Prelude
import Clash.Explicit.BlockRam
import Data.Bifunctor (bimap)

createDomain vSystem{vName="A", vPeriod=hzToPeriod 20e6} -- fast
createDomain vSystem{vName="B", vPeriod=hzToPeriod 10e6} -- slow
createDomain vSystem{vName="C", vPeriod=hzToPeriod 7e6} -- slower

data ThisOrThat
  = This (BitVector 32)
  | That (BitVector 17)
  deriving (Generic, BitPack, Show, ShowX, NFDataX, Lift, Eq)

type Addr = Index 73

simple ::
  ( KnownDomain domA
  , KnownDomain domB
  ) =>

  -- port A
  Clock  domA ->
  Signal domA (Maybe ThisOrThat) ->
  Signal domA Addr ->

  -- port B
  Clock  domB ->
  Signal domB (Maybe ThisOrThat) ->
  Signal domB Addr ->

  ( Signal domA ThisOrThat
  , Signal domB ThisOrThat )
simple = trueDualPortBlockRam

simpleBv ::
  ( KnownDomain domA
  , KnownDomain domB
  ) =>

  -- port A
  Clock  domA ->
  Signal domA (Maybe ThisOrThat) ->
  Signal domA Addr ->

  -- port B
  Clock  domB ->
  Signal domB (Maybe ThisOrThat) ->
  Signal domB Addr ->

  ( Signal domA (BitVector (BitSize ThisOrThat))
  , Signal domB (BitVector (BitSize ThisOrThat)) )
simpleBv clkA datA addrA clkB datB addrB =
  bimap
    (fmap pack)
    (fmap pack)
    (trueDualPortBlockRam clkA datA addrA clkB datB addrB)
