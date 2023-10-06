{-|
Copyright  :  (C) 2023, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Configurable model for true dual-port block RAM
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Clash.Explicit.BlockRam.Model where

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative (liftA2)
#endif
import Control.Exception (throw)
import Data.Sequence (Seq)
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat)

import Clash.Promoted.Nat (SNat(..), natToNum)
import Clash.Signal.Bundle (Bundle(bundle))
import Clash.Signal.Internal
  (Clock (..), Signal (..), ClockAB (..), clockTicks)
import Clash.Sized.Index (Index)
import Clash.XException (XException(..), NFDataX(..), seqX)
import Clash.XException.MaybeX (MaybeX(..), toMaybeX, andX)

import qualified Clash.XException.MaybeX as MaybeX

import qualified Data.Sequence as Seq

-- | Helper used in 'getConflict'
data Conflict = Conflict
  { cfRWA :: !(MaybeX Bool) -- ^ Read/Write conflict for output A
  , cfRWB :: !(MaybeX Bool) -- ^ Read/Write conflict for output B
  , cfWW  :: !(MaybeX Bool) -- ^ Write/Write conflict
  } deriving (Show)

-- | Determines whether there was a write-write or read-write conflict. A conflict
-- occurs when two ports tried to (potentially, in case of undefined values)
-- access the same address and one or both tried to write to it. See documentation
-- of 'Conflict' for more information.
getConflict ::
  -- | Port A: enable, write enable, address
  (MaybeX Bool, MaybeX Bool, MaybeX Int) ->
  -- | Port B: enable, write enable, address
  (MaybeX Bool, MaybeX Bool, MaybeX Int) ->
  -- | 'Just' if there is a (potential) write conflict, otherwise 'Nothing'
  Maybe Conflict
getConflict (enA, wenA, addrA) (enB, wenB, addrB)
  | IsDefined False <- sameAddrX = Nothing
  | otherwise                    = Just conflict
 where
  sameAddrX = liftA2 (==) addrA addrB

  conflict = Conflict
    { cfRWA = enA `andX` (enB `andX` wenB)
    , cfRWB = enB `andX` (enA `andX` wenA)
    , cfWW  = (enA `andX` enB) `andX` (wenA `andX` wenB)
    }

-- | Step through a cycle of a TDP block RAM where only one clock is active. Like
-- 'accessRam', it accounts for 'Clash.XException.XException' in all values
-- supplied by the user of the block RAM.
cycleOne ::
  forall nAddrs a writeEnable .
  ( HasCallStack
  , NFDataX a
  ) =>
  SNat nAddrs ->
  TdpbramModelConfig writeEnable a ->
  -- | Previous value
  a ->
  -- | Memory
  Seq a ->
  -- | Port: enable, address, write enable, write data
  (MaybeX Bool, MaybeX Int, MaybeX writeEnable, a) ->
  -- | Updated memory, output value
  (Seq a, a)
cycleOne SNat TdpbramModelConfig{..} prev ram0 = \case
  -- RAM is disabled, so we do nothing
  (IsDefined False, _, _, _) ->
    (ram0, prev)

  -- RAM is (potentially) enabled, so we run write RAM logic
  (ena, addr, byteEna0, dat) ->
    let
      byteEna1 = tdpMergeWriteEnable ena byteEna0
      (out0, !ram1) =
        accessRam (SNat @nAddrs) tdpIsActiveWriteEnable tdpUpdateRam addr byteEna1 dat ram0

      out1 = MaybeX.maybeX (throw . XException) (const out0) ena
    in
      (ram1, out1)

-- | Step through a cycle of a TDP block RAM where the clock edges of port A and
-- port B coincided. Like 'accessRam', it accounts for 'Clash.XException.XException'
-- in all values supplied by the user of the block RAM.
cycleBoth ::
  forall nAddrs a writeEnable.
  ( NFDataX a
  , HasCallStack
  ) =>
  SNat nAddrs ->
  TdpbramModelConfig writeEnable a ->
  -- | Previous value for port A
  a ->
  -- | Previous value for port B
  a ->
  -- | Memory
  Seq a ->
  -- | Port A: enable, address, write enable, write data
  (MaybeX Bool, MaybeX Int, MaybeX writeEnable, a) ->
  -- | Port B: enable, address, write enable, write data
  (MaybeX Bool, MaybeX Int, MaybeX writeEnable, a) ->
  -- | Updated memory, output value A, output value B
  (Seq a, a, a)
cycleBoth
  SNat TdpbramModelConfig{..} prevA prevB ram0
  (enAx, addrAx, byteEnaAx0, datA)
  (enBx, addrBx, byteEnaBx0, datB) = (ram2, outA2, outB2)
 where
  conflict =
    getConflict
      (enAx, tdpIsActiveWriteEnable byteEnaAx1, addrAx)
      (enBx, tdpIsActiveWriteEnable byteEnaBx1, addrBx)

  writeWriteError = deepErrorX "conflicting write/write queries"
  readWriteError = deepErrorX "conflicting read/write queries"

  byteEnaAx1 = tdpMergeWriteEnable enAx byteEnaAx0
  byteEnaBx1 = tdpMergeWriteEnable enBx byteEnaBx0

  (datA1, datB1) = case conflict of
    Just Conflict{cfWW=IsDefined True} -> (writeWriteError, writeWriteError)
    Just Conflict{cfWW=IsX _} -> (writeWriteError, writeWriteError)
    _ -> (datA, datB)

  (outA0, ram1) =
    accessRam (SNat @nAddrs) tdpIsActiveWriteEnable tdpUpdateRam addrAx byteEnaAx1 datA1 ram0
  (outB0, ram2) =
    accessRam (SNat @nAddrs) tdpIsActiveWriteEnable tdpUpdateRam addrBx byteEnaBx1 datB1 ram1

  outA1 = case conflict of
    Just Conflict{cfRWA=IsDefined True} -> readWriteError
    Just Conflict{cfRWA=IsX _} -> readWriteError
    _ -> outA0

  outB1 = case conflict of
    Just Conflict{cfRWB=IsDefined True} -> readWriteError
    Just Conflict{cfRWB=IsX _} -> readWriteError
    _ -> outB0

  outA2 = if MaybeX.fromMaybeX enAx then outA1 else prevA
  outB2 = if MaybeX.fromMaybeX enBx then outB1 else prevB

-- | Access a RAM and account for undefined values in the address, write enable,
-- and data to write. Return read after write value.
accessRam ::
  forall nAddrs a writeEnable .
  ( NFDataX a
  , HasCallStack ) =>
  SNat nAddrs ->
  -- | Determine whether a write enable is active
  (MaybeX writeEnable -> MaybeX Bool) ->
  -- | Update memory with a defined address
  (Int -> MaybeX writeEnable -> a -> Seq a -> Seq a) ->
  -- | Address
  MaybeX Int ->
  -- | Byte enable
  MaybeX writeEnable ->
  -- | Data to write
  a ->
  -- | Memory to write to
  Seq a ->
  -- | (Read after write value, new memory)
  (a, Seq a)
accessRam SNat tdpIsActiveWriteEnable updateMem addrX byteEnableX dat mem0
  -- Read (do nothing)
  | IsDefined False <- tdpIsActiveWriteEnable byteEnableX
  = (mem0 `Seq.index` MaybeX.fromMaybeX addrX, mem0)

  -- Undefined address and write enable or (partially) unknown
  | IsX addrMsg <- addrX
  = ( deepErrorX $ "Unknown address" <> "\nAddress error message: " <> addrMsg
    , Seq.fromFunction (natToNum @nAddrs) (unknownAddr addrMsg) )

  -- Write with defined address
  | IsDefined addr <- addrX
  , mem1 <- updateMem addr byteEnableX dat mem0
  = (mem1 `Seq.index` addr, mem1)
 where
  unknownAddr :: String -> Int -> a
  unknownAddr msg n =
    deepErrorX ("Write enabled or undefined, but address unknown; position " <> show n <>
                "\nAddress error message: " <> msg)

data TdpbramModelConfig writeEnable a = TdpbramModelConfig
  { tdpIsActiveWriteEnable :: MaybeX writeEnable -> MaybeX Bool
  -- ^ Determine whether a write enable is active

  , tdpMergeWriteEnable :: MaybeX Bool -> MaybeX writeEnable -> MaybeX writeEnable
  -- ^ Merge global enable with write enable

  , tdpUpdateRam :: Int -> MaybeX writeEnable -> a -> Seq a -> Seq a
  -- ^ Update memory with a defined address
  }

-- | Haskell model for a true dual-port block RAM which is polymorphic in its
-- write enables
--
tdpbramModel ::
  forall nAddrs domA domB a writeEnable .
  ( HasCallStack
  , KnownNat nAddrs
  , NFDataX a
  ) =>
  TdpbramModelConfig writeEnable a ->

  Clock domA ->
  -- | Enable
  Signal domA Bool ->
  -- | Address
  Signal domA (Index nAddrs) ->
  -- | Write enable
  Signal domA writeEnable ->
  -- | Write data
  Signal domA a ->

  Clock domB ->
  -- | Enable
  Signal domB Bool ->
  -- | Address
  Signal domB (Index nAddrs) ->
  -- | Write byte enable
  Signal domB writeEnable ->
  -- | Write data
  Signal domB a ->

  (Signal domA a, Signal domB a)
tdpbramModel
  config
  clkA enA addrA byteEnaA datA
  clkB enB addrB byteEnaB datB =
  ( startA :- outA
  , startB :- outB )
 where
  (outA, outB) =
    go
      (Seq.fromFunction (natToNum @nAddrs) initElement)
      (clockTicks clkA clkB)
      (bundle (enA, byteEnaA, fromIntegral <$> addrA, datA))
      (bundle (enB, byteEnaB, fromIntegral <$> addrB, datB))
      startA startB

  startA = deepErrorX $ "Port A: First value undefined"
  startB = deepErrorX $ "Port B: First value undefined"

  initElement :: Int -> a
  initElement n =
    deepErrorX ("Unknown initial element; position " <> show n)

  go ::
    Seq a ->
    [ClockAB] ->
    Signal domA (Bool, writeEnable, Int, a) ->
    Signal domB (Bool, writeEnable, Int, a) ->
    a -> a ->
    (Signal domA a, Signal domB a)
  go _ [] _ _ =
    error "tdpbramModel#.go: `ticks` should have been an infinite list"
  go ram0 (tick:ticks) as0 bs0 =
    case tick of
      ClockA -> goA
      ClockB -> goB
      ClockAB -> goBoth
   where
    (  toMaybeX -> enAx
     , toMaybeX -> byteEnaAx
     , toMaybeX -> addrAx
     , datA0
     ) :- as1 = as0

    (  toMaybeX -> enBx
     , toMaybeX -> byteEnaBx
     , toMaybeX -> addrBx
     , datB0
     ) :- bs1 = bs0

    portA = (enAx, addrAx, byteEnaAx, datA0)
    portB = (enBx, addrBx, byteEnaBx, datB0)

    goBoth prevA prevB = outA1 `seqX` outB1 `seqX` (outA1 :- as2, outB1 :- bs2)
     where
      (ram1, outA1, outB1) =
        cycleBoth
          (SNat @nAddrs) config
          prevA prevB ram0 portA portB
      (as2, bs2) = go ram1 ticks as1 bs1 outA1 outB1

    goA prevA prevB = out `seqX` (out :- as2, bs2)
     where
      (ram1, out) = cycleOne (SNat @nAddrs) config prevA ram0 portA
      (as2, bs2) = go ram1 ticks as1 bs0 out prevB

    goB prevA prevB = out `seqX` (as2, out :- bs2)
     where
      (ram1, out) = cycleOne (SNat @nAddrs) config prevB ram0 portB
      (as2, bs2) = go ram1 ticks as0 bs1 prevA out
