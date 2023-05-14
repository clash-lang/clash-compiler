{-|
  Copyright   :  (C) 2023 QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Internal definitions for "Clash.Cores.Xilinx.BlockRam"
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK hide #-}

module Clash.Cores.Xilinx.BlockRam.Internal where

import Clash.Explicit.Prelude hiding (enable)
import Clash.Annotations.Primitive (Primitive(InlineYamlPrimitive), hasBlackBox)
import Clash.Explicit.BlockRam.Model (TdpbramModelConfig(..), tdpbramModel)
import Clash.Sized.Internal.BitVector (BitVector(BV), undefined#)
import Clash.XException.MaybeX (MaybeX(..))

import Data.String.Interpolate (__i)
import GHC.Stack (HasCallStack)

import Clash.Cores.Xilinx.BlockRam.BlackBoxes (tdpbramTclTF, tdpbramTF)

import qualified Data.Sequence as Seq

-- $setup
-- >>> import Clash.Explicit.Prelude


-- | Returns a byte enable such that it agrees with the global enable of a
-- block RAM. I.e., a deasserted global enable will return a deasserted byte
-- enable.
--
-- >>> mergeEnableByteEnable @3 (pure undefined) (pure $(bLit "01."))
-- IsDefined 0b0..
-- >>> mergeEnableByteEnable @3 (pure False) (pure $(bLit "01."))
-- IsDefined 0b000
-- >>> mergeEnableByteEnable @3 (pure True) (pure $(bLit "01."))
-- IsDefined 0b01.
-- >>> mergeEnableByteEnable @3 (pure True) (pure undefined)
-- IsX ...
--
mergeEnableByteEnable ::
  KnownNat n =>
  -- | Global enable
  MaybeX Bool ->
  -- | Byte enable
  MaybeX (BitVector n) ->
  -- | Byte enable combined with global enable
  MaybeX (BitVector n)
mergeEnableByteEnable ena byteEna =
  case ena of
    IsX _           -> liftA2 (.&.) (pure undefined#) byteEna
    IsDefined True  -> byteEna
    IsDefined False -> pure 0

-- | Clone each bit in a mask 8 times
--
-- >>> byteMaskToBitMask (0b01 :: BitVector 2)
-- 0b0000_0000_1111_1111
--
byteMaskToBitMask :: KnownNat n => BitVector n -> BitVector (8 * n)
byteMaskToBitMask = pack . map go . unpack
 where
  go :: Bool -> BitVector 8
  go True = maxBound
  go False = 0

-- | Determine whether a write enable is (potentially) active
isActiveWriteEnable :: KnownNat nBytes => MaybeX (BitVector nBytes) -> MaybeX Bool
isActiveWriteEnable = fmap (/= 0)

-- | Update a true dual-port block RAM with byte enables
updateRam ::
  ( KnownNat nBytes
  , BitSize a ~ (8 * nBytes)
  , BitPack a
  , NFDataX a ) =>
  Int ->
  MaybeX (BitVector nBytes) ->
  a ->
  Seq.Seq a ->
  Seq.Seq a
updateRam addr (IsX _) dat mem =
  -- Note that 'undefined#' has a defined spine, i.e., is not X. The recursive
  -- call will therefore follow the path applying 'goAdjust'.
  updateRam addr (pure undefined#) dat mem
updateRam addr (IsDefined byteEna) dat mem
  | isDefinedMaxBound byteEna
  = Seq.update addr dat mem
  | otherwise
  = let
    goAdjust oldDat =
      let
        datBv0 = pack dat
        oldDatBv0 = pack oldDat
        bitEnable = byteMaskToBitMask byteEna
        -- Skip any bits that are the same in old and new: by handling this
        -- explicitly, we avoid these bits becoming undefined unnecessarily.
        -- .&. and .|. will take care of any undefined bits in 'byteEna'.
        bitEnableMasked = bitEnable .&. (oldDatBv0 `xor` datBv0)
        datBv1 = datBv0 .&. bitEnableMasked
        oldDatBv1 = oldDatBv0 .&. complement bitEnableMasked
        newDatBv = datBv1 .|. oldDatBv1
      in
        unpack newDatBv
  in
    Seq.adjust goAdjust addr mem
 where
  isDefinedMaxBound :: forall n. KnownNat n => BitVector n -> Bool
  isDefinedMaxBound (BV 0 n) = n == (1 `shiftL` natToNum @n) - 1
  isDefinedMaxBound _        = False

{-# ANN tdpbram# hasBlackBox #-}
{-# NOINLINE tdpbram# #-}
{-# ANN tdpbram# (
   let
     primName = 'tdpbram#
     tfName = 'tdpbramTF
     tclTfName = 'tdpbramTclTF
   in InlineYamlPrimitive [minBound..] [__i|
        BlackBox:
            name: #{primName}
            kind: Declaration
            format: Haskell
            templateFunction: #{tfName}
            includes:
              - name: tdpbram
                extension: clash.tcl
                format: Haskell
                templateFunction: #{tclTfName}
            workInfo: Always
        |]) #-}
-- | Primitive for @tdpbram@
--
tdpbram# ::
  forall nAddrs domA domB nBytes a.
  ( HasCallStack
  , DomainActiveEdge domA ~ 'Rising
  , DomainActiveEdge domB ~ 'Rising
  , KnownNat nAddrs
  , KnownDomain domA
  , KnownDomain domB
  , NFDataX a
  , BitPack a
  , KnownNat nBytes
  , BitSize a ~ (8 * nBytes)
  ) =>
  Clock domA ->
  -- | Enable
  Signal domA Bool ->
  -- | Address
  Signal domA (Index nAddrs) ->
  -- | Write byte enable
  Signal domA (BitVector nBytes) ->
  -- | Write data
  Signal domA a ->

  Clock domB ->
  -- | Enable
  Signal domB Bool ->
  -- | Address
  Signal domB (Index nAddrs) ->
  -- | Write byte enable
  Signal domB (BitVector nBytes) ->
  -- | Write data
  Signal domB a ->

  ( Signal domA a
  , Signal domB a
  )
tdpbram# !clkA enA addrA byteEnaA datA !clkB enB addrB byteEnaB datB =
  tdpbramModel
    TdpbramModelConfig
      { tdpMergeWriteEnable = mergeEnableByteEnable
      , tdpUpdateRam = updateRam
      , tdpIsActiveWriteEnable = isActiveWriteEnable
      }
    clkA enA addrA byteEnaA datA
    clkB enB addrB byteEnaB datB
