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
import Clash.Sized.Internal.Mod (complementMod)
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
  forall nBytes a .
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
        -- Adjust memory data accounting for byte enables. Each bit in a memory
        -- word is treated separately; the byte-enable is expanded to enable
        -- lines for individual bits. Two cases are self-evident:
        --   - Bit-enable deasserted: bit in memory unchanged
        --   - Bit-enable asserted: bit in memory receives new value
        --
        -- When the bit-enable is undefined however, the following holds:
        -- Given @oldDat@ the original contents, @dat@ the data input and
        -- @newDat@ the new memory contents, the truth table is as follows:
        --
        --   oldDat   dat   newDat
        --     0       0      0
        --     0       1      X
        --     0       X      X
        --     1       0      X
        --     1       1      1
        --     1       X      X
        --     X       0      X
        --     X       1      X
        --     X       X      X
        --
        -- In words, if the new data is the same as the old data, the bit in
        -- memory is retained; if this is not certain, it becomes X.
        BV datMask datVal = pack dat
        BV oldMask oldVal = pack oldDat
        BV enaMask enaVal = byteMaskToBitMask byteEna
        complementN = complementMod (natToNum @(BitSize a))
        enaValInv = complementN enaVal
        sameValInv =
          -- Bit is 0 when @old@ and @dat@ are the same defined value, 1
          -- otherwise
          (oldVal `xor` datVal) .|. oldMask .|. datMask
        newMask =
              -- Not enabled, old value undefined
              (oldMask .&. enaValInv)
          .|. -- Enabled, new value undefined
              (datMask .&. enaVal)
          .|. -- Enable undefined, @old@ and @dat@ not the same defined value
              (sameValInv .&. enaMask)
        newVal =     (    -- Not enabled: old value
                          (oldVal .&. enaValInv)
                      .|. -- Enabled: new value
                          (datVal .&. enaVal)
                     )
                 .&. -- Filter out undefined
                     complementN newMask

      in
        unpack $ BV newMask newVal
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
