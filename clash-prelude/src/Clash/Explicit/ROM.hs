{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_HADDOCK show-extensions #-}

{- |
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Google Inc.
                  2019     , Myrtle Software Ltd,
                  2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

ROMs
-}
module Clash.Explicit.ROM (
  -- * Synchronous ROM synchronized to an arbitrary clock
  rom,
  romPow2,

  -- * Internal
  rom#,
)
where

import Data.Array (listArray)
import Data.Array.Base (unsafeAt)
import GHC.Stack (withFrozenCallStack)
import GHC.TypeLits (KnownNat, type (^))
import Prelude hiding (length)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Signal.Internal (
  Clock (..),
  Enable,
  KnownDomain,
  Signal (..),
  fromEnable,
 )
import Clash.Sized.Unsigned (Unsigned)
import Clash.Sized.Vector (Vec, length, toList)
import Clash.XException (NFDataX, deepErrorX, seqX)

{- | A ROM with a synchronous read port, with space for 2^@n@ elements

* __NB__: Read value is delayed by 1 cycle
* __NB__: Initial output value is /undefined/, reading it will throw an
'Clash.XException.XException'

=== See also:

* See "Clash.Sized.Fixed#creatingdatafiles" and "Clash.Explicit.BlockRam#usingrams"
for ideas on how to use ROMs and RAMs.
* A large 'Vec' for the content may be too inefficient, depending on how it
is constructed. See 'Clash.Explicit.ROM.File.romFilePow2' and
'Clash.Explicit.ROM.Blob.romBlobPow2' for different approaches that scale
well.
-}
romPow2 ::
  (KnownDomain dom, KnownNat n, NFDataX a) =>
  -- | 'Clock' to synchronize to
  Clock dom ->
  -- | 'Enable' line
  Enable dom ->
  {- | ROM content

  __NB__: __MUST__ be a constant
  -}
  Vec (2 ^ n) a ->
  -- | Read address @r@
  Signal dom (Unsigned n) ->
  -- | The value of the ROM at address @r@ from the previous clock cycle
  Signal dom a
romPow2 = rom
{-# INLINE romPow2 #-}

{- | A ROM with a synchronous read port, with space for @n@ elements

* __NB__: Read value is delayed by 1 cycle
* __NB__: Initial output value is /undefined/, reading it will throw an
'Clash.XException.XException'

=== See also:

* See "Clash.Sized.Fixed#creatingdatafiles" and "Clash.Explicit.BlockRam#usingrams"
for ideas on how to use ROMs and RAMs.
* A large 'Vec' for the content may be too inefficient, depending on how it
is constructed. See 'Clash.Explicit.ROM.File.romFile' and
'Clash.Explicit.ROM.Blob.romBlob' for different approaches that scale well.
-}
rom ::
  (KnownDomain dom, KnownNat n, NFDataX a, Enum addr) =>
  -- | 'Clock' to synchronize to
  Clock dom ->
  -- | 'Enable' line
  Enable dom ->
  {- | ROM content, also determines the size, @n@, of the ROM

  __NB__: __MUST__ be a constant
  -}
  Vec n a ->
  -- | Read address @r@
  Signal dom addr ->
  -- | The value of the ROM at address @r@ from the previous clock cycle
  Signal dom a
rom = \clk en content rd -> rom# clk en content (fromEnum <$> rd)
{-# INLINE rom #-}

-- | ROM primitive
rom# ::
  forall dom n a.
  (KnownDomain dom, KnownNat n, NFDataX a) =>
  -- | 'Clock' to synchronize to
  Clock dom ->
  -- | 'Enable' line
  Enable dom ->
  {- | ROM content, also determines the size, @n@, of the ROM

  __NB__: __MUST__ be a constant
  -}
  Vec n a ->
  -- | Read address @rd@
  Signal dom Int ->
  -- | The value of the ROM at address @rd@ from the previous clock cycle
  Signal dom a
rom# !_ en content =
  go
    (withFrozenCallStack (deepErrorX "rom: initial value undefined"))
    (fromEnable en)
 where
  szI = length content
  arr = listArray (0, szI - 1) (toList content)

  go o (e :- es) rd@(~(r :- rs)) =
    let o1 = if e then safeAt r else o
     in -- See [Note: register strictness annotations]
        o `seqX` o :- (rd `seq` go o1 es rs)

  safeAt :: Int -> a
  safeAt i =
    if (0 <= i) && (i < szI)
      then
        unsafeAt arr i
      else
        withFrozenCallStack
          ( deepErrorX
              ( "rom: address "
                  ++ show i
                  ++ " not in range [0.."
                  ++ show szI
                  ++ ")"
              )
          )
  {-# INLINE safeAt #-}
{-# OPAQUE rom# #-}
{-# ANN rom# hasBlackBox #-}
