{-|
Copyright  :  (C) 2021-2022, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

= Efficient bundling of ROM content with the compiled code

Leveraging Template Haskell, the content for the ROM components in this module
is stored alongside the compiled Haskell code. It covers use cases where passing
the initial content as a 'Clash.Sized.Vector.Vec' turns out to be
problematically slow.

The data is stored efficiently, with very little overhead (worst-case 7%, often
no overhead at all).

Unlike "Clash.Explicit.ROM.File", "Clash.Explicit.ROM.Blob" generates
practically the same HDL as "Clash.Explicit.ROM" and is compatible with all
tools consuming the generated HDL.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Explicit.ROM.Blob
  ( -- * ROMs defined by a 'MemBlob'
    romBlob
  , romBlobPow2
    -- * Creating and inspecting 'MemBlob'
  , MemBlob
  , createMemBlob
  , memBlobTH
  , unpackMemBlob
    -- * Internal
  , romBlob#
  ) where

import Data.Array (listArray)
import Data.Array.Base (unsafeAt)
import GHC.Stack (withFrozenCallStack)
import GHC.TypeLits (KnownNat, type (^))

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Explicit.BlockRam.Blob (createMemBlob, memBlobTH)
import Clash.Explicit.BlockRam.Internal (MemBlob(..), unpackMemBlob)
import Clash.Promoted.Nat (natToNum)
import Clash.Signal.Internal
  (Clock (..), KnownDomain, Signal (..), Enable, fromEnable)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Unsigned (Unsigned)
import Clash.XException (deepErrorX, seqX)

-- | A ROM with a synchronous read port, with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
-- === See also:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and
-- "Clash.Explicit.BlockRam#usingrams" for ideas on how to use ROMs and RAMs.
romBlob
  :: forall dom addr m n
   . ( KnownDomain dom
     , Enum addr
     )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> MemBlob n m
  -- ^ ROM content, also determines the size, @n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
romBlob = \clk en content rd -> romBlob# clk en content (fromEnum <$> rd)
{-# INLINE romBlob #-}

-- | A ROM with a synchronous read port, with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
-- === See also:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and
-- "Clash.Explicit.BlockRam#usingrams" for ideas on how to use ROMs and RAMs.
romBlobPow2
  :: forall dom m n
   . ( KnownDomain dom
     , KnownNat n
     )
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> MemBlob (2^n) m
  -- ^ ROM content, also determines the size, 2^@n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom (Unsigned n)
  -- ^ Read address @r@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
romBlobPow2 = romBlob
{-# INLINE romBlobPow2 #-}

-- | ROM primitive
romBlob#
  :: forall dom m n
   . KnownDomain dom
  => Clock dom
  -- ^ 'Clock' to synchronize to
  -> Enable dom
  -- ^ 'Enable' line
  -> MemBlob n m
  -- ^ ROM content, also determines the size, @n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom Int
  -- ^ Read address @r@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
romBlob# !_ en content@MemBlob{} =
  go
    (withFrozenCallStack (deepErrorX "romBlob: initial value undefined"))
    (fromEnable en)
 where
  szI = natToNum @n @Int
  arr = listArray (0,szI-1) $ unpackMemBlob content

  go o (e :- es) rd@(~(r :- rs)) =
    let o1 = if e then safeAt r else o
    -- See [Note: register strictness annotations]
    in  o `seqX` o :- (rd `seq` go o1 es rs)

  safeAt :: Int -> BitVector m
  safeAt i =
    if (0 <= i) && (i < szI) then
      unsafeAt arr i
    else
      withFrozenCallStack
        (deepErrorX ("romBlob: address " ++ show i ++
                     " not in range [0.." ++ show szI ++ ")"))
  {-# INLINE safeAt #-}
{-# NOINLINE romBlob# #-}
{-# ANN romBlob# hasBlackBox #-}
