{-|
Copyright  :  (C) 2022     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

= Efficient bundling of ROM content with the compiled code

Leveraging Template Haskell, the content for the ROM components in this module
is stored alongside the compiled Haskell code. It covers use cases where passing
the initial content as a 'Clash.Sized.Vector.Vec' turns out to be
problematically slow.

The data is stored efficiently, with very little overhead (worst-case 7%, often
no overhead at all).

Unlike "Clash.Prelude.ROM.File", "Clash.Prelude.ROM.Blob" generates practically
the same HDL as "Clash.Prelude.ROM" and is compatible with all tools consuming
the generated HDL.
-}

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Prelude.ROM.Blob
  ( -- * Asynchronous ROM defined by a 'MemBlob'
    asyncRomBlob
  , asyncRomBlobPow2
    -- * Synchronous 'MemBlob' ROM synchronized to an arbitrary clock
  , romBlob
  , romBlobPow2
    -- * Creating and inspecting 'MemBlob'
  , MemBlob
  , createMemBlob
  , memBlobTH
  , unpackMemBlob
    -- * Internal
  , asyncRomBlob#
  )
where

import Data.Array (listArray)
import Data.Array.Base (unsafeAt)
import GHC.Stack (withFrozenCallStack)
import GHC.TypeLits (KnownNat, type (^))

import Clash.Annotations.Primitive (hasBlackBox)
import qualified Clash.Explicit.ROM.Blob as E
import Clash.Explicit.BlockRam.Blob (createMemBlob, memBlobTH)
import Clash.Explicit.BlockRam.Internal (MemBlob(..), unpackMemBlob)
import Clash.Promoted.Nat (natToNum)
import Clash.Signal (hideClock, hideEnable, HiddenClock, HiddenEnable)
import Clash.Signal.Internal (Signal)
import Clash.Sized.Internal.BitVector (BitVector)
import Clash.Sized.Internal.Unsigned (Unsigned)
import Clash.XException (deepErrorX)

-- | An asynchronous/combinational ROM with space for @n@ elements
--
-- === See also:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and
-- "Clash.Prelude.BlockRam#usingrams" for ideas on how to use ROMs and RAMs.
asyncRomBlob
  :: Enum addr
  => MemBlob n m
  -- ^ ROM content, also determines the size, @n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> addr
  -- ^ Read address @r@
  -> BitVector m
  -- ^ The value of the ROM at address @r@
asyncRomBlob = \content rd -> asyncRomBlob# content (fromEnum rd)
{-# INLINE asyncRomBlob #-}

-- | An asynchronous/combinational ROM with space for 2^@n@ elements
--
-- === See also:
--
-- * See "Clash.Sized.Fixed#creatingdatafiles" and
-- "Clash.Prelude.BlockRam#usingrams" for ideas on how to use ROMs and RAMs.
asyncRomBlobPow2
  :: KnownNat n
  => MemBlob (2^n) m
  -- ^ ROM content, also determines the size, 2^@n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> Unsigned n
  -- ^ Read address @r@
  -> BitVector m
  -- ^ The value of the ROM at address @r@
asyncRomBlobPow2 = asyncRomBlob
{-# INLINE asyncRomBlobPow2 #-}

-- | asyncRomBlob primitive
asyncRomBlob#
  :: forall m n
   . MemBlob n m
  -- ^ ROM content, also determines the size, @n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> Int
  -- ^ Read address @r@
  -> BitVector m
  -- ^ The value of the ROM at address @r@
asyncRomBlob# content@MemBlob{} = safeAt
  where
    szI = natToNum @n @Int
    arr = listArray (0,szI-1) $ unpackMemBlob content

    safeAt :: Int -> BitVector m
    safeAt i =
      if (0 <= i) && (i < szI) then
        unsafeAt arr i
      else
        withFrozenCallStack
          (deepErrorX ("asyncRom: address " ++ show i ++
                       " not in range [0.." ++ show szI ++ ")"))
{-# ANN asyncRomBlob# hasBlackBox #-}
{-# NOINLINE asyncRomBlob# #-}

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
   . ( HiddenClock dom
     , HiddenEnable dom
     , Enum addr
     )
  => MemBlob n m
  -- ^ ROM content, also determines the size, @n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
romBlob = hideEnable (hideClock E.romBlob)
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
   . ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat n
     )
  => MemBlob (2^n) m
  -- ^ ROM content, also determines the size, 2^@n@, of the ROM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom (Unsigned n)
  -- ^ Read address @r@
  -> Signal dom (BitVector m)
  -- ^ The value of the ROM at address @r@ from the previous clock cycle
romBlobPow2 = hideEnable (hideClock E.romBlobPow2)
{-# INLINE romBlobPow2 #-}
