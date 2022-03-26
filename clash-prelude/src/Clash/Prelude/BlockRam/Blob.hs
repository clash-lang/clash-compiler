{-|
Copyright  :  (C) 2022     , QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

= Efficient bundling of initial RAM content with the compiled code

Leveraging Template Haskell, the initial content for the block RAM components in
this module is stored alongside the compiled Haskell code. It covers use cases
where passing the initial content as a 'Clash.Sized.Vector.Vec' turns out to be
problematically slow.

The data is stored efficiently, with very little overhead (worst-case 7%, often
no overhead at all).

Unlike "Clash.Prelude.BlockRam.File", "Clash.Prelude.BlockRam.Blob" generates
practically the same HDL as "Clash.Prelude.BlockRam" and is compatible with all
tools consuming the generated HDL.
-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Prelude.BlockRam.Blob
  ( -- * BlockRAMs initialized with a 'E.MemBlob'
    blockRamBlob
  , blockRamBlobPow2
    -- * Creating and inspecting 'E.MemBlob'
  , E.MemBlob
  , E.createMemBlob
  , E.memBlobTH
  , E.unpackMemBlob
  )
where

import GHC.TypeLits (KnownNat, type (^))

import qualified Clash.Explicit.BlockRam.Blob as E
import Clash.Signal (hideClock, hideEnable, HiddenClock, HiddenEnable, Signal)
import Clash.Sized.BitVector (BitVector)
import Clash.Sized.Unsigned (Unsigned)

-- | Create a block RAM with space for @n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
--
-- === See also:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- block RAM.
-- * Use the adapter 'Clash.Prelude.BlockRam.readNew' for obtaining
-- write-before-read semantics like this: @'Clash.Prelude.BlockRam.readNew'
-- ('blockRamBlob' content) rd wrM@.
blockRamBlob
  :: forall dom addr m n
   . ( HiddenClock dom
     , HiddenEnable dom
     , Enum addr
     )
  => E.MemBlob n m
  -- ^ Initial content of the BRAM, also determines the size, @n@, of the BRAM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom addr
  -- ^ Read address @r@
  -> Signal dom (Maybe (addr, BitVector m))
  -- ^ (write address @w@, value to write)
  -> Signal dom (BitVector m)
  -- ^ Value of the BRAM at address @r@ from the previous clock cycle
blockRamBlob = hideEnable (hideClock E.blockRamBlob)
{-# INLINE blockRamBlob #-}

-- | Create a block RAM with space for 2^@n@ elements
--
-- * __NB__: Read value is delayed by 1 cycle
-- * __NB__: Initial output value is /undefined/, reading it will throw an
-- 'Clash.XException.XException'
--
-- === See also:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- block RAM.
-- * Use the adapter 'Clash.Prelude.BlockRam.readNew' for obtaining
-- write-before-read semantics like this: @'Clash.Prelude.BlockRam.readNew'
-- ('blockRamBlobPow2' content) rd wrM@.
blockRamBlobPow2
  :: forall dom m n
   . ( HiddenClock dom
     , HiddenEnable dom
     , KnownNat n
     )
  => E.MemBlob (2^n) m
  -- ^ Initial content of the BRAM, also determines the size, 2^@n@, of the BRAM
  --
  -- __NB__: __MUST__ be a constant
  -> Signal dom (Unsigned n)
  -- ^ Read address @r@
  -> Signal dom (Maybe (Unsigned n, BitVector m))
  -- ^ (write address @w@, value to write)
  -> Signal dom (BitVector m)
  -- ^ Value of the BRAM at address @r@ from the previous clock cycle
blockRamBlobPow2 = hideEnable (hideClock E.blockRamBlobPow2)
{-# INLINE blockRamBlobPow2 #-}
