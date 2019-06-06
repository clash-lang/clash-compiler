{-|
Copyright  :  (C) 2015-2016, University of Twente,
                  2017     , Myrtle Software Ltd, Google Inc.
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

RAM primitives with a combinational read port.
-}

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS_HADDOCK show-extensions #-}

module Clash.Prelude.RAM
  ( -- * RAM synchronized to an arbitrary clock
    asyncRam
  , asyncRamPow2
  )
where

import           GHC.TypeLits         (KnownNat)
import           GHC.Stack            (HasCallStack, withFrozenCallStack)

import qualified Clash.Explicit.RAM   as E
import           Clash.Promoted.Nat   (SNat)
import           Clash.Signal
import           Clash.Sized.Unsigned (Unsigned)


-- | Create a RAM with space for @n@ elements.
--
-- * __NB__: Initial content of the RAM is 'undefined'
--
-- Additional helpful information:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- RAM.
asyncRam
  :: (Enum addr, HiddenClock domain gated, HasCallStack)
  => SNat n
  -- ^ Size @n@ of the RAM
  -> Signal domain addr
  -- ^ Read address @r@
  -> Signal domain (Maybe (addr, a))
   -- ^ (write address @w@, value to write)
  -> Signal domain a
   -- ^ Value of the @RAM@ at address @r@
asyncRam = \sz rd wrM -> withFrozenCallStack
  (hideClock (\clk -> E.asyncRam clk clk sz rd wrM))
{-# INLINE asyncRam #-}

-- | Create a RAM with space for 2^@n@ elements
--
-- * __NB__: Initial content of the RAM is 'undefined'
--
-- Additional helpful information:
--
-- * See "Clash.Prelude.BlockRam#usingrams" for more information on how to use a
-- RAM.
asyncRamPow2
  :: (KnownNat n, HiddenClock domain gated, HasCallStack)
  => Signal domain (Unsigned n)
  -- ^ Read address @r@
  -> Signal domain (Maybe (Unsigned n, a))
  -- ^ (write address @w@, value to write)
  -> Signal domain a
  -- ^ Value of the @RAM@ at address @r@
asyncRamPow2 = \rd wrM -> withFrozenCallStack
  (hideClock (\clk -> E.asyncRamPow2 clk clk rd wrM))
{-# INLINE asyncRamPow2 #-}
