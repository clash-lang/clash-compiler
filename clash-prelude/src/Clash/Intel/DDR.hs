{-|
Copyright  :  (C) 2017, Google Inc
                  2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

DDR primitives for Intel FPGAs using ALTDDIO primitives.

For general information about DDR primitives see "Clash.Explicit.DDR".

Note that a reset is only available on certain devices,
see the ALTDDIO user guide for the specifics:
<https://www.altera.com/content/dam/altera-www/global/en_US/pdfs/literature/ug/ug_altddio.pdf>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Intel.DDR
  ( altddioIn
  , altddioOut
    -- * Internal
  , altddioIn#
  , altddioOut#
  )
where

import Data.Bifunctor
import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Explicit.Prelude
import Clash.Explicit.DDR

-- | Intel specific variant of 'ddrIn' implemented using the ALTDDIO_IN IP core.
--
-- Reset values are @0@
--
-- Of the output pair @(o0, o1)@, @o0@ is the data clocked in on the /falling/
-- edge and @o1@ is the data clocked in on the /rising/ edge, and @o0@ comes
-- before @o1@ in time.
--
-- __NB__: This primitive only supports rising edges as the active edge.
altddioIn
  :: forall deviceFamily a dom domDDR
   . HasCallStack
  => KnownDomain dom
  => KnownDomain domDDR
  => DomainPeriod dom ~ (2 * DomainPeriod domDDR)
  => DomainActiveEdge dom ~ 'Rising
  => BitPack a
  => SSymbol deviceFamily
  -- ^ The FPGA family
  --
  -- For example this can be instantiated as follows:
  --
  -- > SSymbol @"Cyclone IV GX"
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal domDDR a
  -- ^ DDR input signal
  -> Signal dom (a, a)
  -- ^ Normal speed output pair @(o0, o1)@
altddioIn devFam clk rst en =
  fmap (bimap unpack unpack) .
    withFrozenCallStack (altddioIn# devFam clk rst en) . fmap pack

altddioIn#
  :: forall deviceFamily n dom domDDR
   . HasCallStack
  => KnownDomain dom
  => KnownDomain domDDR
  => DomainPeriod dom ~ (2 * DomainPeriod domDDR)
  => DomainActiveEdge dom ~ 'Rising
  => KnownNat n
  => SSymbol deviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal domDDR (BitVector n)
  -> Signal dom (BitVector n, BitVector n)
altddioIn# SSymbol clk rst en = withFrozenCallStack ddrIn# clk rst en 0 0 0
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE altddioIn# #-}
{-# ANN altddioIn# hasBlackBox #-}

-- | Intel specific variant of 'ddrOut' implemented using the ALTDDIO_OUT IP core.
--
-- Reset value is @0@
--
-- Of the input pair @(i0, i1)@, @i0@ is the data clocked out on the /rising/
-- edge and @i1@ is the data clocked out on the /falling/ edge, and @i0@ comes
-- before @i1@ in time.
--
-- __NB__: This primitive only supports rising edges as the active edge.
altddioOut
  :: forall deviceFamily a dom domDDR
   . HasCallStack
  => KnownDomain dom
  => KnownDomain domDDR
  => DomainPeriod dom ~ (2 * DomainPeriod domDDR)
  => DomainActiveEdge dom ~ 'Rising
  => BitPack a
  => SSymbol deviceFamily
  -- ^ The FPGA family
  --
  -- For example this can be instantiated as follows:
  --
  -- > SSymbol @"Cyclone IV E"
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom (a, a)
  -- ^ Normal speed input pair @(i0, i1)@
  -> Signal domDDR a
  -- ^ DDR output signal
altddioOut devFam clk rst en =
  fmap unpack . uncurry (withFrozenCallStack altddioOut# devFam clk rst en) .
    unbundle . fmap (bimap pack pack)

altddioOut#
  :: forall deviceFamily n dom domDDR
   . HasCallStack
  => KnownDomain dom
  => KnownDomain domDDR
  => DomainPeriod dom ~ (2 * DomainPeriod domDDR)
  => DomainActiveEdge dom ~ 'Rising
  => KnownNat n
  => SSymbol deviceFamily
  -> Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom (BitVector n)
  -> Signal dom (BitVector n)
  -> Signal domDDR (BitVector n)
altddioOut# SSymbol clk rst en = ddrOut# clk rst en 0
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE altddioOut# #-}
{-# ANN altddioOut# hasBlackBox #-}
