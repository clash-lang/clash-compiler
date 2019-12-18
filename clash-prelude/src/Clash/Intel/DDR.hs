{-|
Copyright  :  (C) 2017, Google Inc
                  2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

DDR primitives for Intel FPGAs using ALTDDIO primitives.

For general information about DDR primitives see "Clash.Explicit.DDR".

Note that a reset is only available on certain devices,
see ALTDDIO userguide for the specifics:
<https://www.altera.com/content/dam/altera-www/global/en_US/pdfs/literature/ug/ug_altddio.pdf>
-}

{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Clash.Intel.DDR
  ( altddioIn
  , altddioOut
  )
where

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Explicit.Prelude
import Clash.Explicit.DDR

-- | Intel specific variant of 'ddrIn' implemented using the ALTDDIO_IN IP core.
--
-- Reset values are @0@
altddioIn
  :: ( HasCallStack
     , KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity)
     , KnownNat m )
  => SSymbol deviceFamily
  -- ^ The FPGA family
  --
  -- For example this can be instantiated as follows:
  --
  -- > SSymbol @"Cyclone IV GX"
  -> Clock slow
  -- ^ clock
  -> Reset slow
  -- ^ reset
  -> Enable slow
  -- ^ Global enable
  -> Signal fast (BitVector m)
  -- ^ DDR input signal
  -> Signal slow (BitVector m,BitVector m)
  -- ^ normal speed output pairs
altddioIn _devFam clk rst en = withFrozenCallStack ddrIn# clk rst en 0 0 0
{-# NOINLINE altddioIn #-}
{-# ANN altddioIn hasBlackBox #-}

-- | Intel specific variant of 'ddrOut' implemented using the ALTDDIO_OUT IP core.
--
-- Reset value is @0@
altddioOut
  :: ( HasCallStack
     , KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity)
     , KnownNat m )
  => SSymbol deviceFamily
  -- ^ The FPGA family
  --
  -- For example this can be instantiated as follows:
  --
  -- > SSymbol @"Cyclone IV E"
  -> Clock slow
  -- ^ clock
  -> Reset slow
  -- ^ reset
  -> Enable slow
  -- ^ Global enable
  -> Signal slow (BitVector m,BitVector m)
  -- ^ normal speed input pair
  -> Signal fast (BitVector m)
  -- ^ DDR output signal
altddioOut devFam clk rst en =
  uncurry (withFrozenCallStack altddioOut# devFam clk rst en) . unbundle

altddioOut#
  :: ( HasCallStack
     , KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity)
     , KnownNat m )
  => SSymbol deviceFamily
  -> Clock slow
  -> Reset slow
  -> Enable slow
  -> Signal slow (BitVector m)
  -> Signal slow (BitVector m)
  -> Signal fast (BitVector m)
altddioOut# _ clk rst en = ddrOut# clk rst en 0
{-# NOINLINE altddioOut# #-}
{-# ANN altddioOut# hasBlackBox #-}
