{-|
Copyright  :  (C) 2017, Google Inc
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

DDR primitives for Intel FPGAs using ALTDDIO primitives.

For general information about DDR primitives see "Clash.Explicit.DDR".

Note that a synchronous reset is only available on certain devices,
see ALTDDIO userguide for the specifics:
<https://www.altera.com/content/dam/altera-www/global/en_US/pdfs/literature/ug/ug_altddio.pdf>
-}

{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

module Clash.Intel.DDR
  ( altddioIn
  , altddioOut
  )
where

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Explicit.Prelude
import Clash.Explicit.DDR

-- | Intel specific variant of 'ddrIn' implemented using the ALTDDIO_IN IP core.
--
-- Reset values are @0@
altddioIn
  :: ( HasCallStack
     , fast ~ 'Dom n pFast
     , slow ~ 'Dom n (2*pFast)
     , KnownNat m )
  => SSymbol deviceFamily
  -- ^ The FPGA family
  --
  -- For example this can be instantiated as follows:
  --
  -- > SSymbol @"Cyclone IV GX"
  -> Clock slow gated
  -- ^ clock
  -> Reset slow synchronous
  -- ^ reset
  -> Signal fast (BitVector m)
  -- ^ DDR input signal
  -> Signal slow (BitVector m,BitVector m)
  -- ^ normal speed output pairs
altddioIn _devFam clk rst = withFrozenCallStack ddrIn# clk rst 0 0 0
{-# NOINLINE altddioIn #-}

-- | Intel specific variant of 'ddrOut' implemented using the ALTDDIO_OUT IP core.
--
-- Reset value is @0@
altddioOut
  :: ( HasCallStack
     , fast ~ 'Dom n pFast
     , slow ~ 'Dom n (2*pFast)
     , KnownNat m )
  => SSymbol deviceFamily
  -- ^ The FPGA family
  --
  -- For example this can be instantiated as follows:
  --
  -- > SSymbol @"Cyclone IV E"
  -> Clock slow gated
  -- ^ clock
  -> Reset slow synchronous
  -- ^ reset
  -> Signal slow (BitVector m,BitVector m)
  -- ^ normal speed input pair
  -> Signal fast (BitVector m)
  -- ^ DDR output signal
altddioOut devFam clk rst =
  uncurry (withFrozenCallStack altddioOut# devFam clk rst) . unbundle

altddioOut#
  :: ( HasCallStack
     , fast ~ 'Dom n pFast
     , slow ~ 'Dom n (2*pFast)
     , KnownNat m )
  => SSymbol deviceFamily
  -> Clock slow gated
  -> Reset slow synchronous
  -> Signal slow (BitVector m)
  -> Signal slow (BitVector m)
  -> Signal fast (BitVector m)
altddioOut# _ clk rst = ddrOut# clk rst 0
{-# NOINLINE altddioOut# #-}
