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
  )
where

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
-- __NB__: This primitive only supports rising edges as the active edge. Trying
-- to instantiate this function in a domain where falling edges are the active
-- edge will lead to a HDL generation or Haskell simulation error.
altddioIn
  :: forall fast fPeriod edge reset init polarity slow m deviceFamily
   . ( HasCallStack
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
  -> Reset slow
  -> Enable slow
  -> Signal fast (BitVector m)
  -- ^ DDR input signal
  -> Signal slow (BitVector m,BitVector m)
  -- ^ Normal speed output pair @(o0, o1)@
altddioIn =
  case activeEdge @slow of
    SRising ->
      withFrozenCallStack altddioIn#
    SFalling ->
      clashCompileError
        "altddioIn: Primitive only supports rising active edge"

altddioIn#
  :: ( HasCallStack
     , KnownConfiguration fast ('DomainConfiguration fast fPeriod 'Rising reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) 'Rising reset init polarity)
     , KnownNat m )
  => SSymbol deviceFamily
  -> Clock slow
  -> Reset slow
  -> Enable slow
  -> Signal fast (BitVector m)
  -> Signal slow (BitVector m,BitVector m)
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
-- __NB__: This primitive only supports rising edges as the active edge. Trying
-- to instantiate this function in a domain where falling edges are the active
-- edge will lead to a HDL generation or Haskell simulation error.
altddioOut
  :: forall fast fPeriod edge reset init polarity slow m deviceFamily
   . ( HasCallStack
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
  -> Reset slow
  -> Enable slow
  -> Signal slow (BitVector m,BitVector m)
  -- ^ Normal speed input pair @(i0, i1)@
  -> Signal fast (BitVector m)
  -- ^ DDR output signal
altddioOut devFam clk rst en =
  case activeEdge @slow of
    SRising ->
      uncurry (withFrozenCallStack altddioOut# devFam clk rst en) . unbundle
    SFalling ->
      clashCompileError
        "altddioOut: Primitive only supports rising active edge"

altddioOut#
  :: ( HasCallStack
     , KnownConfiguration fast ('DomainConfiguration fast fPeriod 'Rising reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) 'Rising reset init polarity)
     , KnownNat m )
  => SSymbol deviceFamily
  -> Clock slow
  -> Reset slow
  -> Enable slow
  -> Signal slow (BitVector m)
  -> Signal slow (BitVector m)
  -> Signal fast (BitVector m)
altddioOut# SSymbol clk rst en = ddrOut# clk rst en 0
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE altddioOut# #-}
{-# ANN altddioOut# hasBlackBox #-}
