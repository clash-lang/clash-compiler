{-|
Copyright  :  (C) 2017, Google Inc
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

DDR primitives for Xilinx FPGAs

For general information about DDR primitives see "Clash.Explicit.DDR".

For more information about the Xilinx DDR primitives see:

* Vivado Design Suite 7 Series FPGA and Zynq-7000 All Programmable SoC
  Libraries Guide, UG953 (v2022.2) October 19, 2022, p369-371, p477-479,
  <https://www.xilinx.com/content/dam/xilinx/support/documents/sw_manuals/xilinx2022_2/ug953-vivado-7series-libraries.pdf>
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Xilinx.DDR
  ( iddr
  , oddr
  )
where

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Explicit.Prelude
import Clash.Explicit.DDR

-- | Xilinx specific variant of 'ddrIn' implemented using the Xilinx IDDR
-- primitive in @SAME_EDGE@ mode.
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
iddr
  :: forall fast fPeriod edge reset init polarity slow m
   . ( HasCallStack
     , KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity)
     , KnownNat m )
  => Clock slow
  -> Reset slow
  -> Enable slow
  -> Signal fast (BitVector m)
  -- ^ DDR input signal
  -> Signal slow ((BitVector m),(BitVector m))
  -- ^ Normal speed output pair @(o0, o1)@
iddr =
  case activeEdge @slow of
    SRising ->
      withFrozenCallStack iddr#
    SFalling ->
      clashCompileError
        "iddr: Primitive only supports rising active edge"

iddr#
  :: ( HasCallStack
     , KnownConfiguration fast ('DomainConfiguration fast fPeriod 'Rising reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) 'Rising reset init polarity)
     , KnownNat m )
  => Clock slow
  -> Reset slow
  -> Enable slow
  -> Signal fast (BitVector m)
  -> Signal slow ((BitVector m),(BitVector m))
iddr# clk rst en = withFrozenCallStack ddrIn# clk rst en 0 0 0
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE iddr# #-}
{-# ANN iddr# hasBlackBox #-}

-- | Xilinx specific variant of 'ddrOut' implemented using the Xilinx ODDR
-- primitive in @SAME_EDGE@ mode.
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
oddr
  :: forall fast fPeriod edge reset init polarity slow m
   . ( KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity)
     , KnownNat m )
  => Clock slow
  -> Reset slow
  -> Enable slow
  -> Signal slow (BitVector m, BitVector m)
  -- ^ Normal speed input pair @(i0, i1)@
  -> Signal fast (BitVector m)
  -- ^ DDR output signal
oddr clk rst en =
  case activeEdge @slow of
    SRising ->
       uncurry (withFrozenCallStack oddr# clk rst en) . unbundle
    SFalling ->
      clashCompileError
        "oddr: Primitive only supports rising active edge"

oddr#
  :: ( KnownConfiguration fast ('DomainConfiguration fast fPeriod 'Rising reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) 'Rising reset init polarity)
     , KnownNat m )
  => Clock slow
  -> Reset slow
  -> Enable slow
  -> Signal slow (BitVector m)
  -> Signal slow (BitVector m)
  -> Signal fast (BitVector m)
oddr# clk rst en = ddrOut# clk rst en 0
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE oddr# #-}
{-# ANN oddr# hasBlackBox #-}
