{-|
Copyright  :  (C) 2017, Google Inc
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

DDR primitives for Xilinx FPGAs

For general information about DDR primitives see "Clash.Explicit.DDR".

For more information about the Xilinx DDR primitives see:

* Vivado Design Suite 7 Series FPGA and Zynq-7000 All Programmable SoC
  Libraries Guide, UG953 (v2018.3) December 5, 2018, p371-373,p481-483,
  <https://www.xilinx.com/support/documentation/sw_manuals/xilinx2018_3/ug953-vivado-7series-libraries.pdf>
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
iddr
  :: ( HasCallStack
     , KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity)
     , KnownNat m )
  => Clock slow
  -- ^ clock
  -> Reset slow
  -- ^ reset
  -> Enable slow
  -- ^ global enable
  -> Signal fast (BitVector m)
  -- ^ DDR input signal
  -> Signal slow ((BitVector m),(BitVector m))
  -- ^ normal speed output pairs
iddr clk rst en = withFrozenCallStack ddrIn# clk rst en 0 0 0
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE iddr #-}
{-# ANN iddr hasBlackBox #-}

-- | Xilinx specific variant of 'ddrOut' implemented using the Xilinx ODDR
-- primitive in @SAME_EDGE@ mode.
--
-- Reset value is @0@
oddr
  :: ( KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity)
     , KnownNat m )
  => Clock slow
  -- ^ clock
  -> Reset slow
  -- ^ reset
  -> Enable slow
  -- ^ global enable
  -> Signal slow (BitVector m, BitVector m)
  -- ^ normal speed input pairs
  -> Signal fast (BitVector m)
  -- ^ DDR output signal
oddr clk rst en = uncurry (withFrozenCallStack oddr# clk rst en) . unbundle

oddr#
  :: ( KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity)
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
