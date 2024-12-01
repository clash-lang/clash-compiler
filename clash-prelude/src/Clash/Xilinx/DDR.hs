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
    -- * Internal
  , iddr#
  , oddr#
  )
where

import Data.Bifunctor
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
-- __NB__: This primitive only supports rising edges as the active edge.
iddr
  :: forall a dom domDDR
   . HasCallStack
  => KnownDomain dom
  => KnownDomain domDDR
  => DomainPeriod dom ~ (2 * DomainPeriod domDDR)
  => DomainActiveEdge dom ~ 'Rising
  => BitPack a
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal domDDR a
  -- ^ DDR input signal
  -> Signal dom (a, a)
  -- ^ Normal speed output pair @(o0, o1)@
iddr clk rst en =
  fmap (bimap unpack unpack) . withFrozenCallStack (iddr# clk rst en) .
    fmap pack

iddr#
  :: forall n dom domDDR
   . HasCallStack
  => KnownDomain dom
  => KnownDomain domDDR
  => DomainPeriod dom ~ (2 * DomainPeriod domDDR)
  => DomainActiveEdge dom ~ 'Rising
  => KnownNat n
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal domDDR (BitVector n)
  -> Signal dom (BitVector n, BitVector n)
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
-- __NB__: This primitive only supports rising edges as the active edge.
oddr
  :: forall a dom domDDR
   . HasCallStack
  => KnownDomain dom
  => KnownDomain domDDR
  => DomainPeriod dom ~ (2 * DomainPeriod domDDR)
  => DomainActiveEdge dom ~ 'Rising
  => BitPack a
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom (a, a)
  -- ^ Normal speed input pair @(i0, i1)@
  -> Signal domDDR a
  -- ^ DDR output signal
oddr clk rst en =
  fmap unpack . uncurry (withFrozenCallStack oddr# clk rst en) . unbundle .
    fmap (bimap pack pack)

oddr#
  :: forall n dom domDDR
   . HasCallStack
  => KnownDomain dom
  => KnownDomain domDDR
  => DomainPeriod dom ~ (2 * DomainPeriod domDDR)
  => DomainActiveEdge dom ~ 'Rising
  => KnownNat n
  => Clock dom
  -> Reset dom
  -> Enable dom
  -> Signal dom (BitVector n)
  -> Signal dom (BitVector n)
  -> Signal domDDR (BitVector n)
oddr# clk rst en = ddrOut# clk rst en 0
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE oddr# #-}
{-# ANN oddr# hasBlackBox #-}
