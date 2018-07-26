{-|
Copyright  :  (C) 2017, Google Inc
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

DDR primitives for Xilinx FPGAs

For general information about DDR primitives see "Clash.Explicit.DDR".

For more information about the Xilinx DDR primitives see:
    * Vivado Design Suite 7 Series FPGA and Zynq-7000 All Programmable SoC
      Libraries Guide, UG953 (v2017.2) June 7, 2016, p294-296,p404-406,
      https://www.xilinx.com/support/documentation/sw_manuals/xilinx2017_2/ug953-vivado-7series-libraries.pdf
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

module Clash.Xilinx.DDR
  ( iddr
  , oddr
  )
where

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Explicit.Prelude
import Clash.Explicit.DDR

-- | Xilinx specific variant of 'ddrIn' implementend using the Xilinx IDDR
-- primitive.
--
-- Reset values are @0@
iddr
  :: ( HasCallStack
     , fast ~ 'Dom n pFast
     , slow ~ 'Dom n (2*pFast)
     , KnownNat m )
  => Clock slow gated
  -- ^ clock
  -> Reset slow synchronous
  -- ^ reset
  -> Signal fast (BitVector m)
  -- ^ DDR input signal
  -> Signal slow ((BitVector m),(BitVector m))
  -- ^ normal speed output pairs
iddr clk rst = withFrozenCallStack ddrIn# clk rst 0 0 0
{-# NOINLINE iddr #-}

-- | Xilinx specific variant of 'ddrOut' implementend using the Xilinx ODDR
-- primitive.
--
-- Reset value is @0@
oddr
  :: ( slow ~ 'Dom n (2*pFast)
     , fast ~ 'Dom n pFast
     , KnownNat m )
  => Clock slow gated
  -- ^ clock
  -> Reset slow synchronous
  -- ^ reset
  -> Signal slow (BitVector m,BitVector m)
  -- ^ normal speed input pairs
  -> Signal fast (BitVector m)
  -- ^ DDR output signal
oddr clk rst = uncurry (withFrozenCallStack oddr# clk rst) . unbundle

oddr# :: ( slow ~ 'Dom n (2*pFast)
         , fast ~ 'Dom n pFast
         , KnownNat m )
      => Clock slow gated
      -> Reset slow synchronous
      -> Signal slow (BitVector m)
      -> Signal slow (BitVector m)
      -> Signal fast (BitVector m)
oddr# clk rst = ddrOut# clk rst 0
{-# NOINLINE oddr# #-}
