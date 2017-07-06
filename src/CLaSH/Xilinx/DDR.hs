{-|
DDR primitives for Xilinx FPGAs

For general information about DDR primitives see "CLaSH.Explicit.DDR".

-}

{-
For more information see:
Vivado Design Suite 7 Series FPGA and Zynq-7000 All Programmable SoC Libraries Guide
UG953 (v2017.2) June 7, 2016, p294-296,p404-406
https://www.xilinx.com/support/documentation/sw_manuals/xilinx2017_2/ug953-vivado-7series-libraries.pdf
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module CLaSH.Xilinx.DDR (iddr,oddr) where

import GHC.Stack (HasCallStack, withFrozenCallStack)

import CLaSH.Explicit.Prelude
import CLaSH.Explicit.DDR

{-| Xilinx specific variant of 'ddrIn' implementend using the Xilinx IDDR primitive.
-}

iddr :: ( HasCallStack
        , fast ~ 'Dom n pFast
        , slow ~ 'Dom n (2*pFast)
        , BitPack a
        , w ~ BitSize a
        , KnownNat w)
     => Clock slow gated         -- ^ clock
     -> Reset slow synchronous   -- ^ reset
     -> Signal fast a            -- ^ DDR input signal
     -> Signal slow (a,a)        -- ^ normal speed output pairs
iddr clk rst = withFrozenCallStack ddrIn# clk rst (unpack 0) (unpack 0) (unpack 0)
{-# NOINLINE iddr #-}


{-| Xilinx specific variant of 'ddrOut' implementend using the Xilinx ODDR primitive.
-}
oddr :: ( slow ~ 'Dom n (2*pFast)
        , fast ~ 'Dom n pFast
        , BitPack a
        , KnownNat (BitSize a))
     => Clock slow gated         -- ^ clock
     -> Reset slow synchronous   -- ^ reset
     -> Signal slow (a,a)        -- ^ normal speed input pairs
     -> Signal fast a            -- ^ DDR output signal
oddr clk rst = uncurry (withFrozenCallStack oddr# clk rst) . unbundle

oddr# :: ( slow ~ 'Dom n (2*pFast)
         , fast ~ 'Dom n pFast
         , BitPack a
         , KnownNat (BitSize a))
      => Clock slow gated
      -> Reset slow synchronous
      -> Signal slow a
      -> Signal slow a
      -> Signal fast a
oddr# clk rst = ddrOut# clk rst (unpack 0)
{-# NOINLINE oddr# #-}
