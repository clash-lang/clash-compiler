{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module CLaSH.Xilinx.DDR (iddr,oddr) where

import GHC.Stack (HasCallStack, withFrozenCallStack)

import CLaSH.Explicit.Prelude
import CLaSH.Explicit.DDR

{-
Xilinx IDDR primitive

UG953 (v2017.2) June 7, 2016, p294
https://www.xilinx.com/support/documentation/sw_manuals/xilinx2017_2/ug953-vivado-7series-libraries.pdf

-}

iddr :: ( HasCallStack
        , fast ~ 'Dom n pFast
        , slow ~ 'Dom n (2*pFast)
        , BitPack a
        , w ~ BitSize a
        , KnownNat w)
     => Clock slow gated
     -> Reset slow synchronous
     -> Signal fast a
     -> Signal slow (a,a)
iddr clk rst = withFrozenCallStack ddrIn# clk rst (unpack 0) (unpack 0) (unpack 0)
{-# NOINLINE iddr #-}

oddr :: ( slow ~ 'Dom n (2*pFast)
        , fast ~ 'Dom n pFast
        , BitPack a
        , KnownNat (BitSize a))
     => Clock slow gated
     -> Reset slow synchronous
     -> Signal slow (a,a)
     -> Signal fast a
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
