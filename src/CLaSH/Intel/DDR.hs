{-|
DDR primitives for Intel FPGAs using ALTDDIO primitives.

For general information about DDR primitives see "CLaSH.Explicit.DDR".

Note that a synchronous reset is only available on certain devices,
see ALTDDIO userguide for the specifics: https://www.altera.com/content/dam/altera-www/global/en_US/pdfs/literature/ug/ug_altddio.pdf
-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module CLaSH.Intel.DDR (altddioIn, altddioOut) where

import GHC.Stack (HasCallStack, withFrozenCallStack)

import CLaSH.Explicit.Prelude
import CLaSH.Explicit.DDR

{-| Intel specific variant of 'ddrIn' implemented using the ALTDDIO_IN IP core.
-}
altddioIn
  :: ( HasCallStack
     , fast ~ 'Dom n pFast
     , slow ~ 'Dom n (2*pFast)
     , BitPack a
     , KnownNat (BitSize a))
  => SSymbol deviceFamily
  -- ^ The FPGA family
  --
  -- For example this can be instantiated as follows:
  --
  -- > SSymbol @"Cyclone IV GX"
  -> Clock slow gated            -- ^ clock
  -> Reset slow synchronous      -- ^ reset
  -> Signal fast a               -- ^ DDR input signal
  -> Signal slow (a,a)           -- ^ normal speed output pairs
altddioIn devFam clk rst = withFrozenCallStack ddrIn# clk rst (unpack 0) (unpack 0) (unpack 0)
{-# NOINLINE altddioIn #-}

{-| Intel specific vairant of 'ddrOut' implementend using the ALTDDIO_OUT IP core.
-}
altddioOut
  :: ( HasCallStack
     , fast ~ 'Dom n pFast
     , slow ~ 'Dom n (2*pFast)
     , BitPack a
     , KnownNat (BitSize a))
  => SSymbol deviceFamily
  -- ^ The FPGA family
  --
  -- For example this can be instantiated as follows:
  --
  -- > SSymbol @"Cyclone IV E"
  -> Clock slow gated           -- ^ clock
  -> Reset slow synchronous     -- ^ reset
  -> Signal slow (a,a)          -- ^ normal speed input pair
  -> Signal fast a              -- ^ DDR output signal
altddioOut devFam clk rst = uncurry (withFrozenCallStack altddioOut# devFam clk rst) . unbundle

altddioOut# :: ( HasCallStack
               , fast ~ 'Dom n pFast
               , slow ~ 'Dom n (2*pFast)
               , BitPack a
               , KnownNat (BitSize a))
            => SSymbol deviceFamily
            -> Clock slow gated
            -> Reset slow synchronous
            -> Signal slow a
            -> Signal slow a
            -> Signal fast a
altddioOut# _ clk rst = ddrOut# clk rst (unpack 0)
{-# NOINLINE altddioOut# #-}
