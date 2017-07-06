{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module CLaSH.Intel.DDR (altddioIn, altddioOut) where

import GHC.Stack (HasCallStack, withFrozenCallStack)

import CLaSH.Explicit.Prelude
import CLaSH.Explicit.DDR

{-
altera ALTDDIO IP cores
see: https://www.altera.com/documentation/eis1415168884929.html
or : https://www.altera.com/content/dam/altera-www/global/en_US/pdfs/literature/ug/ug_altddio.pdf

synch reset only available on certain devices
-}

altddioIn :: ( HasCallStack
             , fast ~ 'Dom n pFast
             , slow ~ 'Dom n (2*pFast)
             , BitPack a
             , KnownNat (BitSize a))
          => SSymbol deviceFamily
          -> Clock slow gated
          -> Reset slow synchronous
          -> Signal fast a
          -> Signal slow (a,a)
altddioIn devFam clk rst = ddrIn# clk rst (unpack 0) (unpack 0) (unpack 0)
{-# NOINLINE altddioIn #-}

altddioOut :: ( HasCallStack
              , fast ~ 'Dom n pFast
              , slow ~ 'Dom n (2*pFast)
              , BitPack a
              , KnownNat (BitSize a))
           => SSymbol deviceFamily
           -> Clock slow gated
           -> Reset slow synchronous
           -> Signal slow (a,a)
           -> Signal fast a
altddioOut devFam clk rst = uncurry (altddioOut# devFam clk rst) . unbundle

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
