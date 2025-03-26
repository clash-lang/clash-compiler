{-|
Copyright  :  (C) 2017, Google Inc
                  2019, Myrtle Software Ltd
                  2025, QBayLogic B.V.
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

We simulate DDR signal by using 'Signal's which have exactly half the period
(or double the speed) of our normal 'Signal's.

The primitives in this module can be used to produce or consume DDR signals.

DDR signals are not meant to be used internally in a design,
but only to communicate with the outside world.

In some cases hardware specific DDR IN registers can be inferred by synthesis
tools from these generic primitives. But to be sure your design will synthesize
to dedicated hardware resources use the functions from "Clash.Intel.DDR"
or "Clash.Xilinx.DDR".
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Explicit.DDR
  ( ddrIn
  , ddrOut
    -- * Internal
  , ddrIn#
  , ddrOut#
  )
where

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Clash.Annotations.Primitive    (hasBlackBox)
import Clash.Explicit.Prelude
import Clash.Signal.Internal

{- $setup
>>> :set -XNoImplicitPrelude -XTypeFamilies -XFlexibleInstances
>>> import Clash.Explicit.Prelude
>>> import Clash.Explicit.DDR
>>> :{
type Fast = ("Fast" :: Domain)
instance KnownDomain "Fast" where
  type KnownConf "Fast" = 'DomainConfiguration "Fast" 5000 'Rising 'Asynchronous 'Defined 'ActiveHigh
  knownDomain = SDomainConfiguration SSymbol SNat SRising SAsynchronous SDefined SActiveHigh
:}

-}

-- | DDR input primitive
--
-- Consumes a DDR input signal and produces a regular signal containing a pair
-- of values.
--
-- Data is clocked in on both edges of the clock signal. We can discern the
-- /active edge/ of the clock and the /other edge/. When the domain has the
-- rising edge as the active edge (which is the most common), this means that
-- the /rising/ edge is the /active/ edge and the /falling/ edge is the /other/
-- edge.
--
-- Of the output pair @(o0, o1)@, @o0@ is the data clocked in on the /other/
-- edge and @o1@ is the data clocked in on the /active/ edge, and @o0@ comes
-- before @o1@ in time. With a domain where the rising edge is the active edge,
-- this means @o0@ is clocked in on the falling clock edge and @o1@ is clocked
-- in on the rising clock edge. For a domain with the falling edge as the active
-- edge, this is the other way around, but @o0@ still comes before @o1@ in time.
--
-- >>> sampleN 5 $ ddrIn systemClockGen systemResetGen enableGen (-1,-2,-3) (fromList [0..10] :: Signal Fast Int)
-- [(-1,-2),(-1,-2),(-3,2),(3,4),(5,6)]
ddrIn
  :: ( HasCallStack
     , NFDataX a
     , KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity) )
  => Clock slow
  -> Reset slow
  -> Enable slow
  -> (a, a, a)
  -- ^ Reset values
  -> Signal fast a
  -- ^ DDR input signal
  -> Signal slow (a, a)
  -- ^ Normal speed output pair @(o0, o1)@
ddrIn clk rst en (i0,i1,i2) =
  withFrozenCallStack $ ddrIn# clk rst en i0 i1 i2


-- For details about all the seq's en seqX's
-- see the [Note: register strictness annotations] in Clash.Signal.Internal
ddrIn#
  :: forall a slow fast fPeriod polarity edge reset init
   . ( HasCallStack
     , NFDataX a
     , KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity) )
  => Clock slow
  -> Reset slow
  -> Enable slow
  -> a
  -> a
  -> a
  -> Signal fast a
  -> Signal slow (a,a)
ddrIn# (Clock _ Nothing) (unsafeToActiveHigh -> hRst) (fromEnable -> ena) i0 i1 i2 =
  case resetKind @fast of
    SAsynchronous ->
      goAsync
        ( deepErrorX "ddrIn: initial value 0 undefined"
        , deepErrorX "ddrIn: initial value 1 undefined"
        , deepErrorX "ddrIn: initial value 2 undefined" )
        hRst
        ena
    SSynchronous ->
      goSync
        ( deepErrorX "ddrIn: initial value 0 undefined"
        , deepErrorX "ddrIn: initial value 1 undefined"
        , deepErrorX "ddrIn: initial value 2 undefined" )
        hRst
        ena
  where
    goSync
      :: (a, a, a)
      -> Signal slow Bool
      -> Signal slow Bool
      -> Signal fast a
      -> Signal slow (a,a)
    goSync (o0,o1,o2) rt@(~(r :- rs)) ~(e :- es) as@(~(x0 :- x1 :- xs)) =
      let (o0',o1',o2') = if r then (i0,i1,i2) else (o2,x0,x1)
      in o0 `seqX` o1 `seqX` (o0,o1)
           :- (rt `seq` as `seq` if e then goSync (o0',o1',o2') rs es xs
                                      else goSync (o0 ,o1 ,o2)  rs es xs)

    goAsync
      :: (a, a, a)
      -> Signal slow Bool
      -> Signal slow Bool
      -> Signal fast a
      -> Signal slow (a, a)
    goAsync (o0,o1,o2) ~(r :- rs) ~(e :- es) as@(~(x0 :- x1 :- xs)) =
      let (o0',o1',o2',o3',o4') = if r then (i0,i1,i0,i1,i2) else (o0,o1,o2,x0,x1)
      in o0' `seqX` o1' `seqX` (o0',o1')
           :- (as `seq` if e then goAsync (o2',o3',o4') rs es xs
                             else goAsync (o0',o1',o2') rs es xs)

ddrIn# _ _ _ _ _ _ =
  error "ddrIn#: dynamic clocks not supported"
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE ddrIn# #-}
{-# ANN ddrIn# hasBlackBox #-}

-- | DDR output primitive
--
-- Produces a DDR output signal from a normal signal of pairs of input.
--
-- Data is clocked out on both edges of the clock signal. We can discern the
-- /active edge/ of the clock and the /other edge/. When the domain has the
-- rising edge as the active edge (which is the most common), this means that
-- the /rising/ edge is the /active/ edge and the /falling/ edge is the /other/
-- edge.
--
-- Of the input pair @(i0, i1)@, @i0@ is the data clocked out on the /active/
-- edge and @i1@ is the data clocked out on the /other/ edge, and @i0@ comes
-- before @i1@ in time. With a domain where the rising edge is the active edge,
-- this means @i0@ is clocked out on the rising clock edge and @i1@ is clocked
-- out on the falling clock edge. For a domain with the falling edge as the
-- active edge, this is the other way around, but @i0@ still comes before @i1@
-- in time.
--
-- >>> sampleN 7 (ddrOut systemClockGen systemResetGen enableGen (-1) (fromList [(0,1),(2,3),(4,5)]) :: Signal Fast Int)
-- [-1,-1,-1,2,3,4,5]
ddrOut
  :: ( HasCallStack
     , NFDataX a
     , KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity) )
  => Clock slow
  -> Reset slow
  -> Enable slow
  -> a
  -- ^ Reset value
  -> Signal slow (a, a)
  -- ^ Normal speed input pair @(i0, i1)@
  -> Signal fast a
  -- ^ DDR output signal
ddrOut clk rst en i0 =
  uncurry (withFrozenCallStack $ ddrOut# clk rst en i0) . unbundle


ddrOut#
  :: ( HasCallStack
     , NFDataX a
     , KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity) )
  => Clock slow
  -> Reset slow
  -> Enable slow
  -> a
  -> Signal slow a
  -> Signal slow a
  -> Signal fast a
ddrOut# clk rst en i0 xs ys =
    -- We only observe one reset value, because when the mux switches on the
    -- next clock level, the second register will already be outputting its
    -- first input.
    --
    -- That is why we drop the first value of the stream.
    let (_ :- out) = zipSig xs' ys' in out
  where
    xs' = register# clk rst en (errorX "ddrOut: unreachable error") i0 xs
    ys' = register# clk rst en (deepErrorX "ddrOut: initial value undefined") i0 ys
    zipSig (a :- as) (b :- bs) = a :- b :- zipSig as bs
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE ddrOut# #-}
{-# ANN ddrOut# hasBlackBox #-}
