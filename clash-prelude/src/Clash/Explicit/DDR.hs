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
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Explicit.DDR
  ( ddrIn
  , ddrOut
  , ddrForwardClock
    -- * Internal
  , ddrIn#
  , ddrOut#
  , ddrForwardClock#
  )
where

import Data.List.Infinite (Infinite(..), (...))
import Data.String.Interpolate (__i)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import Unsafe.Coerce (unsafeCoerce)

import Clash.Annotations.Primitive (hasBlackBox, Primitive(..), HDL(..))
import Clash.Explicit.Prelude hiding ((:<))
import Clash.Signal.Internal

{- $setup
>>> :set -XNoImplicitPrelude -XTypeFamilies -XFlexibleInstances
>>> import Clash.Explicit.Prelude
>>> import Clash.Explicit.DDR
>>> :{
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
-- >>> printX $ sampleN 5 $ ddrIn systemClockGen systemResetGen enableGen (-1,-2,-3) (fromList [0..10] :: Signal "Fast" Int)
-- [(-1,-2),(-1,-2),(-3,2),(3,4),(5,6)]
ddrIn
  :: ( HasCallStack
     , NFDataX a
     , KnownConfiguration fast ('DomainConfiguration fast fPeriod edge reset init polarity)
     , KnownConfiguration slow ('DomainConfiguration slow (2*fPeriod) edge reset init polarity) )
  => Clock slow
  -- ^ clock
  -> Reset slow
  -- ^ reset
  -> Enable slow
  -> (a, a, a)
  -- ^ reset values
  -> Signal fast a
  -- ^ DDR input signal
  -> Signal slow (a, a)
  -- ^ normal speed output pairs
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
-- >>> sampleN 7 (ddrOut systemClockGen systemResetGen enableGen (-1) (fromList [(0,1),(2,3),(4,5)]) :: Signal "Fast" Int)
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
  -- ^ reset value
  -> Signal slow (a, a)
  -- ^ Normal speed input pairs
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

-- | Use a DDR output primitive to forward a clock to an output pin
--
-- This function allows outputting a clock signal on a DDR-capable output pin.
-- As with the DDR output primitive itself, the created clock cannot be used
-- internally in the design.
--
-- The @ddrOut@ primitive passed in will always have its enable asserted. If the
-- @Enable@ input of @ddrForwardClock@ is deasserted, the data inputs of the
-- @ddrOut@ primitive will switch to achieve the desired output signal. This is
-- because the behavior of the enable input of the DDR primitive differs between
-- vendor-specific primitives.
--
-- The @Reset@ input of this function is passed on to the @ddrOut@ primitive and
-- not otherwise used by @ddrForwardClock@.
--
-- With the @phase@ argument, the phase relation between input and output clock
-- can be defined. With the argument @Nothing@, the clocks are in phase: the
-- active edge of the output clock is on the active edge of the input clock,
-- even if the domains differ on what the active edge is.
--
-- With the @idle@ argument, the output level when the @Enable@ input is
-- deasserted can be defined. With @Nothing@, it will be 0 for a clock with the
-- rising edge as the active edge, and 1 for a clock with the falling edge as
-- the active edge.
--
-- __NB__: The deassertion of the @Enable@ input or the assertion of the @Reset@
-- input is not faithfully simulated in Haskell simulation: Haskell simulation
-- of a Clash design has clocks that always run. The generated HDL will actually
-- output an idle state when @Enable@ is deasserted (and the reset depends on
-- the @ddrOut@ primitive used).
ddrForwardClock
  :: forall domDDR domOut domIn
   . KnownDomain domOut
  => DomainPeriod domIn ~ DomainPeriod domOut
  => DomainPeriod domIn ~ (2 * DomainPeriod domDDR)
  => Clock domIn
  -> Reset domIn
  -> Enable domIn
  -> Maybe Bit
  -- ^ @idle@: Output value when @Enable@ is deasserted
  -> Maybe Bit
  -- ^ @phase@: Value to output at active edge of incoming clock
  -> (Clock domIn -> Reset domIn -> Enable domIn -> Signal domIn (Bit, Bit)
      -> Signal domDDR Bit)
  -- ^ @ddrOut@ primitive to use
  -> Clock domOut
ddrForwardClock clk rst en idle phase oddr =
  ddrForwardClock# clk $ oddr clk rst enableGen ins
 where
  ins =
    mux
      (fromEnable en)
      (pure (activeLevel, complement activeLevel))
      (pure (idleLevel, idleLevel))
  activeLevel =
    case phase of
      Nothing ->
        case activeEdge @domOut of
          SRising -> 1
          SFalling -> 0
      Just x -> x
  idleLevel =
    case idle of
      Nothing ->
        case activeEdge @domOut of
          SRising -> 0
          SFalling -> 1
      Just x -> x

ddrForwardClock#
  :: KnownDomain domOut
  => DomainPeriod domIn ~ DomainPeriod domOut
  => DomainPeriod domIn ~ (2 * DomainPeriod domDDR)
  => Clock domIn
  -> Signal domDDR Bit
  -> Clock domOut
ddrForwardClock# (Clock SSymbol periods) ddrSignal =
  Clock (ddrSignal `seq` SSymbol) (unsafeCoerce periods)
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE ddrForwardClock# #-}
{-# ANN ddrForwardClock# (
  let
    bbName = show 'ddrForwardClock#
    _knownDomOut
      :< _domInOutPeriod
      :< _domDDRPeriod
      :< _clkIn
      :< ddrSignal
      :< _ = ((0 :: Int)...)
  in InlineYamlPrimitive [VHDL] [__i|
    BlackBox:
      name: #{bbName}
      kind: Expression
      template: ~TYPMO'(~ARG[#{ddrSignal}])
      workInfo: Never
    |]) #-}
{-# ANN ddrForwardClock# (
  let
    bbName = show 'ddrForwardClock#
    _knownDomOut
      :< _domInOutPeriod
      :< _domDDRPeriod
      :< _clkIn
      :< ddrSignal
      :< _ = ((0 :: Int)...)
  in InlineYamlPrimitive [Verilog, SystemVerilog] [__i|
    BlackBox:
      name: #{bbName}
      kind: Expression
      template: ~ARG[#{ddrSignal}]
      workInfo: Never
    |]) #-}
