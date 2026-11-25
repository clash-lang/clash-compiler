{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Signal.Internal.Ambiguous
  ( knownVClockDomain
  , clockPeriod
  , activeEdge
  , resetKind
  , initBehavior
  , resetPolarity
  ) where

import           Clash.Signal.Internal
import           Clash.Promoted.Nat         (SNat)

-- | Get the whole number of picoseconds in the clock period, rounded down.
-- For the exact period, including fractional picoseconds, use
-- @'vPeriod' ('knownVClockDomain' \@dom)@.
clockPeriod
  :: forall dom period
   . (KnownDomain dom, DomainPeriod dom ~ period)
  => SNat period
clockPeriod =
  case knownClockDomain @dom of
    SClockDomainConfiguration{sPeriod} ->
      sPeriod
{-# OPAQUE clockPeriod #-}
-- @NOINLINE: https://github.com/clash-lang/clash-compiler/issues/662

-- | Get 'ActiveEdge' from a KnownDomain context. Example usage:
--
-- @
-- f :: forall dom . KnownDomain dom => ....
-- f a b c =
--   case activeEdge @dom of
--     SRising -> foo
--     SFalling -> bar
-- @
activeEdge
  :: forall dom edge
   . (KnownDomain dom, DomainActiveEdge dom ~ edge)
  => SActiveEdge edge
activeEdge =
  case knownClockDomain @dom of
    SClockDomainConfiguration{sActiveEdge} ->
      sActiveEdge
{-# OPAQUE activeEdge #-}
-- @NOINLINE: https://github.com/clash-lang/clash-compiler/issues/662

-- | Get 'ResetKind' from a KnownDomain context. Example usage:
--
-- @
-- f :: forall dom . KnownDomain dom => ....
-- f a b c =
--   case resetKind @dom of
--     SAsynchronous -> foo
--     SSynchronous -> bar
-- @
resetKind
  :: forall dom sync
   . (KnownDomain dom, DomainResetKind dom ~ sync)
  => SResetKind sync
resetKind =
  case knownClockDomain @dom of
    SClockDomainConfiguration{sResetKind} ->
      sResetKind
{-# OPAQUE resetKind #-}
-- @NOINLINE: https://github.com/clash-lang/clash-compiler/issues/662

-- | Get 'InitBehavior' from a KnownDomain context. Example usage:
--
-- @
-- f :: forall dom . KnownDomain dom => ....
-- f a b c =
--   case initBehavior @dom of
--     SDefined -> foo
--     SUnknown -> bar
-- @
initBehavior
  :: forall dom init
   . (KnownDomain dom, DomainInitBehavior dom ~ init)
  => SInitBehavior init
initBehavior =
  case knownClockDomain @dom of
    SClockDomainConfiguration{sInitBehavior} ->
      sInitBehavior
{-# OPAQUE initBehavior #-}
-- @NOINLINE: https://github.com/clash-lang/clash-compiler/issues/662

-- | Get 'ResetPolarity' from a KnownDomain context. Example usage:
--
-- @
-- f :: forall dom . KnownDomain dom => ....
-- f a b c =
--   case resetPolarity @dom of
--     SActiveHigh -> foo
--     SActiveLow -> bar
-- @
resetPolarity
  :: forall dom polarity
   . (KnownDomain dom, DomainResetPolarity dom ~ polarity)
  => SResetPolarity polarity
resetPolarity =
  case knownClockDomain @dom of
    SClockDomainConfiguration{sResetPolarity} ->
      sResetPolarity
{-# OPAQUE resetPolarity #-}
-- @NOINLINE: https://github.com/clash-lang/clash-compiler/issues/662

-- | Like 'knownClockDomain but yields a 'VClockDomainConfiguration'. Should only be used
-- in combination with 'createClockDomain'.
knownVClockDomain
  :: forall dom
   . KnownDomain dom
  => VClockDomainConfiguration
knownVClockDomain =
  vClockDomain (knownClockDomain @dom)
