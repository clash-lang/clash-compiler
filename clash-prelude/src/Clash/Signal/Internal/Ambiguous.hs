{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Clash.Signal.Internal.Ambiguous
  ( knownVDomain
  , clockPeriod
  , activeEdge
  , resetKind
  , initBehavior
  , resetPolarity
  ) where

import           Clash.Signal.Internal
import           Clash.Promoted.Nat         (SNat)

-- | Get the clock period from a KnownDomain context
clockPeriod
  :: forall dom period
   . (KnownDomain dom, DomainPeriod dom ~ period)
  => SNat period
clockPeriod =
  case knownDomain @dom of
    SDomainConfiguration _dom period _edge _sync _init _polarity ->
      period
{-# NOINLINE clockPeriod #-}
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
  case knownDomain @dom of
    SDomainConfiguration _dom _period edge _sync _init _polarity ->
      edge
{-# NOINLINE activeEdge #-}
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
  case knownDomain @dom of
    SDomainConfiguration _dom _period _edge sync _init _polarity ->
      sync
{-# NOINLINE resetKind #-}
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
  case knownDomain @dom of
    SDomainConfiguration _dom _period _edge _sync init_ _polarity ->
      init_
{-# NOINLINE initBehavior #-}
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
  case knownDomain @dom of
    SDomainConfiguration _dom _period _edge _sync _init polarity ->
      polarity
{-# NOINLINE resetPolarity #-}
-- @NOINLINE: https://github.com/clash-lang/clash-compiler/issues/662

-- | Like 'knownDomain but yields a 'VDomainConfiguration'. Should only be used
-- in combination with 'createDomain'.
knownVDomain
  :: forall dom
   . KnownDomain dom
  => VDomainConfiguration
knownVDomain =
  vDomain (knownDomain @dom)
