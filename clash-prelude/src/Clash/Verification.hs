{-|
Copyright  :  (C) 2019, Myrtle Software Ltd
License    :  BSD2 (see the file LICENSE)
Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

Verification
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}
{-# LANGUAGE RankNTypes   #-}

module Clash.Verification where

import Clash.Annotations.Primitive (hasBlackBox)
import Clash.Signal
import Clash.Signal.Internal
import Clash.XException

past
  :: HiddenClock dom
  => Signal dom Bool
  -> Signal dom Bool
past s = errorX "no past" :- s
{-# NOINLINE past #-}
{-# ANN past hasBlackBox #-}

validPast#
  :: KnownDomain dom
  => Clock dom
  -> Signal dom Bool
validPast# !_ = False :- pure True
{-# NOINLINE validPast #-}
{-# ANN validPast hasBlackBox #-}

validPast
  :: HiddenClock dom
  => Signal dom Bool
validPast = hideClock validPast#

assertAt
  :: KnownDomain dom
  => Clock dom
  -> (HiddenClock dom => Signal dom Bool)
  -> a
  -> a
assertAt clk f = assertAt# clk (withClock clk f)
{-# INLINE assertAt #-}

assertAt#
  :: KnownDomain dom
  => Clock dom
  -> Signal dom Bool
  -> a
  -> a
assertAt# !_ !_ = id
{-# NOINLINE assertAt# #-}
{-# ANN assertAt# hasBlackBox #-}

assertAlways
  :: Signal dom Bool
  -> a
  -> a
assertAlways !_ = id
{-# NOINLINE assertAlways #-}
{-# ANN assertAlways hasBlackBox #-}

coverAt
  :: KnownDomain dom
  => Clock dom
  -> (HiddenClock dom => Signal dom Bool)
  -> a
  -> a
coverAt clk f = coverAt# clk (withClock clk f)
{-# INLINE coverAt #-}

coverAt#
  :: KnownDomain dom
  => Clock dom
  -> Signal dom Bool
  -> a
  -> a
coverAt# !_ !_ = id
{-# NOINLINE coverAt# #-}
{-# ANN coverAt# hasBlackBox #-}

coverAlways
  :: Signal dom Bool
  -> a
  -> a
coverAlways !_ = id
{-# NOINLINE coverAlways #-}
{-# ANN coverAt# hasBlackBox #-}
