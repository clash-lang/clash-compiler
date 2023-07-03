{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module ManyEntitiesVaried where

import Clash.Prelude

-- When compiling different top entities in parallel, it is probably atypical
-- that each entity takes the same amount of time to normalize. If we have more
-- entities to compile than cores available, we may also experience a scenario
-- where faster compiling entities have to wait on slower entities.
--
-- The sizes here are either powers of 2, or numbers halfway between two powers
-- of 2. This gives a reasonable variation in time to compile, while still
-- compiling with a reasonable specialization limit (under 100) and amount of
-- time for each entity (under 10s).

stage
  :: (HiddenClockResetEnable System, KnownNat n)
  => (Unsigned n)
  -> Signal System (Unsigned n)
  -> Signal System (Unsigned n)
stage i s = let delay = register i $ xor <$> delay <*> s in delay

entity
  :: (HiddenClockResetEnable System, KnownNat n)
  => SNat n
  -> Signal System (Unsigned n)
  -> Signal System (Unsigned n)
entity n =
  foldr (.) id $ map stage $ iterate n (+ 1) 1

type Top n =
  HiddenClockResetEnable System
    => Signal System (Unsigned n)
    -> Signal System (Unsigned n)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE top0 #-}
{-# ANN top0 (defSyn "top_0") #-}
top0 :: Top 24
top0 = entity (SNat @24)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE top1 #-}
{-# ANN top1 (defSyn "top_1") #-}
top1 :: Top 32
top1 = entity (SNat @32)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE top2 #-}
{-# ANN top2 (defSyn "top_2") #-}
top2 :: Top 48
top2 = entity (SNat @48)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE top3 #-}
{-# ANN top3 (defSyn "top_3") #-}
top3 :: Top 64
top3 = entity (SNat @64)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE top4 #-}
{-# ANN top4 (defSyn "top_4") #-}
top4 :: Top 96
top4 = entity (SNat @96)

-- At 128 we blow a specialization limit of 100, and take more than 10s to
-- normalize on my machine. If Clash becomes quicker we can increase the limit
-- we generate to here. -- Alex
