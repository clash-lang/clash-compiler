{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module ManyEntitiesEqual where

import Clash.Prelude

-- When compiling different top entities in parallel, the best case is that
-- every entity takes roughly the same amount of time to compile. This means
-- we avoid situations where slower compiling entities take up all cores,
-- lowering compiler throughput.
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
top0 :: Top 64
top0 = entity (SNat @64)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE top1 #-}
{-# ANN top1 (defSyn "top_1") #-}
top1 :: Top 64
top1 = entity (SNat @64)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE top2 #-}
{-# ANN top2 (defSyn "top_2") #-}
top2 :: Top 64
top2 = entity (SNat @64)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE top3 #-}
{-# ANN top3 (defSyn "top_3") #-}
top3 :: Top 64
top3 = entity (SNat @64)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE top4 #-}
{-# ANN top4 (defSyn "top_4") #-}
top4 :: Top 64
top4 = entity (SNat @64)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE top5 #-}
{-# ANN top5 (defSyn "top_5") #-}
top5 :: Top 64
top5 = entity (SNat @64)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE top6 #-}
{-# ANN top6 (defSyn "top_6") #-}
top6 :: Top 64
top6 = entity (SNat @64)

-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE top7 #-}
{-# ANN top7 (defSyn "top_7") #-}
top7 :: Top 64
top7 = entity (SNat @64)
