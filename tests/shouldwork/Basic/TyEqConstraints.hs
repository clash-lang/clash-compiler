module TyEqConstraints where
import GHC.Stack
import Clash.Prelude

-- see https://github.com/clash-lang/clash-compiler/issues/347
topEntity
    :: (dom ~ System)
    => Clock dom
    -> Signal dom Bit
    -> Signal dom Bit
topEntity = exposeClock board
  where
    board = id
{-# NOINLINE topEntity #-}
{-# ANN topEntity (defSyn "top1") #-}

-- type equality is symmetrical so this should also work:
topEntity2
    :: (System ~ dom)
    => Clock dom
    -> Signal dom Bit
    -> Signal dom Bit
topEntity2 = exposeClock board
  where
    board = id
{-# ANN topEntity2 (defSyn "top2") #-}

-- Test constrained tyvar as a result
topEntity3 :: (a ~ Int, res ~ Signal System Int) => Signal System a -> res
topEntity3 = id
{-# ANN topEntity3 (defSyn "top3") #-}

-- Because clash is changing topEntity (removing type arguments),
-- make sure we can still use it correctly:
topEntity4 :: Clock System -> Signal System Bit -> Signal System Bit
topEntity4 = topEntity
{-# ANN topEntity4 (defSyn "top4") #-}


-- test substituting one TyVar with another TyVar
topEntity5 :: (b ~ a, a ~ Int, dom ~ System) => Signal dom b -> Signal System a
topEntity5 = id
{-# ANN topEntity5 (defSyn "top5") #-}

-- make sure we can get past other constraints
topEntity6 :: (a ~ Int, HasCallStack, dom ~ System) => Signal dom a -> Signal System Int
topEntity6 = id
{-# ANN topEntity6 (defSyn "top6") #-}
