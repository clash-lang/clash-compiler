{-# LANGUAGE CPP #-}

-- | 'blockRamU' and 'blockRam1' storing 'Vec' and 'RTree' values. In
-- SystemVerilog these are unpacked arrays, which cannot be stored in or read
-- from a memory with packed elements without converting them.
module VecPayload where

import Clash.Prelude
import Clash.Explicit.Testbench

type V = Vec 2 (Unsigned 8)
type T = RTree 1 (Unsigned 8)

-- | Read address, write operation, and whether the outputs of the 'blockRamU'
-- and 'blockRam1' instances (respectively) are checked in the next cycle.
type Input = (Index 8, Maybe (Index 8, V), Bool, Bool)

-- | Outputs of 'blockRamU' (Vec, RTree) and 'blockRam1' (Vec, RTree). Outputs
-- that are not checked are replaced by zeroes, so no undefined values reach
-- the output.
type Output = (V, T, V, T)

topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System Input
  -> Signal System Output
topEntity = exposeClockResetEnable go
 where
  go :: HiddenClockResetEnable System => Signal System Input -> Signal System Output
  go inp = bundle (ramU validU, ramUT validU, ram1 valid1, ram1T valid1)
   where
    (rd, wr, checkU, check1) = unbundle inp
    wrT = fmap (fmap v2t) <$> wr

    validU = register False checkU
    valid1 = register False check1

    ramU v = mux v (blockRamU NoClearOnReset d8 rd wr) (pure (repeat 0))
    ramUT v = mux v (blockRamU NoClearOnReset d8 rd wrT) (pure (treplicate d1 0))
    ram1 v = mux v (blockRam1 NoClearOnReset d8 (3 :> 4 :> Nil) rd wr) (pure (repeat 0))
    ram1T v = mux v (blockRam1 NoClearOnReset d8 (v2t (5 :> 6 :> Nil)) rd wrT) (pure (treplicate d1 0))
{-# OPAQUE topEntity #-}

testBench :: Signal System Bool
testBench = done
 where
  testInput = stimuliGenerator clk rst inputs
  inputs :: Vec 11 Input
  inputs =
       -- Read unwritten address: only 'blockRam1' is defined
       (5, Just (0, 1 :> 2 :> Nil), False, True)
    :> (5, Just (1, 3 :> 4 :> Nil), False, True)
    :> (0, Just (2, 5 :> 6 :> Nil), False, True)
    :> (0, Just (3, 7 :> 8 :> Nil), False, True)
       -- Read back written values
    :> (0, Nothing, True, True)
    :> (1, Nothing, True, True)
    :> (2, Nothing, True, True)
       -- Read while writing the same address
    :> (3, Just (3, 9 :> 10 :> Nil), True, True)
    :> (3, Nothing, True, True)
       -- Unwritten address again
    :> (6, Nothing, False, True)
    :> (6, Nothing, False, True)
    :> Nil

  z = repeat 0 :: V
  zT = treplicate d1 0 :: T
  o :: V -> V -> V -> Output
  o u r1 r1T = (u, v2t u, r1, v2t r1T)

  expected :: Vec 12 Output
  expected =
       (z, zT, z, zT)
    :> (z, zT, 3 :> 4 :> Nil, v2t (5 :> 6 :> Nil))
    :> (z, zT, 3 :> 4 :> Nil, v2t (5 :> 6 :> Nil))
    :> (z, zT, 1 :> 2 :> Nil, v2t (1 :> 2 :> Nil))
    :> (z, zT, 1 :> 2 :> Nil, v2t (1 :> 2 :> Nil))
    :> o (1 :> 2 :> Nil) (1 :> 2 :> Nil) (1 :> 2 :> Nil)
    :> o (3 :> 4 :> Nil) (3 :> 4 :> Nil) (3 :> 4 :> Nil)
    :> o (5 :> 6 :> Nil) (5 :> 6 :> Nil) (5 :> 6 :> Nil)
    :> o (7 :> 8 :> Nil) (7 :> 8 :> Nil) (7 :> 8 :> Nil)
    :> o (9 :> 10 :> Nil) (9 :> 10 :> Nil) (9 :> 10 :> Nil)
    :> (z, zT, 3 :> 4 :> Nil, v2t (5 :> 6 :> Nil))
    :> (z, zT, 3 :> 4 :> Nil, v2t (5 :> 6 :> Nil))
    :> Nil

  done = outputVerifier' clk rst expected (topEntity clk rst enableGen testInput)
  clk = tbSystemClockGen (not <$> done)
  rst = systemResetGen
{-# OPAQUE testBench #-}
