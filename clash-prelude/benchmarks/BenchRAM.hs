{-# LANGUAGE MagicHash, TypeApplications, DataKinds #-}
module BenchRAM (ramBench) where

import Criterion (Benchmark, env, bench, nf, bgroup)

import Clash.Explicit.BlockRam
import Clash.Explicit.RAM
import Clash.Explicit.ROM
import Clash.Explicit.Signal
import Clash.Prelude.ROM
import Clash.Promoted.Nat
import Clash.Promoted.Nat.Literals
import qualified Clash.Sized.Vector as V

ramBench :: Benchmark
ramBench = bgroup "RAMs"
  [ asyncRamBench
  , asyncRomBench
  , blockRamBench
  , blockRamROBench
  , romBench
  ]

asyncRamBench :: Benchmark
asyncRamBench = env setup $ \m ->
  bench "asyncRam#" $
  nf (take 298 . drop 2 . simulate_lazy
        (\rw -> let (r,w) = unbundle rw
                in  asyncRam# @System
                      clockGen
                      clockGen
                      enableGen
                      (SNat @4096)
                      r
                      (pure True)
                      w
                      w
                   )) m
  where
    setup   = pure (zip [556,557..856] [557,558..857])

asyncRomBench :: Benchmark
asyncRomBench = env setup $ \m ->
  bench "asyncRom#" $
  nf (take 98 . drop 2 . fmap (asyncRom# ramInit)) m
  where
    ramInit = V.replicate d1024 (1 :: Int)
    setup   = pure ([557,558..857])

blockRamBench :: Benchmark
blockRamBench = env setup $ \m ->
  bench "blockRam# (100% writes)" $
  nf (take 8298 . drop 2 . simulate_lazy
        (\w -> ram w
                    (pure True)
                    w
                    w
                   )) (cycle m)
  where
    ramInit = V.replicate (SNat @4096) (1 :: Int)
    setup   = pure ([557,558..857])
    ram     = blockRam# @System
                    clockGen
                    enableGen
                    ramInit

blockRamROBench :: Benchmark
blockRamROBench = env setup $ \m ->
  bench "blockRam# (0% writes)" $
  nf (take 8298 . drop 2 . simulate_lazy
        (\w -> ram w
                    (pure False)
                    w
                    w
                   )) (cycle m)
  where
    ramInit = V.replicate (SNat @4096) (1 :: Int)
    setup   = pure ([557,558..857])
    ram     = blockRam# @System
                    clockGen
                    enableGen
                    ramInit

romBench :: Benchmark
romBench = env setup $ \m ->
  bench "rom#" $
  nf (take 98 . drop 2 . simulate_lazy
        (\r -> rom# @System
                    clockGen
                    enableGen
                    ramInit
                    r
                   )) m
  where
    ramInit = V.replicate d1024 (1 :: Int)
    setup   = pure ([557,558..857])
