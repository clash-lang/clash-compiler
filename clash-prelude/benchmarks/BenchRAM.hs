{-# LANGUAGE MagicHash, TypeApplications #-}
module BenchRAM where

import Criterion (Benchmark, env, bench, nf)

import Clash.Explicit.BlockRam
import Clash.Explicit.RAM
import Clash.Explicit.ROM
import Clash.Explicit.Signal
import Clash.Prelude.ROM
import Clash.Promoted.Nat.Literals
import qualified Clash.Sized.Vector as V

asyncRamBench :: Benchmark
asyncRamBench = env setup $ \m ->
  bench "asyncRam#" $
  nf (take 98 . drop 2 . simulate_lazy
        (\rw -> let (r,w) = unbundle rw
                in  asyncRam# @System
                      clockGen
                      clockGen
                      enableGen
                      d1024
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
  nf (take 98 . drop 2 . simulate_lazy
        (\w -> blockRam# @System
                    clockGen
                    enableGen
                    ramInit
                    w
                    (pure True)
                    w
                    w
                   )) m
  where
    ramInit = V.replicate d1024 (1 :: Int)
    setup   = pure ([557,558..857])

blockRamROBench :: Benchmark
blockRamROBench = env setup $ \m ->
  bench "blockRam# (0% writes)" $
  nf (take 98 . drop 2 . simulate_lazy
        (\w -> blockRam# @System
                    clockGen
                    enableGen
                    ramInit
                    w
                    (pure False)
                    w
                    w
                   )) m
  where
    ramInit = V.replicate d1024 (1 :: Int)
    setup   = pure ([557,558..857])

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
