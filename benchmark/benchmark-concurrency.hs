{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Criterion.Main
import Data.List (isPrefixOf, partition)
import Data.Time (getCurrentTime)
import System.Environment (getArgs, withArgs)

import Clash.Backend
import Clash.Backend.VHDL (VHDLState)
import Clash.Driver
import Clash.Driver.Types (ClashEnv(..), ClashOpts(..))

import Clash.GHC.PartialEval
import Clash.GHC.Evaluator
import Clash.GHC.NetlistTypes (ghcTypeToHWType)

import BenchmarkCommon

main :: IO ()
main = do
  args <- getArgs
  let (idirs0,rest)         = partition ((== "-i") . take 2) args
      idirs1                = ".":map (drop 2) idirs0
      (fileArgs,optionArgs) = break (isPrefixOf "-") rest
      tests | null fileArgs = concurrencyTests
            | otherwise     = fileArgs

  withArgs optionArgs (defaultMain $ fmap (benchFile idirs1) tests)
 where
  concurrencyTests =
    [ "benchmark/tests/ManyEntitiesEqual.hs"
    , "benchmark/tests/ManyEntitiesVaried.hs"
    ]

benchFile :: [FilePath] -> FilePath -> Benchmark
benchFile idirs src =
  env ((,) <$> runInputStage idirs src <*> getCurrentTime) $
    \ ~((clashEnv, clashDesign),startTime) -> do
      bench ("Generating HDL: " ++ src)
            (nfIO (generateHDL clashEnv clashDesign
                     (Just (initBackend @VHDLState (envOpts clashEnv)))
                     (ghcTypeToHWType (_opt_intWidth (envOpts clashEnv)))
                     ghcEvaluator evaluator Nothing startTime))
