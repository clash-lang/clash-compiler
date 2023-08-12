{-# LANGUAGE CPP #-}

import           Clash.Driver
import           Clash.Driver.Types           (ClashEnv(..), ClashOpts(opt_intWidth))

import           Clash.GHC.PartialEval
import           Clash.GHC.Evaluator
import           Clash.GHC.NetlistTypes       (ghcTypeToHWType)

import qualified Control.Concurrent.Supply    as Supply
import           Control.DeepSeq              (deepseq)
import           Data.Binary                  (decode)
import           Data.List                    (partition)
import           System.Environment           (getArgs)

import qualified Data.ByteString.Lazy as B

import           SerialiseInstances
import           BenchmarkCommon

main :: IO ()
main = do
  args <- getArgs
  let (idirs0,tests0) = partition ((== "-i") . take 2) args
  let tests1 | null tests0 = defaultTests
             | otherwise = tests0
      idirs1 = ".":map (drop 2) idirs0

  res <- mapM (benchFile idirs1) tests1
  deepseq res $ return ()

benchFile :: [FilePath] -> FilePath -> IO ()
benchFile idirs src = do
  supplyN <- Supply.newSupply
  (bindingsMap, tcm, tupTcm, primMap, reprs, topEntityNames, topEntity, domains) <- setupEnv src
  putStrLn $ "Doing normalization of " ++ src

  let clashEnv = ClashEnv
                   { envOpts = opts idirs
                   , envTyConMap = tcm
                   , envTupleTyCons = tupTcm
                   , envPrimitives = fmap (fmap unremoveBBfunc) primMap
                   , envCustomReprs = reprs
                   , envDomains = domains
                   }

  res <- normalizeEntity clashEnv bindingsMap
                   (ghcTypeToHWType (opt_intWidth (envOpts clashEnv)))
                   ghcEvaluator
                   evaluator
                   topEntityNames supplyN topEntity
  res `deepseq` putStrLn ".. done\n"

setupEnv :: FilePath -> IO NormalizationInputs
setupEnv src = do
  let bin = src ++ ".bin"
  putStrLn $ "Reading from: " ++ bin
  decode <$> B.readFile bin
