{-# LANGUAGE CPP #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import           Clash.Signal (VDomainConfiguration)

import           Clash.Annotations.BitRepresentation.Internal (CustomReprs)
import           Clash.Core.TyCon
import           Clash.Core.Var
import           Clash.Driver
import           Clash.Driver.Types

import           Clash.GHC.PartialEval
import           Clash.GHC.Evaluator

import           Clash.Netlist.Types          (TopEntityT)
import           Clash.Primitives.Types

import           Criterion.Main

import qualified Control.Concurrent.Supply    as Supply
import           Control.DeepSeq              (NFData(..), rwhnf)
import           Data.HashMap.Strict          (HashMap)
import           Data.IntMap.Strict           (IntMap)
import           Data.List                    (isPrefixOf, partition)
import           Data.Text                    (Text)
import           System.Environment           (getArgs, withArgs)

import BenchmarkCommon

-- | Run benchmark the normalization process
--
-- You can provide you own test cases as commandline arguments.
--
-- All arguments from the first one starting with a '-' are given to criterion.
-- All argument before that are interpreted as test cases.
main :: IO ()
main = do
  args <- getArgs
  let (idirs0,rest)         = partition ((== "-i") . take 2) args
      idirs1                = ".":map (drop 2) idirs0
      (fileArgs,optionArgs) = break (isPrefixOf "-") rest
      tests | null fileArgs = defaultTests
            | otherwise     = fileArgs

  withArgs optionArgs (defaultMain $ fmap (benchFile idirs1) tests)

benchFile :: [FilePath] -> FilePath -> Benchmark
benchFile idirs src =
  env (setupEnv idirs src) $
    \ ~((bindingsMap,tcm,tupTcm,_topEntities,primMap,reprs,_domainConfs,topEntityNames,topEntity),supplyN) -> do
      bench ("normalization of " ++ src)
            (nf (normalizeEntity reprs bindingsMap primMap tcm tupTcm typeTrans
                                 ghcEvaluator
                                 evaluator
                                 topEntityNames
                                 (opts idirs) supplyN :: _ -> BindingMap) topEntity)

setupEnv
  :: [FilePath]
  -> FilePath
  -> IO ((BindingMap, TyConMap, IntMap TyConName
         ,[TopEntityT]
         ,CompiledPrimMap, CustomReprs, HashMap Text VDomainConfiguration, [Id], Id
         )
        ,Supply.Supply
        )
setupEnv idirs src = do
  inp <- runInputStage idirs src
  supplyN <- Supply.newSupply
  return (inp,supplyN)

instance NFData Supply.Supply where
  rnf = rwhnf
