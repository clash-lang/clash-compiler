{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-orphans                 #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import           Clash.Annotations.BitRepresentation.Internal (CustomReprs)
import           Clash.Annotations.TopEntity
import           Clash.Core.Name
import           Clash.Core.Term
import           Clash.Core.TyCon
import           Clash.Core.Type
import           Clash.Driver
import           Clash.Driver.Types
import           Clash.GHC.Evaluator          (reduceConstant)
import           Clash.Primitives.Types

import           Criterion.Main

import qualified Control.Concurrent.Supply    as Supply
import           Control.DeepSeq              (NFData(rnf),rwhnf)
import           Data.HashMap.Strict          (HashMap)
import           Data.IntMap.Strict           (IntMap)
import           Data.List                    (break, isPrefixOf)
import           System.Environment           (getArgs, withArgs)
import           System.FilePath              (FilePath)

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
  let (fileArgs,optionArgs) = break (isPrefixOf "-") args
      tests | null fileArgs = defaultTests
            | otherwise     = fileArgs
  withArgs optionArgs (defaultMain $ fmap benchFile tests)

benchFile :: FilePath -> Benchmark
benchFile src =
  env (setupEnv src) $
    \ ~((bindingsMap,tcm,tupTcm,_topEntities,primMap,reprs,topEntityNames,topEntity),supplyN) -> do
      bench ("normalization of " ++ src)
            (nf (normalizeEntity reprs bindingsMap primMap tcm tupTcm typeTrans
                                 reduceConstant topEntityNames
                                 opts supplyN :: _ -> BindingMap) topEntity)

setupEnv
  :: FilePath
  -> IO ((BindingMap, HashMap TyConOccName TyCon, IntMap TyConName
         ,[(TmName, Type, Maybe TopEntity, Maybe TmName)]
         ,CompiledPrimMap, CustomReprs, [OccName Term], TmOccName
         )
        ,Supply.Supply
        )
setupEnv src = do
  inp <- runInputStage src
  supplyN <- Supply.newSupply
  return (inp,supplyN)

instance NFData Supply.Supply where
  rnf = rwhnf
