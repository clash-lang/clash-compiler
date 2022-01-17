{-# LANGUAGE TypeApplications #-}

module BenchmarkCommon where

import Clash.Annotations.Primitive (HDL(VHDL))
import Clash.Backend
import Clash.Backend.VHDL
import Clash.Core.Var
import Clash.Driver
import Clash.Driver.Types
import Clash.Netlist.Types (TopEntityT(topId))

import Clash.GHC.PartialEval
import Clash.GHC.Evaluator
import Clash.GHC.GenerateBindings
import Clash.GHC.NetlistTypes

import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Supply as Supply

defaultTests :: [FilePath]
defaultTests =
  [ "examples/FIR.hs"
  , "examples/Reducer.hs"
  , "examples/Queens.hs"
  , "benchmark/tests/BundleMapRepeat.hs"
  , "benchmark/tests/PipelinesViaFolds.hs"
  , "tests/shouldwork/Basic/AES.hs"
  , "tests/shouldwork/Basic/T1354B.hs"
  ]

opts :: [FilePath] -> ClashOpts
opts idirs =
  defClashOpts{
      opt_cachehdl=False
    , opt_clear=True
    , opt_errorExtra = True
    , opt_importPaths=idirs
    , opt_specLimit=100 -- For "ManyEntitiesVaried"
    }

hdl :: HDL
hdl = VHDL

runInputStage
  :: [FilePath]
  -> FilePath
  -> IO (ClashEnv, ClashDesign)
runInputStage idirs src = do
  let o = opts idirs
  let backend = initBackend @VHDLState o
  pds <- primDirs backend
  generateBindings o (return ()) pds (opt_importPaths o) [] (hdlKind backend) src Nothing

runNormalisationStage
  :: [FilePath]
  -> String
  -> IO (ClashEnv, ClashDesign, Id)
runNormalisationStage idirs src = do
  supplyN <- Supply.newSupply
  lock <- MVar.newMVar ()
  (env, design) <- runInputStage idirs src
  let topEntityNames = fmap topId (designEntities design)
  let topEntity = head topEntityNames
  transformedBindings <-
        normalizeEntity env (designBindings design)
          (ghcTypeToHWType (opt_intWidth (opts idirs)))
          ghcEvaluator
          evaluator
          lock
          topEntityNames supplyN topEntity
  return (env, design{designBindings=transformedBindings},topEntity)
