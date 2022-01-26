{-# LANGUAGE CPP #-}

#include "MachDeps.h"
#define HDLSYN Other

module BenchmarkCommon where

import Clash.Annotations.Primitive (HDL(VHDL))
import Clash.Annotations.BitRepresentation.Internal (CustomReprs)
import Clash.Backend
import Clash.Backend.VHDL
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.Var
import Clash.Driver
import Clash.Driver.Types

import Clash.GHC.PartialEval
import Clash.GHC.Evaluator
import Clash.GHC.GenerateBindings
import Clash.GHC.NetlistTypes
import Clash.Netlist.BlackBox.Types (HdlSyn(Other))
import Clash.Netlist.Types
  (PreserveCase(..), HWMap, FilteredHWType, topId)

import qualified Control.Concurrent.Supply as Supply
import Control.Monad.State.Strict   (State)

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

typeTrans :: (CustomReprs -> TyConMap -> Type ->
              State HWMap (Maybe (Either String FilteredHWType)))
typeTrans = ghcTypeToHWType WORD_SIZE_IN_BITS

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

backend :: VHDLState
backend = initBackend WORD_SIZE_IN_BITS HDLSYN True PreserveCase Nothing (AggressiveXOptBB False) (RenderEnums True)

runInputStage
  :: [FilePath]
  -> FilePath
  -> IO (ClashEnv, ClashDesign)
runInputStage idirs src = do
  let o = opts idirs
  pds <- primDirs backend
  generateBindings o (return ()) pds (opt_importPaths o) [] (hdlKind backend) src Nothing

runNormalisationStage
  :: [FilePath]
  -> String
  -> IO (ClashEnv, ClashDesign, Id)
runNormalisationStage idirs src = do
  supplyN <- Supply.newSupply
  (env, design) <- runInputStage idirs src
  let topEntityNames = fmap topId (designEntities design)
  let topEntity = head topEntityNames
  let transformedBindings =
        normalizeEntity env (designBindings design) typeTrans
          ghcEvaluator
          evaluator
          topEntityNames supplyN topEntity
  return (env, design{designBindings=transformedBindings},topEntity)
