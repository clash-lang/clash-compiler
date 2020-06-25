{-# LANGUAGE CPP #-}

#include "MachDeps.h"
#define HDLSYN Other

module BenchmarkCommon where

import Clash.Annotations.BitRepresentation.Internal (CustomReprs, buildCustomReprs)
import Clash.Backend
import Clash.Backend.VHDL
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Core.Var
import Clash.Driver
import Clash.Driver.Types
import Clash.GHC.Evaluator
import Clash.GHC.GenerateBindings
import Clash.GHC.NetlistTypes
import Clash.Netlist.BlackBox.Types (HdlSyn(Other))
import Clash.Netlist.Types          (HWMap, FilteredHWType, TopEntityT, topId)
import Clash.Primitives.Types

import Util (OverridingBool(..))

import qualified Control.Concurrent.Supply as Supply
import Control.Monad.State.Strict   (State)
import Data.IntMap.Strict           (IntMap)

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
typeTrans = ghcTypeToHWType WORD_SIZE_IN_BITS True

opts :: [FilePath] -> ClashOpts
opts idirs =
  defClashOpts{
      opt_cachehdl=False
    , opt_errorExtra = True
    , opt_floatSupport = True
    , opt_importPaths=idirs
    , opt_specLimit=40 -- For "PipelinesViaFolds"
    }

backend :: VHDLState
backend = initBackend WORD_SIZE_IN_BITS HDLSYN True Nothing

runInputStage
  :: [FilePath]
  -- ^ Import dirs
  -> FilePath
  -> IO (BindingMap
        ,TyConMap
        ,IntMap TyConName
        ,[TopEntityT]
        ,CompiledPrimMap
        ,CustomReprs
        ,[Id]
        ,Id
        )
runInputStage idirs src = do
  pds <- primDirs backend
  (bindingsMap,tcm,tupTcm,topEntities,primMap,reprs,_domainConfs) <- generateBindings Auto pds idirs [] (hdlKind backend) src Nothing
  let topEntityNames = map topId topEntities
      tm = head topEntityNames
  return (bindingsMap,tcm,tupTcm,topEntities, primMap, buildCustomReprs reprs, topEntityNames,tm)

runNormalisationStage
  :: [FilePath]
  -> String
  -> IO (BindingMap
        ,[TopEntityT]
        ,CompiledPrimMap
        ,TyConMap
        ,CustomReprs
        ,Id
        )
runNormalisationStage idirs src = do
  supplyN <- Supply.newSupply
  (bindingsMap,tcm,tupTcm,topEntities,primMap,reprs,topEntityNames,topEntity) <-
    runInputStage idirs src
  let opts1 = opts idirs
      transformedBindings =
        normalizeEntity reprs bindingsMap primMap tcm tupTcm typeTrans evaluator
          topEntityNames opts1 supplyN topEntity
  return (transformedBindings,topEntities,primMap,tcm,reprs,topEntity)
