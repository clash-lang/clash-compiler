{-# LANGUAGE CPP #-}

#include "MachDeps.h"
#define HDLSYN Other

module BenchmarkCommon where

import Clash.Signal (VDomainConfiguration)

import Clash.Annotations.Primitive (HDL(VHDL))
import Clash.Annotations.BitRepresentation.Internal (CustomReprs, buildCustomReprs)
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
  (PreserveCase(..), HWMap, FilteredHWType, TopEntityT, topId)
import Clash.Primitives.Types

#if MIN_VERSION_ghc(9,0,0)
import GHC.Utils.Misc (OverridingBool(..))
#else
import Util (OverridingBool(..))
#endif

import qualified Control.Concurrent.Supply as Supply
import Control.Monad.State.Strict   (State)
import Data.HashMap.Strict          (HashMap)
import Data.IntMap.Strict           (IntMap)
import Data.Text                    (Text)

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
    , opt_clear=True
    , opt_errorExtra = True
    , opt_floatSupport = True
    , opt_importPaths=idirs
    , opt_specLimit=100 -- For "ManyEntitiesVaried"
    }

hdl :: HDL
hdl = VHDL

backend :: VHDLState
backend = initBackend WORD_SIZE_IN_BITS HDLSYN True PreserveCase Nothing (AggressiveXOptBB False) (RenderEnums True)

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
        ,HashMap Text VDomainConfiguration
        ,[Id]
        ,Id
        )
runInputStage idirs src = do
  pds <- primDirs backend
  (bindingsMap,tcm,tupTcm,topEntities,primMap,reprs,domainConfs) <- generateBindings (return ()) Auto pds idirs [] (hdlKind backend) src Nothing
  let topEntityNames = map topId topEntities
      tm = head topEntityNames
  return (bindingsMap,tcm,tupTcm,topEntities, primMap, buildCustomReprs reprs, domainConfs, topEntityNames,tm)

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
  (bindingsMap,tcm,tupTcm,topEntities,primMap,reprs,_domainConfs,topEntityNames,topEntity) <-
    runInputStage idirs src
  let opts1 = opts idirs
      transformedBindings =
        normalizeEntity reprs bindingsMap primMap tcm tupTcm typeTrans
          ghcEvaluator
          evaluator
          topEntityNames opts1 supplyN topEntity
  return (transformedBindings,topEntities,primMap,tcm,reprs,topEntity)
