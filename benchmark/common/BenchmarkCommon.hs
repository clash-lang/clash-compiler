{-# LANGUAGE CPP #-}

#include "MachDeps.h"
#define HDLSYN Other

module BenchmarkCommon where

import Clash.Annotations.BitRepresentation.Internal (CustomReprs, buildCustomReprs)
import Clash.Annotations.TopEntity
import Clash.Backend
import Clash.Backend.VHDL
import Clash.Core.Name
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Type
import Clash.Driver
import Clash.Driver.Types
import Clash.GHC.GenerateBindings
import Clash.GHC.LoadModules (ghcLibDir)
import Clash.GHC.NetlistTypes
import Clash.Netlist.BlackBox.Types (HdlSyn(Other))
import Clash.Netlist.Types          (HWType)
import Clash.Primitives.Types

import Data.HashMap.Strict          (HashMap)
import Data.IntMap.Strict           (IntMap)

defaultTests :: [FilePath]
defaultTests =
  [ "examples/FIR.hs"
  , "examples/Queens.hs"
  , "benchmark/tests/BundleMapRepeat.hs"
  , "benchmark/tests/PipelinesViaFolds.hs"
  ]

typeTrans :: (CustomReprs -> HashMap TyConOccName TyCon -> Bool -> Type -> Maybe (Either String HWType))
typeTrans = ghcTypeToHWType WORD_SIZE_IN_BITS True

opts :: ClashOpts
opts = ClashOpts 20 20 15 0 DebugNone False True WORD_SIZE_IN_BITS Nothing HDLSYN True True ["."] Nothing

backend :: VHDLState
backend = initBackend WORD_SIZE_IN_BITS HDLSYN

runInputStage
  :: FilePath
  -> IO (BindingMap
        ,HashMap TyConOccName TyCon
        ,IntMap TyConName
        ,[(TmName, Type,Maybe TopEntity, Maybe TmName)]
        ,CompiledPrimMap
        ,CustomReprs
        ,[OccName Term]
        ,TmOccName
        )
runInputStage src = do
  pds <- primDirs backend
  (bindingsMap,tcm,tupTcm,topEntities,primMap,reprs) <- generateBindings pds ["."] (hdlKind backend) src Nothing
  let topEntityNames = map (\(x,_,_,_) -> nameOcc x) topEntities
      [(topEntity,_,_,_)] = topEntities
      tm = nameOcc topEntity
  topDir   <- ghcLibDir
  primMap2 <- sequence $ fmap (compilePrimitive [] topDir) primMap
  return (bindingsMap,tcm,tupTcm,topEntities, primMap2, buildCustomReprs reprs, topEntityNames,tm)
