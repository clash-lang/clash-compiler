{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

#include "MachDeps.h"

-- | This module exposes a function for running the clash compiler as far as
-- the netlist stage, and returns the netlist AST before it is serialized.
--
-- This module is somewhat of a nasty hack (inspired by clash-benchmark) as
-- clash-lib currently lacks a suitable top-level API for running the compiler
-- only as far as a required stage.
--
module Test.Tasty.Clash.NetlistTest
  ( runToNetlistStage
  ) where

import qualified Prelude as P
import           Clash.Prelude

import           Clash.Annotations.Primitive (HDL(..))
import           Clash.Annotations.BitRepresentation.Internal
import           Clash.Backend as Backend
import           Clash.Backend.SystemVerilog
import           Clash.Backend.VHDL
import           Clash.Backend.Verilog
import           Clash.Core.Name
import           Clash.Core.TyCon
import           Clash.Core.Type
import           Clash.Core.Var
import           Clash.Core.VarEnv
import           Clash.Driver as Driver
import           Clash.Driver.Types

#if EXPERIMENTAL_EVALUATOR
import           Clash.GHC.PartialEval
#else
import           Clash.GHC.Evaluator
#endif

import           Clash.GHC.GenerateBindings
import           Clash.GHC.NetlistTypes
import           Clash.Netlist
import qualified Clash.Netlist.Id as Id
import           Clash.Netlist.BlackBox.Types (HdlSyn(Other))
import           Clash.Netlist.Types hiding (backend, hdlDir)
import           Clash.Util

import           Util

import qualified Control.Concurrent.Supply as Supply
import           Control.DeepSeq (force)
import           Control.Monad.State.Strict (State)
import           Data.Maybe
import qualified Data.Text as Text
import           System.FilePath ((</>))

import           Test.Tasty.Clash

typeTrans
  :: CustomReprs
  -> TyConMap
  -> Type
  -> State HWMap (Maybe (Either String FilteredHWType))
typeTrans = ghcTypeToHWType WORD_SIZE_IN_BITS True

mkClashOpts :: ClashOpts
mkClashOpts = defClashOpts
  { opt_cachehdl     = False
  , opt_errorExtra   = True
  , opt_floatSupport = True
  }

type family TargetToState (target :: HDL) where
  TargetToState 'VHDL          = VHDLState
  TargetToState 'Verilog       = VerilogState
  TargetToState 'SystemVerilog = SystemVerilogState

mkBackend
  :: (Backend (TargetToState target))
  => SBuildTarget target -> TargetToState target
mkBackend _ = initBackend WORD_SIZE_IN_BITS Other True Nothing (AggressiveXOptBB False)

runToNetlistStage
  :: (Backend (TargetToState target))
  => SBuildTarget target
  -- ^ Singleton for the build target
  -> (ClashOpts -> ClashOpts)
  -- ^ Function to modify the default clash options
  -> FilePath
  -- ^ Module to load
  -> IO [([Bool], SrcSpan, Id.IdentifierSet, Component)]
runToNetlistStage target f src = do
  pds <- primDirs backend
  (bm, tcm, tupTcm, tes, pm, rs, _)
    <- generateBindings Auto pds (opt_importPaths opts) [] (hdlKind backend) src Nothing

  let (compNames, initIs) = genTopNames Nothing True hdl tes
      teNames = fmap topId tes
      te      = topId (P.head tes)
      reprs   = buildCustomReprs rs
      tes2    = mkVarEnv (P.zip (P.map topId tes) tes)

  supplyN <- Supply.newSupply

  let transformedBindings = normalizeEntity reprs bm pm tcm tupTcm typeTrans
#if EXPERIMENTAL_EVALUATOR
          ghcEvaluator
#else
          evaluator
#endif
          teNames opts supplyN te

  fmap (\(_,x,_) -> force (eltsVarEnv x)) $ netlistFrom (transformedBindings, tcm, tes2, compNames, pm, reprs, te, initIs)
 where
  backend = mkBackend target
  opts = f mkClashOpts
  hdl = buildTargetToHdl target

  netlistFrom (bm, tcm, tes, compNames, pm, rs, te, seen) =
    genNetlist False opts rs bm tes compNames pm tcm typeTrans
      iw ite (SomeBackend hdlSt) seen hdlDir Nothing te
   where
    iw      = opt_intWidth opts
    teS     = Text.unpack . nameOcc $ varName te
    modN    = takeWhile (/= '.') teS
    hdlSt   = setModName (Text.pack modN) backend
    ite     = ifThenElseExpr hdlSt
    hdlDir  = fromMaybe "." (opt_hdlDir opts)
      </> Backend.name hdlSt
      </> takeWhile (/= '.') teS
