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

import           Clash.Annotations.BitRepresentation.Internal
import           Clash.Backend as Backend
import           Clash.Backend.SystemVerilog
import           Clash.Backend.VHDL
import           Clash.Backend.Verilog
import           Clash.Core.Name
import           Clash.Core.TyCon
import           Clash.Core.Type
import           Clash.Core.Var
import           Clash.Driver as Driver
import           Clash.Driver.Types
import           Clash.GHC.Evaluator
import           Clash.GHC.GenerateBindings
import           Clash.GHC.NetlistTypes
import           Clash.Netlist
import           Clash.Netlist.BlackBox.Types (HdlSyn(Other))
import           Clash.Netlist.Types hiding (backend, hdlDir)
import           Clash.Util

import           Util

import qualified Control.Concurrent.Supply as Supply
import           Control.DeepSeq (force)
import           Control.Monad.State.Strict (State)
import qualified Control.Monad.State as State
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
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

type family TargetToState (target :: BuildTarget) where
  TargetToState 'VHDL          = VHDLState
  TargetToState 'Verilog       = VerilogState
  TargetToState 'SystemVerilog = SystemVerilogState

mkBackend
  :: (Backend (TargetToState target))
  => SBuildTarget target -> TargetToState target
mkBackend _ = initBackend WORD_SIZE_IN_BITS Other True Nothing

runToNetlistStage
  :: (Backend (TargetToState target))
  => SBuildTarget target
  -- ^ Singleton for the build target
  -> (ClashOpts -> ClashOpts)
  -- ^ Function to modify the default clash options
  -> FilePath
  -- ^ Module to load
  -> IO [([Bool], SrcSpan, HashMap Identifier Word, Component)]
runToNetlistStage target f src = do
  pds <- primDirs backend
  (bm, tcm, tupTcm, tes, pm, rs)
    <- generateBindings Auto pds (opt_importPaths opts) [] (hdlKind backend) src Nothing

  let teNames = fmap topId tes
      te      = topId (P.head tes)
      reprs   = buildCustomReprs rs

  supplyN <- Supply.newSupply

  let transformedBindings = normalizeEntity reprs bm pm tcm tupTcm typeTrans
          primEvaluator teNames opts supplyN te

  fmap (force . fst) $ netlistFrom (transformedBindings, tcm, tes, pm, reprs, te)
 where
  backend = mkBackend target
  opts = f mkClashOpts

  netlistFrom (bm, tcm, tes, pm, rs, te) =
    genNetlist False opts rs bm tes pm tcm typeTrans
      iw mkId1 extId ite (SomeBackend hdlSt) seen hdlDir prefixM te
   where
    iw      = opt_intWidth opts
    teS     = Text.unpack . nameOcc $ varName te
    modN    = takeWhile (/= '.') teS
    hdlSt   = setModName (Text.pack modN) backend
    mkId1   = State.evalState mkIdentifier hdlSt
    extId   = State.evalState extendIdentifier hdlSt
    prefixM = ComponentPrefix Nothing Nothing
    ite     = ifThenElseExpr hdlSt
    seen    = HashMap.empty
    hdlDir  = fromMaybe "." (opt_hdlDir opts)
      </> Backend.name hdlSt
      </> takeWhile (/= '.') teS
