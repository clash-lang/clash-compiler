{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

#include "MachDeps.h"

module Test.Tasty.Clash.CoreTest
  ( runToCoreStage
  ) where

import Clash.Backend
import Clash.Backend.SystemVerilog
import Clash.Backend.Verilog
import Clash.Backend.VHDL
import Clash.Core.TyCon
import Clash.Driver.Types
import Clash.GHC.GenerateBindings
import Clash.Netlist.BlackBox.Types (HdlSyn(Other))

import Util

import Test.Tasty.Clash

type family TargetToState (target :: BuildTarget) where
  TargetToState 'SystemVerilog = SystemVerilogState
  TargetToState 'VHDL          = VHDLState
  TargetToState 'Verilog       = VerilogState

mkClashOpts :: ClashOpts
mkClashOpts = defClashOpts
  { opt_cachehdl     = False
  , opt_errorExtra   = True
  , opt_floatSupport = True
  }

mkBackend
  :: (Backend (TargetToState target))
  => SBuildTarget target
  -> TargetToState target
mkBackend _ = initBackend WORD_SIZE_IN_BITS Other True Nothing

-- Run clash as far as having access to core for all bindings. This is used
-- to test operations on core, such as transformations and evaluation.
--
-- TODO This is somewhat of a hack. Ideally, Clash.Driver would provide a means
-- to run the compiler up to a given stage. There are currently numerous
-- problems standing in the way of this however.
--
runToCoreStage
  :: (Backend (TargetToState target))
  => SBuildTarget target
  -> (ClashOpts -> ClashOpts)
  -> FilePath
  -> IO (BindingMap, TyConMap)
runToCoreStage target f src = do
  pds <- primDirs backend
  (bm, tcm, _, _, _, _) <- generateBindings
    Auto pds (opt_importPaths opts) [] (hdlKind backend) src Nothing

  return (bm, tcm)
 where
  backend = mkBackend target
  opts = f mkClashOpts

