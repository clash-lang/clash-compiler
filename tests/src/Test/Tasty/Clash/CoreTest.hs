{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Tasty.Clash.CoreTest
  ( TargetToState
  , runToCoreStage
  ) where

import Clash.Annotations.Primitive (HDL(..))
import Clash.Backend
import Clash.Backend.SystemVerilog
import Clash.Backend.Verilog
import Clash.Backend.VHDL
import Clash.Driver.Types
import Clash.Util.Supply

import Clash.GHC.GenerateBindings

import Test.Tasty.Clash

type family TargetToState (target :: HDL) where
  TargetToState 'SystemVerilog = SystemVerilogState
  TargetToState 'VHDL          = VHDLState
  TargetToState 'Verilog       = VerilogState

mkClashOpts :: ClashOpts
mkClashOpts = defClashOpts
  { opt_cachehdl     = False
  , opt_errorExtra   = True
  }

-- Run clash as far as having access to core for all bindings. This is used
-- to test operations on core, such as transformations and evaluation.
--
-- NOTE This does /not/ do any normalization!
-- It only runs GHC and translates GHC Core to Clash Core.
--
-- TODO This is somewhat of a hack. Ideally, Clash.Driver would provide a means
-- to run the compiler up to a given stage. There are currently numerous
-- problems standing in the way of this however.
--
runToCoreStage
  :: forall target
   . (Backend (TargetToState target))
  => SBuildTarget target
  -> (ClashOpts -> ClashOpts)
  -> FilePath
  -> IO (ClashEnv, ClashDesign, Supply)
runToCoreStage _target f src = do
  ids <- newSupply
  pds <- primDirs backend
  (env, design) <- generateBindings opts (return ()) pds (opt_importPaths opts) [] (hdlKind backend) src Nothing

  return (env, design, ids)
 where
  opts = f mkClashOpts
  backend = initBackend @(TargetToState target) opts
