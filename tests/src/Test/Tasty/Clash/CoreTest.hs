{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Tasty.Clash.CoreTest
  ( TargetToState
  , runToCoreStage
  , findBinding
  ) where

import Control.Concurrent.Supply
import qualified Data.List as List (find)

import Clash.Annotations.Primitive (HDL(..))
import Clash.Backend
import Clash.Backend.SystemVerilog
import Clash.Backend.Verilog
import Clash.Backend.VHDL
import Clash.Core.Binding
import Clash.Core.PartialEval
import Clash.Core.Name
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Var
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types

import Clash.GHC.GenerateBindings
import Clash.GHC.PartialEval

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

findBinding
  :: OccName
  -> (BindingMap Term, TyConMap, Supply)
  -> IO Term
findBinding nm (bm, tcm, ids) =
  case List.find byName (UniqMap.elems bm) of
    Just bd ->
      let env = mkGlobalEnv bm tcm mempty ids 20 mempty 0
       in fst <$> nf ghcEvaluator env False (bindingId bd) (bindingTerm bd)

    Nothing -> error ("Not in binding map: " <> show nm)
 where
  byName b = nm == nameOcc (varName $ bindingId b)
