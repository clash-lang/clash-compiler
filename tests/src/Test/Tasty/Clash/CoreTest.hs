{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

#include "MachDeps.h"

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
import Clash.Core.PartialEval
import Clash.Core.Name
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Driver.Types
import Clash.Netlist.BlackBox.Types (HdlSyn(Other))
import Clash.Netlist.Types (PreserveCase(..))

import Clash.GHC.GenerateBindings
import Clash.GHC.PartialEval

#if MIN_VERSION_ghc(9,0,0)
import           GHC.Utils.Misc
#else
import           Util
#endif

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

mkBackend
  :: (Backend (TargetToState target))
  => SBuildTarget target
  -> TargetToState target
mkBackend _ = initBackend WORD_SIZE_IN_BITS Other True PreserveCase Nothing (AggressiveXOptBB False) (RenderEnums True)

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
  -> IO (BindingMap, TyConMap, Supply)
runToCoreStage target f src = do
  ids <- newSupply
  pds <- primDirs backend
  (bm, tcm, _, _, _, _, _) <- generateBindings
    (return ()) Auto pds (opt_importPaths opts) [] (hdlKind backend) src Nothing

  return (bm, tcm, ids)
 where
  backend = mkBackend target
  opts = f mkClashOpts

findBinding
  :: OccName
  -> (BindingMap, TyConMap, Supply)
  -> IO Term
findBinding nm (bm, tcm, ids) =
  case List.find byName (eltsVarEnv bm) of
    Just bd ->
      let env = mkGlobalEnv bm tcm emptyInScopeSet ids 20 mempty 0
       in fst <$> nf ghcEvaluator env False (bindingId bd) (bindingTerm bd)

    Nothing -> error ("Not in binding map: " <> show nm)
 where
  byName b = nm == nameOcc (varName $ bindingId b)
