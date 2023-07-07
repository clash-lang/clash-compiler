{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module exposes a function for running the clash compiler as far as
-- the netlist stage, and returns the netlist AST before it is serialized.
--
-- This module is somewhat of a nasty hack (inspired by clash-benchmark) as
-- clash-lib currently lacks a suitable top-level API for running the compiler
-- only as far as a required stage.
--
module Test.Tasty.Clash.NetlistTest
  ( runToNetlistStage
  , TargetToState
  ) where

import qualified Prelude as P
import           Clash.Prelude hiding (Type)

import           Clash.Annotations.Primitive (HDL(..))
import           Clash.Backend as Backend
import           Clash.Backend.SystemVerilog
import           Clash.Backend.VHDL
import           Clash.Backend.Verilog
import           Clash.Core.Name
import           Clash.Core.Term (Term)
import           Clash.Core.Var
import           Clash.Core.VarEnv
import           Clash.Driver as Driver
import           Clash.Driver.Types

import           Clash.GHC.PartialEval
import           Clash.GHC.Evaluator
import           Clash.GHC.GenerateBindings
import           Clash.GHC.NetlistTypes
import           Clash.Netlist
import           Clash.Netlist.Types hiding (backend, hdlDir)

import qualified Control.Concurrent.Supply as Supply
import           Control.DeepSeq (force)
import           Data.Maybe
import qualified Data.Map.Ordered as OMap
import qualified Data.Text as Text
import           System.FilePath ((</>))

import           Test.Tasty.Clash

mkClashOpts :: ClashOpts
mkClashOpts = defClashOpts
  { opt_cachehdl     = False
  , opt_errorExtra   = True
  }

type family TargetToState (target :: HDL) where
  TargetToState 'VHDL          = VHDLState
  TargetToState 'Verilog       = VerilogState
  TargetToState 'SystemVerilog = SystemVerilogState

runToNetlistStage
  :: forall target
   . (Backend (TargetToState target))
  => SBuildTarget target
  -- ^ Singleton for the build target
  -> (ClashOpts -> ClashOpts)
  -- ^ Function to modify the default clash options
  -> FilePath
  -- ^ Module to load
  -> IO [(ComponentMeta, Component)]
runToNetlistStage target f src = do
  pds <- primDirs backend
  (env, design) <- generateBindings opts (return ()) pds (opt_importPaths opts) [] (hdlKind backend) src Nothing

  let (compNames, initIs) = genTopNames opts hdl (designEntities design)
      teNames = fmap topId (designEntities design)
      tes2    = mkVarEnv (P.zip (P.map topId (designEntities design)) (designEntities design))

  supplyN <- Supply.newSupply

  let transformAndGenNetlist te = do
        transformedBindings <-
          normalizeEntity env (designBindings design)
            (ghcTypeToHWType (opt_intWidth opts))
            ghcEvaluator
            evaluator
            teNames supplyN te
        netlistFrom (env, transformedBindings, tes2, compNames, initIs, designDomains design) te
  netlists <- mapM (fmap (\(_,x,_) -> force (P.map snd (OMap.assocs x))) . transformAndGenNetlist) teNames
  return $ P.concat netlists
 where
  opts = f mkClashOpts
  backend = initBackend @(TargetToState target) opts
  hdl = buildTargetToHdl target

  netlistFrom :: (ClashEnv, BindingMap, VarEnv TopEntityT,
                       VarEnv Identifier, IdentifierSet,
                       Backend.DomainMap)
                      -> Var Term
                      -> IO (Component, ComponentMap, IdentifierSet)
  netlistFrom (env, bm, tes, compNames, seen, domainConfs) te =
    genNetlist env False bm tes compNames (ghcTypeToHWType (opt_intWidth opts))
      ite (SomeBackend hdlSt) seen hdlDir Nothing te
   where
    teS     = Text.unpack . nameOcc $ varName te
    modN    = takeWhile (/= '.') teS
    hdlSt   = setDomainConfigurations domainConfs
            $ setModName (Text.pack modN) backend
    ite     = ifThenElseExpr hdlSt
    hdlDir  = fromMaybe "." (opt_hdlDir opts)
      </> Backend.name hdlSt
      </> takeWhile (/= '.') teS
