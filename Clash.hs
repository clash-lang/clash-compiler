{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main where

#include "MachDeps.h"
#define HDLSYN Other

import Clash.Driver
import Clash.Driver.Types
import Clash.GHC.Evaluator
import Clash.GHC.GenerateBindings
import Clash.GHC.NetlistTypes
import Clash.GHC.PartialEval
import Clash.Backend
import Clash.Backend.SystemVerilog
import Clash.Backend.VHDL
import Clash.Backend.Verilog
import Clash.Netlist.BlackBox.Types
import Clash.Netlist.Types (PreserveCase(..))
import Clash.Annotations.BitRepresentation.Internal (buildCustomReprs)
import Clash.Util

import Control.DeepSeq
import qualified Data.Time.Clock as Clock
import GHC.Stack (HasCallStack)

#if MIN_VERSION_ghc(9,0,0)
import GHC.Utils.Misc (OverridingBool(..))
#else
import Util (OverridingBool(..))
#endif

genSystemVerilog
  :: String
  -> IO ()
genSystemVerilog = doHDL (initBackend WORD_SIZE_IN_BITS HDLSYN True PreserveCase Nothing (AggressiveXOptBB False) (RenderEnums True) :: SystemVerilogState)

genVHDL
  :: String
  -> IO ()
genVHDL = doHDL (initBackend WORD_SIZE_IN_BITS HDLSYN True PreserveCase Nothing (AggressiveXOptBB False) (RenderEnums True) :: VHDLState)

genVerilog
  :: String
  -> IO ()
genVerilog = doHDL (initBackend WORD_SIZE_IN_BITS HDLSYN True PreserveCase Nothing (AggressiveXOptBB False) (RenderEnums True) :: VerilogState)

doHDL
  :: HasCallStack
  => Backend s
  => s
  -> String
  -> IO ()
doHDL b src = do
  startTime <- Clock.getCurrentTime
  pd      <- primDirs b
  (bindingsMap,tcm,tupTcm,topEntities,primMap,reprs,domainConfs) <-
    generateBindings (return ()) Auto pd ["."] [] (hdlKind b) src Nothing
  prepTime <- startTime `deepseq` bindingsMap `deepseq` tcm `deepseq` reprs `deepseq` Clock.getCurrentTime
  let prepStartDiff = reportTimeDiff prepTime startTime
  putStrLn $ "Loading dependencies took " ++ prepStartDiff

  generateHDL (buildCustomReprs reprs) domainConfs bindingsMap (Just b) primMap tcm tupTcm
    (ghcTypeToHWType WORD_SIZE_IN_BITS) ghcEvaluator evaluator topEntities Nothing
    defClashOpts{opt_cachehdl = False, opt_debug = debugSilent, opt_clear = True}
    startTime

main :: IO ()
main = genVHDL "./examples/FIR.hs"
