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
import Clash.Util

import Control.DeepSeq
import qualified Data.Time.Clock as Clock
import GHC.Stack (HasCallStack)

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
  let opts = defClashOpts { opt_cachehdl = False, opt_debug = debugSilent, opt_clear = True }
  (clashEnv, clashDesign) <-
    generateBindings opts (return ()) pd ["."] [] (hdlKind b) src Nothing
  prepTime <- startTime `deepseq` designBindings clashDesign `deepseq` envTyConMap clashEnv `deepseq` envCustomReprs clashEnv `deepseq` Clock.getCurrentTime
  let prepStartDiff = reportTimeDiff prepTime startTime
  putStrLn $ "Loading dependencies took " ++ prepStartDiff

  generateHDL clashEnv clashDesign (Just b)
    (ghcTypeToHWType WORD_SIZE_IN_BITS) ghcEvaluator evaluator Nothing startTime

main :: IO ()
main = genVHDL "./examples/FIR.hs"
