{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

#include "MachDeps.h"
#define HDLSYN Other

import Clash.Driver
import Clash.Driver.Types
import Clash.GHC.Evaluator
import Clash.GHC.GenerateBindings
import Clash.GHC.NetlistTypes
import Clash.Backend
import Clash.Backend.SystemVerilog
import Clash.Backend.VHDL
import Clash.Backend.Verilog
import Clash.Netlist.BlackBox.Types
import Clash.Annotations.BitRepresentation.Internal (buildCustomReprs)
import Clash.Util

import Control.DeepSeq
import qualified Data.Time.Clock as Clock

import Util (OverridingBool(..))

genSystemVerilog
  :: String
  -> IO ()
genSystemVerilog = doHDL (initBackend WORD_SIZE_IN_BITS HDLSYN True Nothing :: SystemVerilogState)

genVHDL
  :: String
  -> IO ()
genVHDL = doHDL (initBackend WORD_SIZE_IN_BITS HDLSYN True Nothing :: VHDLState)

genVerilog
  :: String
  -> IO ()
genVerilog = doHDL (initBackend WORD_SIZE_IN_BITS HDLSYN True Nothing :: VerilogState)

doHDL
  :: HasCallStack
  => Backend s
  => s
  -> String
  -> IO ()
doHDL b src = do
  startTime <- Clock.getCurrentTime
  pd      <- primDirs b
  (bindingsMap,tcm,tupTcm,topEntities,primMap,reprs) <- generateBindings Auto pd ["."] [] (hdlKind b) src Nothing
  prepTime <- startTime `deepseq` bindingsMap `deepseq` tcm `deepseq` reprs `deepseq` Clock.getCurrentTime
  let prepStartDiff = reportTimeDiff prepTime startTime
  putStrLn $ "Loading dependencies took " ++ prepStartDiff

  generateHDL (buildCustomReprs reprs) bindingsMap (Just b) primMap tcm tupTcm
    (ghcTypeToHWType WORD_SIZE_IN_BITS True) primEvaluator topEntities
    defClashOpts{opt_cachehdl = False, opt_dbgLevel = DebugSilent}
    (startTime,prepTime)

main :: IO ()
main = genVHDL "./examples/FIR.hs"
