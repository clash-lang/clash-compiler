{-# LANGUAGE CPP #-}

#include "MachDeps.h"
#define HDLSYN Other

import Clash.Driver
import Clash.Driver.Types
import Clash.GHC.Evaluator
import Clash.GHC.GenerateBindings
import Clash.GHC.NetlistTypes
import Clash.GHC.LoadModules (ghcLibDir)
import Clash.Backend
import Clash.Backend.SystemVerilog
import Clash.Backend.VHDL
import Clash.Backend.Verilog
import Clash.Netlist.BlackBox.Types
import Clash.Annotations.BitRepresentation.Internal (buildCustomReprs)
import Clash.Util

import Control.DeepSeq
import Control.Exception (finally)
import qualified Data.Time.Clock as Clock
import qualified Data.HashMap.Strict as HM
import System.Directory (removeDirectoryRecursive)

import GHC.Stack (HasCallStack)
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
  tmpDir <- createTemporaryClashDirectory

  finally (do
    startTime <- Clock.getCurrentTime
    pd      <- primDirs b
    (bindingsMap,tcm,tupTcm,topEntities,primMap,reprs) <- generateBindings tmpDir Auto pd ["."] (hdlKind b) src Nothing
    prepTime <- startTime `deepseq` bindingsMap `deepseq` tcm `deepseq` reprs `deepseq` Clock.getCurrentTime
    let prepStartDiff = reportTimeDiff prepTime startTime
    putStrLn $ "Loading dependencies took " ++ prepStartDiff

    -- Parse primitives:
    startTime' <- Clock.getCurrentTime
    topDir     <- ghcLibDir
    primMap2   <- sequence $ HM.map (sequence . fmap (compilePrimitive ["."] [] topDir)) primMap
    prepTime'  <- startTime `deepseq` primMap2 `seq` Clock.getCurrentTime
    let prepStartDiff' = reportTimeDiff prepTime' startTime'
    putStrLn $ "Parsing primitives took " ++ prepStartDiff'

    generateHDL (buildCustomReprs reprs) bindingsMap (Just b) primMap2 tcm tupTcm
      (ghcTypeToHWType WORD_SIZE_IN_BITS True) reduceConstant topEntities
      (ClashOpts 20 20 15 0 DebugNone False True True Auto WORD_SIZE_IN_BITS
        Nothing tmpDir HDLSYN True True ["."] Nothing True True False Nothing)
      (startTime,prepTime)
   ) (do
    removeDirectoryRecursive tmpDir
   )


main :: IO ()
main = genVHDL "./examples/FIR.hs"
