{-# LANGUAGE CPP #-}

#include "MachDeps.h"
#define HDLSYN Other

import CLaSH.Driver
import CLaSH.Driver.Types
import CLaSH.Rewrite.Types
import CLaSH.GHC.Evaluator
import CLaSH.GHC.GenerateBindings
import CLaSH.GHC.NetlistTypes
import CLaSH.Backend
import CLaSH.Backend.SystemVerilog
import CLaSH.Backend.VHDL
import CLaSH.Backend.Verilog
import CLaSH.Netlist.BlackBox.Types

import Control.DeepSeq
import qualified Data.Time.Clock as Clock

genSystemVerilog :: String
                 -> IO ()
genSystemVerilog = doHDL (initBackend WORD_SIZE_IN_BITS HDLSYN :: SystemVerilogState)

genVHDL :: String
        -> IO ()
genVHDL = doHDL (initBackend WORD_SIZE_IN_BITS HDLSYN :: VHDLState)

genVerilog :: String
           -> IO ()
genVerilog = doHDL (initBackend WORD_SIZE_IN_BITS HDLSYN :: VerilogState)

doHDL :: Backend s
       => s
       -> String
       -> IO ()
doHDL b src = do
  startTime <- Clock.getCurrentTime
  pd      <- primDir b
  (bindingsMap,tcm,tupTcm,topEnt,testInpM,expOutM,primMap) <- generateBindings pd (hdlKind b) src Nothing
  prepTime <- startTime `deepseq` bindingsMap `deepseq` tcm `deepseq` Clock.getCurrentTime
  let prepStartDiff = Clock.diffUTCTime prepTime startTime
  putStrLn $ "Loading dependencies took " ++ show prepStartDiff
  generateHDL bindingsMap (Just b) primMap tcm tupTcm (ghcTypeToHWType WORD_SIZE_IN_BITS True) reduceConstant topEnt testInpM expOutM (CLaSHOpts 20 20 15 DebugFinal True WORD_SIZE_IN_BITS Nothing HDLSYN True True False) (startTime,prepTime)

main :: IO ()
main = genVHDL "./examples/FIR.hs"
