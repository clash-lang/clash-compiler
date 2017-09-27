{-# LANGUAGE CPP #-}

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
  (bindingsMap,tcm,tupTcm,topEntities,primMap) <- generateBindings pd ["."] (hdlKind b) src Nothing
  prepTime <- startTime `deepseq` bindingsMap `deepseq` tcm `deepseq` Clock.getCurrentTime
  let prepStartDiff = Clock.diffUTCTime prepTime startTime
  putStrLn $ "Loading dependencies took " ++ show prepStartDiff
  generateHDL bindingsMap (Just b) primMap tcm tupTcm (ghcTypeToHWType WORD_SIZE_IN_BITS True) reduceConstant topEntities
    (ClashOpts 20 20 15 0 DebugFinal False True WORD_SIZE_IN_BITS Nothing HDLSYN True True False ["."]) (startTime,prepTime)

main :: IO ()
main = genVHDL "./examples/FIR.hs"
