{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main where

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
import Clash.Util

import Control.DeepSeq
import Data.Proxy
import qualified Data.Time.Clock as Clock
import GHC.Stack (HasCallStack)

genSystemVerilog
  :: ClashOpts
  -> String
  -> IO ()
genSystemVerilog = doHDL (Proxy @SystemVerilogState)

genVHDL
  :: ClashOpts
  -> String
  -> IO ()
genVHDL = doHDL (Proxy @VHDLState)

genVerilog
  :: ClashOpts
  -> String
  -> IO ()
genVerilog = doHDL (Proxy @VerilogState)

doHDL
  :: forall s
   . HasCallStack
  => Backend s
  => Proxy s
  -> ClashOpts
  -> String
  -> IO ()
doHDL Proxy opts src = do
  startTime <- Clock.getCurrentTime
  let backend = initBackend @s opts
  pd      <- primDirs backend
  (clashEnv, clashDesign) <-
    generateBindings opts (return ()) pd ["."] [] (hdlKind backend) src Nothing
  prepTime <- startTime `deepseq` designBindings clashDesign `deepseq` envTyConMap clashEnv `deepseq` envCustomReprs clashEnv `deepseq` Clock.getCurrentTime
  let prepStartDiff = reportTimeDiff prepTime startTime
  putStrLn $ "Loading dependencies took " ++ prepStartDiff

  generateHDL clashEnv clashDesign (Just backend)
    (ghcTypeToHWType (_opt_intWidth opts)) ghcEvaluator evaluator Nothing startTime

main :: IO ()
main =
  let opts = defClashOpts
               { _opt_cachehdl = False
               , _opt_debug = debugSilent
               , _opt_clear = True
               }
   in genVHDL opts "./examples/FIR.hs"
