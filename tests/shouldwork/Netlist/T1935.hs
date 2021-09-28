{-# LANGUAGE OverloadedStrings #-}
module T1935 where

import qualified Prelude as P

import Clash.Prelude

import Clash.Netlist.Types
import Clash.Backend (Backend)

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

import Control.Monad (when)

topEntity
  :: Clock System
  -> Reset System
  -> Signal System (Unsigned 8)
topEntity clk rst = withClockResetEnable clk rst enableGen x
  where
    x :: SystemClockResetEnable => Signal System (Unsigned 8)
    x = register 4 (x+1)

testPath :: FilePath
testPath = "tests/shouldwork/Netlist/T1935.hs"

countRegisters :: Component -> Int
countRegisters (Component _nm _inps _outs ds) =
  let regs = filter isRegister ds
   in P.length regs
 where
  isRegister (BlackBoxD nm _ _ _ _ _)
    | nm == "Clash.Signal.Internal.register#" = True
  isRegister _ = False

mainGeneric :: Backend (TargetToState target) => SBuildTarget target -> IO ()
mainGeneric hdl = do
  netlist <- runToNetlistStage hdl id testPath
  let regs = sum $ fmap (countRegisters . snd) netlist
  when (regs /= 1) $ error ("Expected 1 register, but found: " <> show regs)

mainVHDL :: IO ()
mainVHDL = mainGeneric SVHDL

mainVerilog :: IO ()
mainVerilog = mainGeneric SVerilog

mainSystemVerilog :: IO ()
mainSystemVerilog = mainGeneric SSystemVerilog
