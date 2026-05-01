{-# LANGUAGE OverloadedStrings #-}

module T3276 where

import qualified Prelude as P

import Control.Monad (when)

import Clash.Backend (Backend)
import Clash.Explicit.Prelude
import Clash.Explicit.Testbench
import Clash.Netlist.Types

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

topEntity :: Signal System (Unsigned 8)
topEntity = cntr + x
 where
  cntr = register clk noReset enableGen 0 0
  x = register clk noReset enableGen 100 0
  done = (== 100) <$> cntr
  clk = tbClockGen $ not <$> done

testPath :: FilePath
testPath = "tests/shouldwork/Netlist/T3276.hs"

countBlackBoxes :: Component -> (Int, Int)
countBlackBoxes (Component _nm _inps _outs ds) =
  ( P.length (P.filter isTbClockGen ds)
  , P.length (P.filter isRegister ds)
  )
 where
  isTbClockGen (BlackBoxD nm _ _ _ _ _) =
    nm == "Clash.Signal.Internal.tbClockGen"
  isTbClockGen _ = False

  isRegister (BlackBoxD nm _ _ _ _ _) =
    nm == "Clash.Signal.Internal.register#"
  isRegister _ = False

mainGeneric :: Backend (TargetToState target) => SBuildTarget target -> IO ()
mainGeneric hdl = do
  netlist <- runToNetlistStage hdl id testPath
  let (tbClockGens, registers) =
        P.foldl addCounts (0, 0) (P.map (countBlackBoxes . snd) netlist)

  when (tbClockGens /= 1) $
    error ("Expected 1 tbClockGen, but found: " <> show tbClockGens)

  when (registers /= 2) $
    error ("Expected 2 registers, but found: " <> show registers)

mainVHDL :: IO ()
mainVHDL = mainGeneric SVHDL

mainVerilog :: IO ()
mainVerilog = mainGeneric SVerilog

mainSystemVerilog :: IO ()
mainSystemVerilog = mainGeneric SSystemVerilog

addCounts :: (Int, Int) -> (Int, Int) -> (Int, Int)
addCounts (a0, b0) (a1, b1) = (a0 + a1, b0 + b1)
