{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module T1041 where

import Prelude as P

import Clash.Prelude
import Clash.Netlist.Types
import qualified Clash.Netlist.Id as Id

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

data VGASync dom = VGASync
    { vgaHSync :: Signal dom (Unsigned 8)
    , vgaVSync :: Signal dom (Unsigned 8)
    , vgaDE    :: Signal dom (Unsigned 8)
    }

data VGAOut dom = VGAOut
    { vgaSync  :: VGASync dom
    , vgaR     :: Signal dom (Unsigned 8)
    , vgaG     :: Signal dom (Unsigned 8)
    , vgaB     :: Signal dom (Unsigned 8)
    }

{-# ANN getVgaOut
   (Synthesize
     { t_name   = "Pattern"
     , t_inputs =
        [ PortName "CLK_25MHZ"
        , PortName "RESET"
        ]
     , t_output =
        PortProduct "VGA"
          [ PortProduct ""
            [ PortName "HSYNC"
            , PortName "VSYNC"
            , PortName "DE"
            ]
          , PortName "RED"
          , PortName "GREEN"
          , PortName "BLUE"
          ]
     }) #-}
getVgaOut :: Clock System -> Reset System -> VGAOut System
getVgaOut clk rst = VGAOut{..}
 where
  vgaSync = VGASync{..}
   where
    vgaHSync = pure 0
    vgaVSync = pure 1
    vgaDE = pure 2

  vgaR = pure 3
  vgaG = pure 4
  vgaB = pure 5

testPath :: FilePath
testPath = "tests/shouldwork/Naming/T1041.hs"

assertOneVGA :: Component -> IO ()
assertOneVGA (Component _ _ _ ds _ _ _)
  | vgas == 1 = pure ()
  | otherwise = error $ "Expected one declaration of VGA wire: got " <> show vgas
 where
  vgas = P.length (filter isVGADecl ds)

  -- Multiple cases as mkUniqueIdentifier Basic
  -- in VHDL changes names to be lowercase.
  --
  isVGADecl (NetDecl' _ (Id.toText -> "vga") _ _) = True
  isVGADecl (NetDecl' _ (Id.toText -> "VGA") _ _) = True
  isVGADecl _ = False

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL id testPath
  mapM_ assertOneVGA netlist

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog id testPath
  mapM_ assertOneVGA netlist

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  netlist <- runToNetlistStage SSystemVerilog id testPath
  mapM_ assertOneVGA netlist
