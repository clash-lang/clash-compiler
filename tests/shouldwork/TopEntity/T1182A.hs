{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module T1182A where

import qualified Prelude as P

import Clash.Prelude
import qualified Clash.Netlist.Ast.Type as N
import qualified Clash.Netlist.Types as N
import qualified Clash.Netlist.Id as Id
import Clash.Annotations.TH

import Clash.Class.HasDomain

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

import qualified Data.Text as T

data SevenSegment dom (n :: Nat) = SevenSegment
    { anodes :: "AN" ::: Signal dom (Vec n Bool) }

type instance TryDomain t (SevenSegment dom n) = Found dom

topEntity
    :: "CLK" ::: Clock System
    -> "SS" ::: SevenSegment System 8
topEntity clk = withClockResetEnable clk resetGen enableGen $
    SevenSegment{ anodes = pure $ repeat False }
makeTopEntity 'topEntity

testPath :: FilePath
testPath = "tests/shouldwork/TopEntity/T1182A.hs"

assertInputs :: N.HWType -> N.Component -> IO ()
assertInputs expType (N.Component _ [(clk,N.Clock _)] [(N.Wire,(ssan,actType),Nothing)] ds _ _ _)
  | Id.toText clk == T.pack "CLK"
  , Id.toText ssan == T.pack "SS_AN"
  = pure ()
assertInputs _ c = error $ "Component mismatch: " P.++ show c

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL id testPath
  mapM_ (assertInputs (N.BitVector 8)) netlist

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog id testPath
  mapM_ (assertInputs (N.Vector 8 N.Bool)) netlist

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  netlist <- runToNetlistStage SSystemVerilog id testPath
  mapM_ (assertInputs (N.BitVector 8)) netlist
