{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module T1182B where

import qualified Prelude as P

import Clash.Prelude
import qualified Clash.Netlist.Types as N
import qualified Clash.Netlist.Id as Id
import Clash.Annotations.TH

import Clash.Class.HasDomain

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

import qualified Data.Text as T

data SevenSegment dom (n :: Nat) = SevenSegment
    { anodes :: "AN" ::: Signal dom (Vec n Bool)
    , segments :: "SEG" ::: Signal dom (Vec 7 Bool)
    , dp :: "DP" ::: Signal dom Bool
    }

type instance TryDomain t (SevenSegment dom n) = Found dom

topEntity
    :: "CLK" ::: Clock System
    -> "SS" ::: SevenSegment System 8
topEntity clk = withClockResetEnable clk resetGen enableGen $
    SevenSegment{ anodes = pure $ repeat False
                , segments = pure $ repeat False
                , dp = pure False }
makeTopEntity 'topEntity

testPath :: FilePath
testPath = "tests/shouldwork/TopEntity/T1182B.hs"

assertInputs :: N.HWType-> N.HWType -> N.Component -> IO ()
assertInputs exp1 exp2 (N.Component _ [(clk, N.Clock _)]
  [ (N.Wire, (ssan, act1), Nothing)
  , (N.Wire, (ssseg, act2), Nothing)
  , (N.Wire, (ssdp, N.Bool), Nothing)
  ] ds)
  | Id.toText clk == T.pack "CLK"
  , Id.toText ssan == T.pack "SS_AN"
  , Id.toText ssseg == T.pack "SS_SEG"
  , Id.toText ssdp == T.pack "SS_DP"
  = pure ()
assertInputs _ _ c = error $ "Component mismatch: " P.++ show c

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL id testPath
  mapM_ (assertInputs (N.BitVector 8) (N.BitVector 7) . snd) netlist

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog id testPath
  mapM_ (assertInputs (N.Vector 8 N.Bool) (N.Vector 7 N.Bool) . snd) netlist

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  netlist <- runToNetlistStage SSystemVerilog id testPath
  mapM_ (assertInputs (N.BitVector 8) (N.BitVector 7) . snd) netlist
