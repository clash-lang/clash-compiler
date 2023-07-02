{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module T508 where

import Clash.Prelude

import Clash.Backend
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

data AB = A | B

ab :: KnownNat n => Index n -> AB -> AB
ab n A = if n >  0 then A else B
ab n B = if n == 0 then B else A
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE ab #-}

topEntity = ab @1
-- See: https://github.com/clash-lang/clash-compiler/pull/2511
{-# CLASH_OPAQUE topEntity #-}

testPath :: FilePath
testPath = "tests/shouldwork/Issues/T508.hs"

abIsConstant :: Component -> IO ()
abIsConstant (Component nm _ _ ds)
  | Id.toText nm == "ab" =
      case ds of
        [Assignment{}] -> pure ()
        _ -> error $ "Zero-width construct was not constant folded in: " <> show ds

  | otherwise = pure ()

mainCommon :: (Backend (TargetToState hdl)) => SBuildTarget hdl -> IO ()
mainCommon hdl = do
  netlist <- runToNetlistStage hdl id testPath
  mapM_ (abIsConstant . snd) netlist

mainVHDL :: IO ()
mainVHDL = mainCommon SVHDL

mainVerilog :: IO ()
mainVerilog = mainCommon SVerilog

mainSystemVerilog :: IO ()
mainSystemVerilog = mainCommon SSystemVerilog
