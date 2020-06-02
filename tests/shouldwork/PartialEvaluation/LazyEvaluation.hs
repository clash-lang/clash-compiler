{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module LazyEvaluation where

import qualified Data.List as List (find)

import Clash.Prelude

import Clash.Backend
import Clash.Core.Evaluator.Models
import Clash.Core.Name
import Clash.Core.Subst
import Clash.Core.Term
import Clash.Core.TyCon
import Clash.Core.Var
import Clash.Core.VarEnv
import Clash.Debug
import Clash.Driver.Types

import Clash.GHC.PartialEval

import Test.Tasty.Clash
import Test.Tasty.Clash.CoreTest

topEntity :: Integer -> Integer
topEntity = flip const nats
 where
  nats :: [Integer]
  nats = 0 : fmap succ nats

testPath :: FilePath
testPath = "tests/shouldwork/PartialEvaluation/LazyEvaluation.hs"

mainCommon
  :: (Backend (TargetToState target))
  => SBuildTarget target
  -> IO ()
mainCommon hdl = do
  entities <- runToCoreStage hdl id testPath
  let te = findBinding "LazyEvaluation.topEntity" entities

  if |  NLam i (NNeu (NeVar j)) <- te
     ,  i == j
     -> pure ()

     |  otherwise
     -> error ("Evaluation was not lazy: " <> show te)

mainVHDL :: IO ()
mainVHDL = mainCommon SVHDL

mainVerilog :: IO ()
mainVerilog = mainCommon SVerilog

mainSystemVerilog :: IO ()
mainSystemVerilog = mainCommon SSystemVerilog

