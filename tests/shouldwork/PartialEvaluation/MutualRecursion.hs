{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- TODO: When it is possible to completely unfold and evaluate these
-- in the evaluator and evaluate primops this test should be changed to
-- show that the PE can reduce the entity to a constant
--
module MutualRecursion where

import Control.DeepSeq
import Numeric.Natural

import Clash.Prelude

import Clash.Backend
import Clash.Core.Evaluator.Models
import Clash.Core.Literal
import Clash.Core.Subst
import Clash.Debug

import Clash.GHC.PartialEval

import Test.Tasty.Clash
import Test.Tasty.Clash.CoreTest

f :: Natural -> Natural
f 0 = 1
f n = n - m (f $ n-1)

m :: Natural -> Natural
m 0 = 0
m n = n - f (m $ n-1)

topEntity :: Natural
topEntity = f 10

testPath :: FilePath
testPath = "tests/shouldwork/PartialEvaluation/MutualRecursion.hs"

mainCommon
  :: (Backend (TargetToState target))
  => SBuildTarget target
  -> IO ()
mainCommon hdl = do
  entities <- runToCoreStage hdl id testPath

  -- TODO I can't think of a meaningful test until unrolling and primitive
  -- evaluation are implemented. The best that can be said right now is that
  -- normal forms can be obtained for f and m (meaning the evaluator doesn't
  -- attempt to unroll them indefinitely.

  let fNf = findBinding "MutualRecursion.f" entities
      mNf = findBinding "MutualRecursion.m" entities

  fNf `seq` mNf `seq` pure ()

mainVHDL :: IO ()
mainVHDL = mainCommon SVHDL

mainVerilog :: IO ()
mainVerilog = mainCommon SVerilog

mainSystemVerilog :: IO ()
mainSystemVerilog = mainCommon SSystemVerilog

