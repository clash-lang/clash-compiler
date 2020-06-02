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
 where
  findBinding name (bm, tcm, ids) =
    case List.find byName (eltsVarEnv bm) of
      Just bd ->
        fst3 $ nf ghcEvaluator bm (mempty, 0)
          tcm emptyInScopeSet ids (bindingTerm bd)

      Nothing ->
        error ("No entity in module: " <> show name)
   where
    fst3 (x, _, _) = x
    byName b = name == nameOcc (varName $ bindingId b)

mainVHDL :: IO ()
mainVHDL = mainCommon SVHDL

mainVerilog :: IO ()
mainVerilog = mainCommon SVerilog

mainSystemVerilog :: IO ()
mainSystemVerilog = mainCommon SSystemVerilog

