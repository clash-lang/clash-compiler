{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- Case alternatives should be selected by the partial evalautor when they
-- can be statically determined. This means we need to show that each type of
-- pattern can be correctly identified (Data, Literal, Default). In these tests
-- we give top entities which
--
--  * syntactically start with a case when translated to core
--  * have a statically known scrutinee, so an alternative should be selected
--
-- and check that the partial evaluator selects the correct branch (which
-- will is a list of the first 10 Catalan numbers).
--
module KnownCase where

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
import Clash.Driver.Types

import Clash.GHC.PartialEval

import Test.Tasty.Clash
import Test.Tasty.Clash.CoreTest

{-# INLINE matchedAlt #-}
matchedAlt :: [Integer]
matchedAlt = [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862]

{-# NOINLINE caseOfData #-}
{-# ANN caseOfData (Synthesize
          { t_name   = "caseOfData"
          , t_inputs = []
          , t_output = PortName "res"
          })
  #-}
caseOfData :: [Integer]
caseOfData = maybe [] (const matchedAlt) (Just 0)

{-# NOINLINE caseOfLit #-}
{-# ANN caseOfLit (Synthesize
          { t_name   = "caseOfLit"
          , t_inputs = []
          , t_output = PortName "res"
          })
  #-}
caseOfLit :: [Integer]
caseOfLit =
  case (3 :: Integer) of
    2 -> [1, 1, 2, 3, 5, 8, 13, 21, 34, 55]
    3 -> matchedAlt
    _ -> []

{-# NOINLINE caseOfDefault #-}
{-# ANN caseOfDefault (Synthesize
          { t_name   = "caseOfDefault"
          , t_inputs = []
          , t_output = PortName "res"
          })
  #-}
caseOfDefault :: [Integer]
caseOfDefault =
  case 'X' of
    _ -> matchedAlt

testPath :: FilePath
testPath = "tests/shouldwork/PartialEvaluation/KnownCase.hs"

mainCommon
  :: (Backend (TargetToState target))
  => SBuildTarget target
  -> IO ()
mainCommon hdl = do
  entities <- runToCoreStage hdl id testPath

  let alt  = findBinding "KnownCase.matchedAlt" entities
      just = findBinding "KnownCase.caseOfData" entities
      lit  = findBinding "KnownCase.caseOfLit" entities
      def  = findBinding "KnownCase.caseOfDefault" entities

  if aeqTerm (asTerm just) (asTerm alt)
     then pure ()
     else error ("Not alpha equivalent: " <> show just <> "\n\n" <> show alt)

  if aeqTerm (asTerm lit) (asTerm alt)
     then pure ()
     else error ("Not alpha equivalent: " <> show lit <> "\n\n" <> show alt)

  if aeqTerm (asTerm def) (asTerm alt)
     then pure ()
     else error ("Not alpha equivalent: " <> show def <> "\n\n" <> show alt)
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

