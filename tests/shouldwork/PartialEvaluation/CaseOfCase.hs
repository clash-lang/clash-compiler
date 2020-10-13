{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- The case-of-case optimization should be performed by the partial evaluator
-- when evaluating terms. This means that expressions like
--
--   case (case x of A -> a ; B -> b) of C -> c ; D -> d
--
-- should be safely converted to
--
--   case x of
--     A -> case a of
--            C -> c
--            D -> d
--
--     B -> case b of
--            C -> c
--            D -> d
--
module CaseOfCase where

import Clash.Prelude

import Clash.Backend
import Clash.Core.PartialEval
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

data Foo = A | B
data Bar = C | D

topEntity :: Foo -> Bar -> Bar -> Integer
topEntity x a b =
  case go x of
    C -> 0
    D -> 1
 where
  {-# NOINLINE go #-}
  go y = case y of
         A -> a
         B -> b

testPath :: FilePath
testPath = "tests/shouldwork/PartialEvaluation/CaseOfCase.hs"

checkAlts :: Id -> Id -> [(Pat, Term)] -> IO ()
checkAlts a b alts
  | [(aP, aA), (bP, bA)] <- alts
  = checkAlt a aP aA >> checkAlt b bP bA

  | otherwise
  = error ("Expected two patterns [A,B], got " <> show alts)
 where
  checkAlt var (DataPat dc [] []) alt
    | Case s _ as <- alt
    , Var v <- s
    , v == var
    = pure ()

  checkAlt _ _ alt =
    error ("Expected inner case expression, got " <> show alt)

mainCommon
  :: (Backend (TargetToState target))
  => SBuildTarget target
  -> IO ()
mainCommon hdl = do
  entities <- runToCoreStage hdl id testPath
  te <- findBinding "CaseOfCase.topEntity" entities

  if |  Lam x (Lam a (Lam b e)) <- te
     ,  Case s _ alts <- e
     ,  Var v <- s
     ,  v == x
     -> checkAlts a b alts

     |  otherwise
     -> error ("case-of-case transformation not applied")

mainVHDL :: IO ()
mainVHDL = mainCommon SVHDL

mainVerilog :: IO ()
mainVerilog = mainCommon SVerilog

mainSystemVerilog :: IO ()
mainSystemVerilog = mainCommon SSystemVerilog

