{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.ANF"
-}

{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.ANF (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.DataCon (DataCon)
import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Bind (..), Term (..), mkApps)
import Clash.Normalize.Transformations.ANF (nonRepANF)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)

import Test.Clash.Rewrite
  ( assertAlphaEq, inScopeOf, intFunTy, intId, intTy, mkDataCon
  , parseTyConTy, runSingleTransformation )

nonRepANFR :: NormRewrite
nonRepANFR = $(asRewriteQ nonRepANF)

-- | @MkP :: Int -> (Int -> Int) -> P@
mkP :: DataCon
mkP = mkDataCon "MkP" 1001 [] [] [intTy, intFunTy 1] (parseTyConTy "P")

-- | @MkP x (let x = u in \\y -> x)@, with the first @x@ free. 'nonRepANF'
-- must not move the first argument under the let-binder @x@.
nonRepANFCapture :: IO Term
nonRepANFCapture =
  runSingleTransformation def def (inScopeOf [u, x]) nonRepANFR $
    mkApps (Data mkP)
      [ Left (Var x)
      , Left (Let (NonRec x (Var u)) (Lam y (Var x))) ]
 where
  u = intId User "u" 1
  x = intId User "x" 2
  y = intId User "y" 3

-- | The expected result of 'nonRepANFCapture'
nonRepANFCaptureExpected :: Term
nonRepANFCaptureExpected =
  Let (NonRec x1 (Var u))
    (mkApps (Data mkP) [Left (Var x), Left (Lam y (Var x1))])
 where
  u = intId User "u" 1
  x = intId User "x" 2
  y = intId User "y" 3
  x1 = intId User "x" 4

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.ANF"
    -- 'nonRepANF' moved the let-bindings out of a non-representable argument
    -- of a data constructor without renaming them. They could then capture
    -- free variables of the other arguments.
    -- https://github.com/clash-lang/clash-compiler/pull/1071
    [ testCase "nonRepANF deshadows let-bindings (#1071)" $
        nonRepANFCapture >>= assertAlphaEq nonRepANFCaptureExpected
    ]
