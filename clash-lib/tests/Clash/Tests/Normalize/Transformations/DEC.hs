{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.DEC"
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.DEC (tests) where

import Data.Default (def)
import Data.List (find)
import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (IntLiteral))
import Clash.Core.Name (Name (..), NameSort (User))
import Clash.Core.Term (Pat (..), Term (..), mkApps)
import Clash.Core.Var (Id, Var (..))
import Clash.Normalize.Transformations.DEC (disjointExpressionConsolidation)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteState (..))
import Clash.Unique (Unique)
import Clash.Util.Supply (Supply, newSupply)

import Test.Clash.Rewrite
  ( assertNoShadowing, globalId, inScopeOf, intFunTy, intId, intLit, intTy
  , letBinders, mkBindingMap, runSingleTransformation, showPprU )

decR :: NormRewrite
decR = $(asRewriteQ disjointExpressionConsolidation)

sId, xId, yId :: Id
sId = intId User "s" 30
xId = intId User "x" 31
yId = intId User "y" 32

-- | Global function @f :: Int -> Int@, with body @\\a -> a@
fId :: Id
fId = globalId User "f" 10 (intFunTy 1)

-- | Global function @k :: Int -> Int -> Int@
kId :: Id
kId = globalId User "k" 33 (intFunTy 2)

-- | @case s of { DEFAULT -> f y; 1 -> let b = 3 in k b (f x) }@, where @b@ has
-- the given unique. DEC lifts out @f@, and substitutes @f_out@ for its
-- applications in both alternatives.
decTerm :: Unique -> Term
decTerm bUniq =
  Case (Var sId) intTy
    [ (DefaultPat, App (Var fId) (Var yId))
    , ( LitPat (IntLiteral 1)
      , Letrec [(b, intLit 3)]
          (mkApps (Var kId) [Left (Var b), Left (App (Var fId) (Var xId))]))
    ]
 where
  b = intId User "b" bUniq

-- | Run DEC on a term, with the given unique supply
runDEC :: Supply -> Term -> IO Term
runDEC supply = runSingleTransformation def st (inScopeOf [sId, xId, yId]) decR
 where
  st = def{_bindings = mkBindingMap [(fId, Lam a (Var a))], _uniqSupply = supply}
  a = intId User "a" 20

-- | DEC substitutes a reference to the lifted @f_out@ for every application of
-- @f@ in the alternatives, which puts that reference under the binders of the
-- alternatives. It used to pick the unique of @f_out@ avoiding only the
-- variables in scope of the case-expression, not the binders inside the
-- alternatives, so a let-binder in an alternative could capture the reference.
--
-- To provoke this, we first run DEC to find out which unique @f_out@ gets, and
-- then run it again, with the same unique supply, on a term in which the
-- let-binder in the alternative has exactly that unique.
--
-- https://github.com/clash-lang/clash-compiler/pull/1317
-- https://github.com/clash-lang/clash-compiler/commit/959ee9072fdd0c5deaf91f5e82adc2c23b0f8e16
decDoesNotCaptureLiftedExpression :: Assertion
decDoesNotCaptureLiftedExpression = do
  supply <- newSupply
  res1 <- runDEC supply (decTerm 1000)
  fOut <- case find (Text.isSuffixOf "f_out" . nameOcc . varName) (letBinders res1) of
    Just i -> pure i
    Nothing -> assertFailure ("DEC did not lift out f:\n" <> showPprU res1)
  runDEC supply (decTerm (varUniq fOut)) >>= assertNoShadowing

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.DEC"
    [ testCase "DEC does not capture lifted expressions (#1317)"
        decDoesNotCaptureLiftedExpression
    ]
