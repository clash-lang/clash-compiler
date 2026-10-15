{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.ANF"
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.ANF (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.DataCon (DataCon)
import Clash.Core.Name (NameSort (User), noSrcSpan)
import Clash.Core.Term
  ( Bind (..), IsMultiPrim (SingleResult), PrimInfo (..)
  , PrimUnfolding (NoUnfolding), Term (..), WorkInfo (WorkVariable), mkApps )
import Clash.Core.Type (mkFunTy)
import Clash.Core.Var (Id, Var (varUniq))
import Clash.Core.VarEnv (emptyInScopeSet)
import Clash.Normalize.Transformations.ANF (makeANF, nonRepANF)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.Combinators ((>->))
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteState (..))
import Clash.Unique (Unique)
import Clash.Util.Supply (Supply, newSupply)

import Test.Clash.Rewrite
  ( assertAlphaEq, assertNoFreeLocals, assertNoShadowing, globalId, inScopeOf
  , intFunTy, intId
  , intLit, intTy, letBinders, mkDataCon, parseTyConTy, runSingleTransformation
  , showPprU )

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

-- * makeANF

-- | Global function @k :: Int -> Int -> Int@
kId :: Id
kId = globalId User "k" 33 (intFunTy 2)

-- | Global function @h :: Int -> Int@
hId :: Id
hId = globalId User "h" 50 (intFunTy 1)

xId :: Id
xId = intId User "x" 31

-- | @k (h x) (let b = 3 in b)@, where @b@ has the given unique. ANF let-binds
-- @h x@ and lifts @b@ into the same let-expression.
anfTerm :: Unique -> Term
anfTerm bUniq =
  mkApps (Var kId)
    [ Left (App (Var hId) (Var xId))
    , Left (Letrec [(b, intLit 3)] (Var b)) ]
 where
  b = intId User "b" bUniq

-- | Run 'makeANF' on a term, with the given unique supply
runANF :: Supply -> Term -> IO Term
runANF supply =
  runSingleTransformation def def{_uniqSupply = supply} (inScopeOf [xId]) makeANF

-- | ANF creates new let-binders and moves all existing let-binders into a
-- single let-expression. It used to pick uniques for the new binders that only
-- avoided the free variables, not the bound variables of the expression, so a
-- new binder could get the same unique as an existing one.
--
-- To provoke this, we first run ANF to find out which unique the binder for
-- @h x@ gets, and then run it again, with the same unique supply, on a term in
-- which the existing let-binder has exactly that unique.
--
-- https://github.com/clash-lang/clash-compiler/commit/63827c31fff4ab27005f25cb027969f8805d646d
anfDoesNotDuplicateBinders :: Assertion
anfDoesNotDuplicateBinders = do
  supply <- newSupply
  res1 <- runANF supply (anfTerm 1000)
  appArg <- case filter ((/= 1000) . varUniq) (letBinders res1) of
    [i] -> pure i
    _ -> assertFailure ("ANF did not let-bind h x:\n" <> showPprU res1)
  res2 <- runANF supply (anfTerm (varUniq appArg))
  assertEqual ("Number of let-binders in:\n" <> showPprU res2)
    2 (length (letBinders res2))
  assertNoShadowing res2

-- | ANF moves all let-bindings it creates to the root of the term. It used to
-- also let-bind expressions referring to local variables that would not end up
-- in scope there, introducing free variables. Today, ANF relies on
-- 'nonRepANF', which runs right before it, to turn non-representable arguments
-- of primitives and constructors, such as lambdas, into applications of new
-- global functions. This checks that the two together don't let-bind @h z@ in
-- @p (\\z -> k (h z) z)@ outside of the lambda binding @z@.
--
-- https://github.com/clash-lang/clash-compiler/commit/36d60f54bf90f03b0cbf2d291f6411252c86dc7f
anfDoesNotLiftOutOfLambdas :: Assertion
anfDoesNotLiftOutOfLambdas = do
  supply <- newSupply
  let p = PrimInfo
        { primName = "p"
        , primType = mkFunTy (intFunTy 1) intTy
        , primWorkInfo = WorkVariable
        , primMultiResult = SingleResult
        , primUnfolding = NoUnfolding }
      z = intId User "z" 71
      cf = globalId User "top" 42 (intFunTy 1)
      tm = App (Prim p) (Lam z (mkApps (Var kId) [Left (App (Var hId) (Var z)), Left (Var z)]))
      st = def{_curFun = (cf, noSrcSpan), _uniqSupply = supply}
  runSingleTransformation def st emptyInScopeSet (nonRepANFR >-> makeANF) tm
    >>= assertNoFreeLocals

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
    , testCase "ANF does not duplicate binders"
        anfDoesNotDuplicateBinders
    , testCase "ANF does not lift expressions out of lambdas"
        anfDoesNotLiftOutOfLambdas
    ]
