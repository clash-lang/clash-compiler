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
  ( Bind (..), IsMultiPrim (SingleResult), Pat (..), PrimInfo (..)
  , PrimUnfolding (NoUnfolding), Term (..), WorkInfo (WorkVariable), mkApps )
import Clash.Core.Type (Type (VarTy), mkFunTy)
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
  ( assertAlphaEq, assertNoFreeLocals, assertNoShadowing, assertWellScoped
  , globalId, inScopeOf, intFunTy, intId, intLit, intTy, letBinders, localId
  , mkDataCon, parseTyConTy, runSingleTransformation, showPprU, tyVar )

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

-- | A stand-in for @Signal@, see 'signalCons'
signalTy :: Type
signalTy = parseTyConTy "Sig"

-- | @(:-) :: Int -> Sig -> Sig@. ANF recognizes this constructor by its
-- name.
signalCons :: DataCon
signalCons =
  mkDataCon "Clash.Signal.Internal.:-" 1101 [] [] [intTy, signalTy] signalTy

-- | ANF used to leave case-expressions with an alternative for the @(:-)@
-- constructor of @Signal@ in place, but still let-bound the subterms of that
-- alternative outside of it. Here, it let-bound @h x@ outside of the
-- alternative binding @x@:
--
-- > \s -> case s of x :- xs -> k (h x) x
anfSignalCons :: Assertion
anfSignalCons = do
  supply <- newSupply
  runSingleTransformation def def{_uniqSupply = supply} emptyInScopeSet makeANF
    (Lam s (Case (Var s) intTy
      [ (DataPat signalCons [] [x, xs]
        , mkApps (Var kId) [Left (App (Var hId) (Var x)), Left (Var x)]) ]))
    >>= assertWellScoped
 where
  s = localId User "s" 1102 signalTy
  x = intId User "x" 1103
  xs = localId User "xs" 1104 signalTy

-- | ANF leaves alternatives whose pattern binds existential type variables in
-- place, but used to let-bind the subterms of these alternatives outside of
-- them. Here, it let-bound @h y@ outside of the alternative binding @y@:
--
-- > \e -> case e of MkE @m (x :: m) (y :: Int) -> k (h y) y
anfExistential :: Assertion
anfExistential = do
  supply <- newSupply
  runSingleTransformation def def{_uniqSupply = supply} emptyInScopeSet makeANF
    (Lam e (Case (Var e) intTy
      [ (DataPat mkE [m] [x, y]
        , mkApps (Var kId) [Left (App (Var hId) (Var y)), Left (Var y)]) ]))
    >>= assertWellScoped
 where
  m = tyVar User "m" 1201
  -- MkE :: forall m. m -> Int -> E
  mkE = mkDataCon "MkE" 1202 [] [m] [VarTy m, intTy] (parseTyConTy "E")
  e = localId User "e" 1203 (parseTyConTy "E")
  x = localId User "x" 1204 (VarTy m)
  y = intId User "y" 1205

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
    , testCase "ANF does not lift expressions out of Signal (:-) alternatives"
        anfSignalCons
    , testCase "ANF does not lift expressions out of existential alternatives"
        anfExistential
    ]
