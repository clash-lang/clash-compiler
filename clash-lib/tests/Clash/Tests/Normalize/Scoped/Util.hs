{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Util": the regression tests of
  "Clash.Tests.Normalize.Util", and agreement with "Clash.Normalize.Util" on
  converted inputs.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Normalize.Scoped.Util (tests) where

import qualified Control.Lens as Lens
import Control.Monad.State.Strict (evalState)
import Data.Default (def)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import GHC.Types.SrcLoc (noSrcSpan)
import Test.Tasty
import Test.Tasty.HUnit

import qualified Clash.Core.FreeVars as C (freeLocalIds)
import Clash.Core.Literal (Literal (..))
import Clash.Core.Name (NameSort (..))
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.Var as C
import Clash.Core.VarEnv (lookupVarEnv, nullVarEnv)
import qualified Clash.Data.UniqMap as UniqMap
import Clash.Driver.Types (Binding (..))
import Clash.Normalize.Types (NormalizeState (..))
import qualified Clash.Normalize.Util as Current
import qualified Clash.Rewrite.Scoped as Scoped (changed)
import Clash.Rewrite.Combinators (bottomupR)
import Clash.Rewrite.Types (Rewrite, RewriteState (..), TransformContext (..))
import qualified Clash.Rewrite.Util as Current (changed)
import Clash.Unique (Unique)

import Clash.Core.Scoped.Convert
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (mkTicks)
import Clash.Core.Scoped.TermInfo (isClosed)
import Clash.Core.Scoped.Util (mkLets)
import qualified Clash.Normalize.Scoped.Util as Scoped
import Clash.Rewrite.Scoped.Types
  (Frame (..), ScopedRewrite, ScopedTransform (..), pushFrame, rootContext)
import qualified Clash.Rewrite.Scoped.Combinators as Scoped (bottomupR)

import Test.Clash.Rewrite
  ( assertAlphaEq, globalId, inScopeOf, intFunTy, intId, intLit, intTy
  , liftedEqTy, localId, mkBinding, mkBindingMap, nameToUnique, pairDataCon
  , rewriteEnvWithTyCons, runRewriteTest, signalTy, systemDomTy, tyVar )
import Test.Clash.Scoped

-- * Type equality constraints

dom, dom2, a2, c3 :: C.TyVar
dom = tyVar User "dom" 1
a2 = tyVar User "a" 2
c3 = tyVar User "c" 3
dom2 = tyVar User "dom2" 8

-- | @a ~ b@
eqTy :: C.Type -> C.Type -> C.Type
eqTy = liftedEqTy

-- | Assert that 'Scoped.substWithTyEq' agrees with 'Current.substWithTyEq',
-- also in a scope where all binders of the term clash with the scope
agreesSubst :: C.Term -> Assertion
agreesSubst tm = withTermScopes tm $ \sc tm' ->
  assertScopedAlphaEq (Current.substWithTyEq tm) (Scoped.substWithTyEq sc tm')

-- | Assert the result of 'Scoped.substWithTyEq', also in a scope where all
-- binders of the term clash with the scope
substGives :: C.Term -> C.Term -> Assertion
substGives tm expected = withTermScopes tm $ \sc tm' ->
  assertScopedAlphaEq expected (Scoped.substWithTyEq sc tm')

-- | Assert the result of 'Scoped.tvSubstWithTyEq', also in a scope where all
-- binders of the type clash with the scope
tvSubstGives :: C.Type -> C.Type -> Assertion
tvSubstGives ty expected = withTypeScopes ty $ \sc ty' ->
  assertScopedTyAlphaEq expected (Scoped.tvSubstWithTyEq sc ty')

substWithTyEqTests :: [TestTree]
substWithTyEqTests =
  -- The regression tests of "Clash.Tests.Normalize.Util"
  [ testCase "substWithTyEq doesn't introduce free variables (#1060)" $ do
      -- /\dom -> \(eq :: dom ~ System) -> eq
      let eq = localId User "eq" 2 (eqTy (C.VarTy dom) systemDomTy)
          tm = C.TyLam dom (C.Lam eq (C.Var eq))
      agreesSubst tm
      -- \(eq :: System ~ System) -> removedArg @(System ~ System)
      assertBool "closed" (isClosed (Scoped.substWithTyEq emptyScope (fromClashTermClosed tm)))
  , testCase "substWithTyEq avoids capture (#1868)" $ do
      let eq = localId User "eq" 3 (eqTy (C.VarTy dom) (C.VarTy a2))
          x = localId User "x" 4 (C.VarTy dom)
      -- /\a -> /\dom -> \(eq :: dom ~ a) -> /\a -> \(x :: dom) -> x
      agreesSubst (C.TyLam a2 (C.TyLam dom (C.Lam eq (C.TyLam a2 (C.Lam x (C.Var x))))))
  , testCase "substWithTyEq keeps the order of TyLams and Lams (#1868)" $ do
      let eq = localId User "eq" 3 (eqTy (C.VarTy dom) systemDomTy)
          x = localId User "x" 4 (C.VarTy a2)
          y = localId User "y" 5 (C.VarTy a2)
      -- /\a -> \(x :: a) -> /\dom -> \(eq :: dom ~ System) -> \(y :: a) -> y
      agreesSubst (C.TyLam a2 (C.Lam x (C.TyLam dom (C.Lam eq (C.Lam y (C.Var y))))))

  , testCase "substWithTyEq handles several constraints, either way around" $ do
      let eq1 = localId User "eq1" 3 (eqTy (C.VarTy dom) systemDomTy)
          eq2 = localId User "eq2" 4 (eqTy systemDomTy (C.VarTy dom2))
          x = localId User "x" 5 (signalTy (C.VarTy dom2) intTy)
      -- /\dom -> /\dom2 -> \(eq1 :: dom ~ System) -> \(eq2 :: System ~ dom2) ->
      -- \(x :: Signal dom2 Int) -> eq1 `seq` x
      agreesSubst (C.TyLam dom (C.TyLam dom2 (C.Lam eq1 (C.Lam eq2 (C.Lam x
        (C.App (C.Var x) (C.Var eq1)))))))
  , testCase "substWithTyEq without constraints changes nothing" $ do
      let x = localId User "x" 4 (C.VarTy a2)
      agreesSubst (C.TyLam a2 (C.Lam x (C.Var x)))
      agreesSubst (intLit 3)

  , testCase "substWithTyEq moves a type lambda past binders that don't mention it" $ do
      let eq = localId User "eq" 3 (eqTy (C.VarTy dom) (C.VarTy a2))
          x = localId User "x" 4 (C.VarTy dom)
          y = localId User "y" 5 intTy
      -- /\dom -> \(y :: Int) -> /\a -> \(eq :: dom ~ a) -> \(x :: dom) -> x
      -- gives
      -- \(y :: Int) -> /\a -> \(eq :: a ~ a) -> \(x :: a) -> x
      let tm = C.TyLam dom (C.Lam y (C.TyLam a2 (C.Lam eq (C.Lam x (C.Var x)))))
      agreesSubst tm

    -- TODO(substWithTyEq): 'Current.substWithTyEq' leaves @dom@ free in the
    -- type of @c@. 'Scoped.substWithTyEq' can't: the type of @c@ is in scope
    -- of @dom@.
  , testCase "substWithTyEq substitutes in the binders before the constraint" $ do
      let c = localId User "c" 3 (signalTy (C.VarTy dom) intTy)
          eq = localId User "eq" 4 (eqTy (C.VarTy dom) systemDomTy)
          c' = localId User "c" 3 (signalTy systemDomTy intTy)
          eq' = localId User "eq" 4 (eqTy systemDomTy systemDomTy)
      -- /\dom -> \(c :: Signal dom Int) -> \(eq :: dom ~ System) -> c
      substGives (C.TyLam dom (C.Lam c (C.Lam eq (C.Var c))))
                 (C.Lam c' (C.Lam eq' (C.Var c')))
  , testCase "substWithTyEq keeps a constraint whose type isn't in scope" $ do
      let y = localId User "y" 3 (C.VarTy dom)
          eq = localId User "eq" 4 (eqTy (C.VarTy dom) (C.VarTy a2))
          -- /\dom -> \(y :: dom) -> /\a -> \(eq :: dom ~ a) -> y: the type of
          -- y can't mention a
          tm = C.TyLam dom (C.Lam y (C.TyLam a2 (C.Lam eq (C.Var y))))
      substGives tm tm
  ]

tvSubstWithTyEqTests :: [TestTree]
tvSubstWithTyEqTests =
  -- The regression test of "Clash.Tests.Normalize.Util"
  [ testCase "tvSubstWithTyEq avoids capture (#1868)" $
      -- forall a dom. (dom ~ a) -> forall a. dom -> a
      -- gives
      -- forall a. (a ~ a) -> forall c. a -> c
      tvSubstGives
        (C.ForAllTy a2 $ C.ForAllTy dom $
          eqTy (C.VarTy dom) (C.VarTy a2) `C.mkFunTy`
            C.ForAllTy a2 (C.VarTy dom `C.mkFunTy` C.VarTy a2))
        (C.ForAllTy a2 $
          eqTy (C.VarTy a2) (C.VarTy a2) `C.mkFunTy`
            C.ForAllTy c3 (C.VarTy a2 `C.mkFunTy` C.VarTy c3))

    -- TODO(substWithTyEq): 'Current.tvSubstWithTyEq' leaves @dom@ free in the
    -- constraint
  , testCase "tvSubstWithTyEq substitutes in the constraint" $
      -- forall dom. (dom ~ System) -> Signal dom Int
      tvSubstGives
        (C.ForAllTy dom (eqTy (C.VarTy dom) systemDomTy `C.mkFunTy` signalTy (C.VarTy dom) intTy))
        (eqTy systemDomTy systemDomTy `C.mkFunTy` signalTy systemDomTy intTy)
  , testCase "tvSubstWithTyEq substitutes in the arguments before the constraint" $
      -- forall dom. Signal dom Int -> (System ~ dom) -> Int
      tvSubstGives
        (C.ForAllTy dom (signalTy (C.VarTy dom) intTy `C.mkFunTy`
          (eqTy systemDomTy (C.VarTy dom) `C.mkFunTy` intTy)))
        (signalTy systemDomTy intTy `C.mkFunTy`
          (eqTy systemDomTy systemDomTy `C.mkFunTy` intTy))
  , testCase "tvSubstWithTyEq moves a forall past binders that don't mention it" $
      -- forall dom a. (dom ~ a) -> dom
      tvSubstGives
        (C.ForAllTy dom $ C.ForAllTy a2 $ eqTy (C.VarTy dom) (C.VarTy a2) `C.mkFunTy` C.VarTy dom)
        (C.ForAllTy a2 $ eqTy (C.VarTy a2) (C.VarTy a2) `C.mkFunTy` C.VarTy a2)
  , testCase "tvSubstWithTyEq without constraints changes nothing" $ do
      let ty = C.ForAllTy a2 (C.VarTy a2 `C.mkFunTy` C.VarTy a2)
      assertAgreesTy Current.tvSubstWithTyEq Scoped.tvSubstWithTyEq ty
      tvSubstGives ty ty
  ]

-- * Recursive globals

gGlobal, gLocal, xRec :: C.Id
gGlobal = globalId User "g" 7 (intFunTy 1)
gLocal = localId User "g" 7 (intFunTy 1)
xRec = localId User "x" 8 (intFunTy 0)

-- | @g = \\x -> g x@
recState :: RewriteState NormalizeState
recState = def
  { _bindings = mkBindingMap [(gGlobal, C.Lam xRec (C.App (C.Var gGlobal) (C.Var xRec)))] }

recursionTests :: [TestTree]
recursionTests =
  [ testCase "isNonRecursiveGlobalVar doesn't confuse a local with a global" $ do
      -- The local variable keeps the unique of the global
      ((localRes, globalRes, recRes), _, _) <- runRewriteTest def recState $
        withConvEnv [] [gLocal] $ \env -> do
          l <- Scoped.isNonRecursiveGlobalVar (fromClashTerm env (C.Var gLocal))
          g <- Scoped.isNonRecursiveGlobalVar (GlobalVar (mkGlobalId gGlobal) :: Term 'VoidS)
          r <- Scoped.isRecursiveBndr (mkGlobalId gGlobal)
          pure (l, g, r)
      assertBool "local variable is a non-recursive global" (not localRes)
      assertBool "recursive global is non-recursive" (not globalRes)
      assertBool "recursive global is non-recursive" recRes
  , testCase "isRecursiveBndr of a global without a binding" $ do
      (r, _, _) <- runRewriteTest def (def :: RewriteState NormalizeState) $
        Scoped.isRecursiveBndr (mkGlobalId gGlobal)
      r @?= False
  ]

-- * Inlining

inliningTests :: [TestTree]
inliningTests =
  [ testCase "addNewInline counts inlinings per function" $ do
      let f = mkGlobalId (globalId User "f" 10 intTy)
          cf = mkGlobalId (globalId User "cf" 11 intTy)
          res = flip evalState def $ do
            Scoped.addNewInline f cf
            Scoped.addNewInline f cf
            (,) <$> Scoped.alreadyInlined f cf <*> Scoped.alreadyInlined cf f
      res @?= (Just 2, Nothing)
  , testCase "mkInlineTick agrees" $ do
      let f = globalId User "Some.Module.f" 10 intTy
      assertScopedAlphaEq
        (C.mkTicks (intLit 1) [Current.mkInlineTick f])
        (mkTicks (Literal (IntLiteral 1)) [Scoped.mkInlineTick (mkGlobalId f)] :: Term 'VoidS)
  ]

-- * Constant arguments

shouldReduceTests :: [TestTree]
shouldReduceTests =
  [ testCase "shouldReduce in an argument a primitive wants constant" $ do
      (r, _, _) <- runRewriteTest def (def :: RewriteState NormalizeState) $
        Scoped.shouldReduce (argCtx "Clash.Explicit.SimIO.mealyIO" 2)
      r @?= True
  , testCase "shouldReduce elsewhere" $ do
      (r, _, _) <- runRewriteTest def (def :: RewriteState NormalizeState) $
        (,) <$> Scoped.shouldReduce (argCtx "unknownPrim" 2)
            <*> Scoped.shouldReduce (rootContext emptyScope)
      r @?= (False, False)
  ]
 where
  argCtx nm i = pushFrame (AppArg (Just (nm, 0, i))) (rootContext emptyScope)

-- * Constant specialization

-- | Assert that 'Scoped.constantSpecInfo' agrees with
-- 'Current.constantSpecInfo', comparing the new term in the scope of its new
-- let-bindings
agreesConstantSpec :: C.Term -> Assertion
agreesConstantSpec tm = do
  ((cTm, cFound), _, _) <- runRewriteTest rewriteEnvWithTyCons st $ do
    csr <- Current.constantSpecInfo
             (TransformContext (inScopeOf (Lens.toListOf C.freeLocalIds tm)) []) tm
    let lets = foldr (\(i, x) e -> C.Let (C.NonRec i x) e)
                 (Current.csrNewTerm csr) (Current.csrNewBindings csr)
    pure (lets, Current.csrFoundConstant csr)
  withFreeVarsOf [tm] $ \env -> do
    ((sTm, sFound), _, _) <- runRewriteTest rewriteEnvWithTyCons st $
      Scoped.constantSpecInfo (rootContext (convScope env)) (fromClashTerm env tm) >>= \case
        csr@(Scoped.ConstantSpecInfo _ binds t _) ->
          pure (toClashTerm (mkLets binds t), Scoped.csrFoundConstant csr)
    sFound @?= cFound
    assertAlphaEq cTm sTm
 where
  st :: RewriteState NormalizeState
  st = def{_curFun = (globalId User "cur" 50 intTy, noSrcSpan)}

constantSpecTests :: [TestTree]
constantSpecTests =
  [ testCase "a literal is constant" $ agreesConstantSpec (intLit 3)
  , testCase "a variable is bound" $ agreesConstantSpec (C.Var x1)
  , testCase "a constructor with a constant field" $
      agreesConstantSpec (mkPair (intLit 1) (C.Var x1))
  , testCase "a constructor without constant fields is bound as a whole" $
      agreesConstantSpec (mkPair (C.Var x1) (C.Var y2))
  , testCase "a primitive with a non-constant argument is bound as a whole" $
      agreesConstantSpec (mkPair (C.App (C.Prim incPrim) (C.Var x1)) (intLit 2))
  , testCase "a primitive with constant arguments is constant" $
      agreesConstantSpec (mkPair (C.App (C.Prim incPrim) (intLit 1)) (C.Var y2))
  , testCase "a closed lambda is constant" $
      agreesConstantSpec (C.Lam y2 (C.Var y2))
  , testCase "an open lambda is bound" $
      agreesConstantSpec (C.Lam y2 (C.Var x1))
  ]
 where
  x1 = intId User "x" 1
  y2 = intId User "y" 2
  mkPair a b = C.mkApps (C.Data pairDataCon) [Left a, Left b]
  incPrim = somePrim{C.primName = "incPrim", C.primType = intFunTy 1}

-- * Call graphs

callGraphTests :: [TestTree]
callGraphTests =
  [ testCase "callGraph agrees" $ do
      let cur = Current.callGraph (mkBindingMap bs) f
          sco = Scoped.callGraph (mkScopedBindingMap bs) (mkGlobalId f)
      flatten cur @?= flattenS sco
      List.sort (foldr (:) [] (Current.collectCallGraphUniques cur))
        @?= List.sort (map globalIdUniq (globalSetToList (Scoped.callGraphGlobals sco)))
  ]
 where
  fn nm = globalId User nm (nameToUnique nm) (intFunTy 1)
  f = fn "f"
  g = fn "g"
  h = fn "h"
  k = fn "k"
  x = intId User "x" 1
  -- f = \x -> g (g x); g = \x -> h x; h = \x -> x; k = \x -> f x
  bs =
    [ (f, C.Lam x (C.App (C.Var g) (C.App (C.Var g) (C.Var x))))
    , (g, C.Lam x (C.App (C.Var h) (C.Var x)))
    , (h, C.Lam x (C.Var x))
    , (k, C.Lam x (C.App (C.Var f) (C.Var x)))
    ]

  flatten :: UniqMap.UniqMap (UniqMap.UniqMap Word) -> [(Unique, [(Unique, Word)])]
  flatten cg = List.sort
    [ (u, List.sort (UniqMap.toList m)) | (u, m) <- UniqMap.toList cg ]

  flattenS :: Scoped.CallGraph -> [(Unique, [(Unique, Word)])]
  flattenS cg = List.sort
    [ (globalIdUniq u, List.sort [(globalIdUniq v, n) | (v, n) <- globalEnvToList m])
    | (u, m) <- globalEnvToList cg ]

-- * Classifying functions

classifyTests :: [TestTree]
classifyTests =
  [ testCase ("classifyFunction agrees on " <> show i) $ withFreeVarsOf [tm] $ \env -> do
      let s = fromClashTerm env tm
      show (Scoped.classifyFunction s) @?= show (Current.classifyFunction tm)
      Scoped.isCheapFunction s @?= Current.isCheapFunction tm
  | (i, tm) <- zip [0 :: Int ..] terms
  ]
 where
  f = globalId User "f" 20 (intFunTy 1)
  p = C.Prim somePrim
  x = intId User "x" 1
  y = intId User "y" 2
  terms =
    [ C.Lam x (C.App (C.Var f) (C.Var x))
    , C.Lam x (C.App (C.Var y) (C.Var x))
    , C.Lam x (C.Let (C.NonRec y (C.App (C.Var f) (C.Var x))) (C.App (C.Var f) (C.Var y)))
    , C.Lam x (C.Case (C.Var x) intTy
        [ (C.LitPat (IntLiteral 1), C.App (C.Var f) (C.Var x))
        , (C.DefaultPat, C.App p (C.Var x)) ])
    , C.Let (C.Rec [(y, C.App p (C.Var y))]) (C.App p (C.App (C.Var f) (C.Var y)))
    , C.Tick C.DeDup (C.App p (C.Var x))
    ]

-- * Normalizing global binders

-- | A rewrite of Clash Core that increments every literal
incLits :: Rewrite NormalizeState
incLits = bottomupR $ \_ -> \case
  C.Literal (IntLiteral i) -> Current.changed (C.Literal (IntLiteral (i + 1)))
  e -> pure e

-- | 'incLits' on scoped Core
incLitsScoped :: ScopedRewrite NormalizeState
incLitsScoped = Scoped.bottomupR $ ScopedTransform $ \_ -> \case
  Literal (IntLiteral i) -> Scoped.changed (Literal (IntLiteral (i + 1)))
  e -> pure e

-- | A state with the given bindings, normalized by 'incLits'
normState :: [(C.Id, C.Term)] -> RewriteState NormalizeState
normState bs = st
  { _extra = (_extra st)
      { _normalizationStrategy = incLits
      , _scopedNormalizationStrategy = incLitsScoped } }
 where
  st = def{_bindings = mkBindingMap bs, _curFun = (curF, noSrcSpan)}
  curF = case bs of
    (i, _) : _ -> i
    [] -> error "normState: no bindings"

normalizeTests :: [TestTree]
normalizeTests =
  [ testCase "normalizeTopLvlBndr agrees, and caches its result" $ do
      let st = normState [(f, fTm)]
      (cur, _, _) <- runRewriteTest def st $
        Current.normalizeTopLvlBndr False f (mkBinding f fTm)
      ((sco, sco2), st1, _) <- runRewriteTest def st $ do
        b1 <- Scoped.normalizeTopLvlBndr False (mkGlobalId f) (fromClashTermClosed <$> mkBinding f fTm)
        -- From the cache, so it doesn't matter what the binding is
        b2 <- Scoped.normalizeTopLvlBndr False (mkGlobalId f) (fromClashTermClosed <$> mkBinding f (intLit 0))
        pure (b1, b2)
      assertScopedAlphaEq (bindingTerm cur) (bindingTerm sco)
      assertScopedAlphaEq (bindingTerm cur) (bindingTerm sco2)
      bindingRecursive sco @?= bindingRecursive cur
      assertAlphaEq (C.varType (bindingId cur)) (C.varType (bindingId sco))
      -- The cache is that of Clash Core
      case lookupVarEnv f (_normalized (_extra st1)) of
        Just b -> assertAlphaEq (bindingTerm cur) (bindingTerm b)
        Nothing -> assertFailure "not cached"
      -- The strategy ran once
      _transformCounter st1 @?= 1

  , testCase "normalizeTopLvlBndr of a recursive binder" $ do
      let rec = C.Lam x1 (C.App (C.Var f) (intLit 1))
          st = normState [(f, rec)]
      (cur, _, _) <- runRewriteTest def st $
        Current.normalizeTopLvlBndr False f (mkBinding f rec)
      (sco, _, _) <- runRewriteTest def st $
        Scoped.normalizeTopLvlBndr False (mkGlobalId f) (fromClashTermClosed <$> mkBinding f rec)
      assertScopedAlphaEq (bindingTerm cur) (bindingTerm sco)
      bindingRecursive sco @?= True

  , testCase "normalizeTopLvlBndr substitutes equality constraints of top entities" $ do
      let eq = localId User "eq" 3 (eqTy (C.VarTy dom) systemDomTy)
          x = localId User "x" 4 (signalTy (C.VarTy dom) intTy)
          topTy = C.ForAllTy dom (eqTy (C.VarTy dom) systemDomTy `C.mkFunTy`
                    (signalTy (C.VarTy dom) intTy `C.mkFunTy` signalTy (C.VarTy dom) intTy))
          top = globalId User "top" 30 topTy
          topTm = C.TyLam dom (C.Lam eq (C.Lam x (C.Var x)))
          st = normState [(top, topTm)]
      (cur, _, _) <- runRewriteTest def st $
        Current.normalizeTopLvlBndr True top (mkBinding top topTm)
      (sco, _, _) <- runRewriteTest def st $
        Scoped.normalizeTopLvlBndr True (mkGlobalId top) (fromClashTermClosed <$> mkBinding top topTm)
      assertScopedAlphaEq (bindingTerm cur) (bindingTerm sco)
      assertAlphaEq (C.varType (bindingId cur)) (C.varType (bindingId sco))

  , testCase "normalizeWorkFreeApp agrees, and caches its result" $ do
      -- f = \x -> \y -> x + 1, applied to 1
      let st = normState [(f2, f2Tm)]
          args = [Left (intLit 1)]
      (cur, _, _) <- runRewriteTest def st $
        Current.normalizeWorkFreeApp f2 (mkBinding f2 f2Tm) args
      ((sco, sco2), st1, _) <- runRewriteTest def st $ do
        let b = fromClashTermClosed <$> mkBinding f2 f2Tm
            sArgs = [Left (Literal (IntLiteral 1))] :: [Either (Term 'VoidS) (Type 'VoidS)]
        r1 <- Scoped.normalizeWorkFreeApp (mkGlobalId f2) b sArgs
        r2 <- Scoped.normalizeWorkFreeApp (mkGlobalId f2) b sArgs
        pure (r1, r2)
      assertScopedAlphaEq cur sco
      assertScopedAlphaEq cur sco2
      -- The cache is that of Clash Core
      case HashMap.lookup (C.App (C.Var f2) (intLit 1)) (_workFreeAppCache (_extra st1)) of
        Just t -> assertAlphaEq cur t
        Nothing -> assertFailure "not cached"
      _transformCounter st1 @?= 1
      -- The application was normalized as the body of a binder of its own,
      -- which isn't added to the normalized binders
      assertBool "normalized binders" (nullVarEnv (_normalized (_extra st1)))
  ]
 where
  f = globalId User "f" 10 (intFunTy 1)
  f2 = globalId User "f2" 11 (intFunTy 2)
  x1 = intId User "x" 1
  y2 = intId User "y" 2
  -- \x -> let y = 1 in y
  fTm = C.Lam x1 (C.Let (C.NonRec y2 (intLit 1)) (C.Var y2))
  -- \x -> \y -> f x 1
  f2Tm = C.Lam x1 (C.Lam y2 (C.mkApps (C.Var f) [Left (C.Var x1), Left (intLit 1)]))

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Scoped.Util"
    [ testGroup "substWithTyEq" substWithTyEqTests
    , testGroup "tvSubstWithTyEq" tvSubstWithTyEqTests
    , testGroup "recursion" recursionTests
    , testGroup "inlining" inliningTests
    , testGroup "shouldReduce" shouldReduceTests
    , testGroup "constantSpecInfo" constantSpecTests
    , testGroup "callGraph" callGraphTests
    , testGroup "classifyFunction" classifyTests
    , testGroup "normalizing" normalizeTests
    ]
