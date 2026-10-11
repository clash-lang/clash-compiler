{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.Scoped.Util": the scoped twins of the regression
  tests in "Clash.Tests.Rewrite.Util", and agreement with the functions of
  "Clash.Rewrite.Util" on converted inputs.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Rewrite.Scoped.Util (tests) where

import Data.Default (def)
import Data.Either (rights)
import qualified Data.IntMap as IntMap

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Property, testProperty, (===))

import qualified Clash.Core.Literal as C
import Clash.Core.Name (NameSort (User), noSrcSpan)
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import qualified Clash.Core.TysPrim as C
import Clash.Core.Var (Id, Var (varName, varType, varUniq))
import qualified Clash.Core.FreeVars as C
import Clash.Core.VarEnv (lookupVarEnv)
import Clash.Driver.Types (Binding (..))
import Clash.Normalize.Types (NormalizeSession, NormalizeState)
import Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..))
import qualified Clash.Rewrite.Util as C
import Clash.Util.Supply (newSupply)

import Clash.Core.Scoped.Alpha (aeqTm)
import Clash.Core.Scoped.Convert
  (convScope, fromClashTerm, fromClashType, toClashTerm, toClashType, withConvEnv)
import Clash.Core.Scoped.Internal.Scope (scopeWatermark)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (AbsBinder (..))
import Clash.Rewrite.Scoped (rootContext, whnfRW)
import qualified Clash.Rewrite.Scoped as Scoped
import Clash.Rewrite.Scoped.Util

import Test.Clash.Rewrite
  ( addTy, argUniques, assertAlphaEq, assertNoFreeLocals, assertNoFreeTyVars
  , firstUniques, globalId, heapLookupEvaluator, intFunTy, intId
  , intLit, intTy, kindedTyVar, localId, numTy, runRewriteTest, showPprU
  , testTyConMap, tyVar, vecTy )
import Test.Clash.Scoped
  ( Step (..), aeqTermTyped, assertLetHandlerAgrees, atPath, forAllScrambled
  , runScopedOn, testTypeEnv, withFreeVarsOf )

-- * liftBinding

-- | The state for 'liftBinding': the function being normalized, and a unique
-- supply
liftState :: IO (RewriteState NormalizeState)
liftState = do
  supply <- newSupply
  pure def{_curFun = (globalId User "top" 42 (intFunTy 1), noSrcSpan), _uniqSupply = supply}

-- | Lift the first binding of a recursive let-expression with
-- 'liftBinding'. Gives the term that replaces the binder, and the body of the
-- global it applies.
liftFirst :: RewriteState NormalizeState -> C.Term -> IO (C.Term, Maybe C.Term)
liftFirst st tm = do
  (newExpr, st1, _) <- runScopedOn def st tm $ \sc -> \case
    Let bind body -> openLetBindings sc bind body $ \scL _ lbs _ -> case lbs of
      lb : _ -> toClashTerm <$> liftBinding scL lb
      [] -> error "liftFirst: no bindings"
    _ -> error "liftFirst: not a let-expression"
  case C.collectArgs newExpr of
    (C.Var g, _) -> pure (newExpr, bindingTerm <$> lookupVarEnv g (_bindings st1))
    _ -> assertFailure ("Expected an application of a global, but got:\n" <> showPprU newExpr)

-- | The let-expression that binds @r = e@, with body @r@
letrec :: Id -> C.Term -> C.Term
letrec r e = C.Let (C.Rec [(r, e)]) (C.Var r)

-- | Scoped twin of the test of the same name in "Clash.Tests.Rewrite.Util".
-- Lifting
--
-- > r = \z -> k (/\a -> r z) x    where x :: a, and a is free
--
-- must not give
--
-- > top_r = /\a -> \x z -> k (/\a -> top_r @a x z) x
liftBindingTyVarCapture :: Assertion
liftBindingTyVarCapture = do
  st <- liftState
  let a = tyVar User "a" 50
      r = localId User "r" 40 (intFunTy 1)
      z = intId User "z" 41
      x = localId User "x" 31 (C.VarTy a)
      k = globalId User "k" 33 (intFunTy 2)
      e = C.Lam z (C.mkApps (C.Var k) [Left (C.TyLam a (C.App (C.Var r) (C.Var z))), Left (C.Var x)])
  liftFirst st (letrec r e) >>= \case
    (newExpr, Just body) | (C.Var rLifted, _) <- C.collectArgs newExpr -> do
      let b' = tyVar User "b" 51
          rApp = C.mkApps (C.Var rLifted) [Right (C.VarTy a), Left (C.Var x), Left (C.Var z)]
      assertAlphaEq
        (C.TyLam a (C.Lam x (C.Lam z (C.mkApps (C.Var k)
          [Left (C.TyLam b' rApp), Left (C.Var x)]))))
        body
    _ -> assertFailure "Lifted function not found in global bindings"

-- | Scoped twin of the test of the same name in "Clash.Tests.Rewrite.Util".
-- Lifting
--
-- > r = \z -> k (r z) x    where x :: a, a :: kk, and kk is free
--
-- must give @/\\kk a -> ..@, not @/\\a kk -> ..@, even though the unique of @a@
-- is smaller than the unique of @kk@.
liftBindingTyVarOrder :: Assertion
liftBindingTyVarOrder = do
  st <- liftState
  let kk = tyVar User "kk" 51
      a = kindedTyVar User "a" 50 (C.VarTy kk)
      r = localId User "r" 40 (intFunTy 1)
      z = intId User "z" 41
      x = localId User "x" 31 (C.VarTy a)
      k = globalId User "k" 33 (intFunTy 2)
      e = C.Lam z (C.mkApps (C.Var k) [Left (C.App (C.Var r) (C.Var z)), Left (C.Var x)])
  liftFirst st (letrec r e) >>= \case
    (newExpr, Just body) -> do
      assertEqual ("Type arguments of the lifted function in:\n" <> showPprU newExpr)
        [C.VarTy kk, C.VarTy a] (rights (snd (C.collectArgs newExpr)))
      assertNoFreeTyVars body
    _ -> assertFailure "Lifted function not found in global bindings"

-- | Scoped twin of the test of the same name in "Clash.Tests.Rewrite.Util":
-- 'liftBinding' replaces the recursive occurrences by an application of the
-- new function /before/ abstracting over the free variables.
liftBindingSubstitutesBeforeAbstracting :: Assertion
liftBindingSubstitutesBeforeAbstracting = do
  st <- liftState
  let r = localId User "r" 40 (intFunTy 1)
      z = intId User "z" 41
      x = intId User "x" 31
      k = globalId User "k" 33 (intFunTy 2)
      -- r = \z -> k (r z) x
      e = C.Lam z (C.mkApps (C.Var k) [Left (C.App (C.Var r) (C.Var z)), Left (C.Var x)])
  liftFirst st (letrec r e) >>= \case
    (newExpr, Just body) -> do
      assertEqual ("Arguments of the lifted function in:\n" <> showPprU newExpr)
        [Just (varUniq x)] (argUniques (snd (C.collectArgs newExpr)))
      assertNoFreeLocals body
    _ -> assertFailure "Lifted function not found in global bindings"

-- | Scoped twin of the test of the same name in "Clash.Tests.Rewrite.Util":
-- the new global function gets a unique from the supply, which can be the
-- unique of a free local variable of the lifted binding. The two must not be
-- confused.
liftBindingLocalSharesUniqueWithGlobal :: Assertion
liftBindingLocalSharesUniqueWithGlobal = do
  st <- liftState
  u : _ <- pure (firstUniques 1 (_uniqSupply st))
  let r = localId User "r" 40 (intFunTy 1)
      z = intId User "z" 41
      x = intId User "x" u
      k = globalId User "k" 33 (intFunTy 2)
      -- r = \z -> k (r z) x
      e = C.Lam z (C.mkApps (C.Var k) [Left (C.App (C.Var r) (C.Var z)), Left (C.Var x)])
  liftFirst st (letrec r e) >>= \case
    (newExpr, Just body) | (C.Var rLifted, _) <- C.collectArgs newExpr -> do
      assertEqual "The lifted function gets the unique of x" u (varUniq rLifted)
      -- \x z -> k (rLifted x z) x
      assertAlphaEq
        (C.Lam x (C.Lam z (C.mkApps (C.Var k)
          [ Left (C.mkApps (C.Var rLifted) [Left (C.Var x), Left (C.Var z)])
          , Left (C.Var x) ])))
        body
    _ -> assertFailure "Lifted function not found in global bindings"

-- * whnfRW

whnfEnv :: RewriteEnv
whnfEnv = def{_evaluator = heapLookupEvaluator}

whnfState :: RewriteState NormalizeState
whnfState = def{_globalHeap = (IntMap.empty, 0)}

-- | Reduce the subterm at a path of a closed term to WHNF with the scoped
-- 'whnfRW', in the scoped context of the subterm. Gives whether the result is
-- the subterm itself, and the result.
whnfAt :: [Step] -> C.Term -> IO (Bool, C.Term)
whnfAt path tm = do
  (res, _, _) <- runScopedOn whnfEnv whnfState tm $ \sc e0 ->
    atPath path (rootContext sc) e0 $ \ctx e -> do
      e1 <- whnfRW False ctx e (\_ t -> C.changed t)
      pure (aeqTm e1 e, toClashTerm e1)
  pure res

-- | 'whnfRW' uses the let-bindings of scoped contexts, which are never
-- shadowed
whnfScopedContextTests :: [TestTree]
whnfScopedContextTests =
  [ testCase "let x = 3 in \\y -> x" $
      whnfAt [LetBodyStep, LamStep] (C.Let (C.NonRec x (intLit 3)) (C.Lam y (C.Var x)))
        >>= assertAlphaEq (intLit 3) . snd
  , testCase "let x = 3 in \\x -> x" $
      whnfAt [LetBodyStep, LamStep] (C.Let (C.NonRec x (intLit 3)) (C.Lam x (C.Var x)))
        >>= assertBool "unchanged" . fst
    -- Like for Clash Core, the bindings of a recursive let-binding are not
    -- used in its right-hand sides
  , testCase "let x = 3 in letrec { x = 4; w = x } in w" $
      whnfAt [LetBodyStep, LetRhsStep 1]
        (C.Let (C.NonRec x (intLit 3))
          (C.Let (C.Rec [(x, intLit 4), (w, C.Var x)]) (C.Var w)))
        >>= assertBool "unchanged" . fst
  , testCase "let x = 3 in let y = x in y" $
      whnfAt [LetBodyStep, LetBodyStep]
        (C.Let (C.NonRec x (intLit 3)) (C.Let (C.NonRec y (C.Var x)) (C.Var y)))
        >>= assertAlphaEq (intLit 3) . snd
  ]
 where
  x = intId User "x" 1
  y = intId User "y" 2
  w = intId User "w" 5

-- * Agreement with Clash Core

-- | Local functions, and local variables of type @Int@
vf, vg, va, vb, vc, vx, vy, vz, vr, vs :: Id
vf = localId User "f" 10 (intFunTy 1)
vg = localId User "g" 11 (intFunTy 2)
va = intId User "a" 1
vb = intId User "b" 2
vc = intId User "c" 3
vx = intId User "x" 4
vy = intId User "y" 5
vz = intId User "z" 6
vr = localId User "r" 7 (intFunTy 1)
vs = intId User "s" 8

-- | The state for transformations that lift bindings, see 'liftState'
liftStateFixed :: RewriteState NormalizeState
liftStateFixed = def{_curFun = (globalId User "top" 42 (intFunTy 1), noSrcSpan)}

app :: Id -> [C.Term] -> C.Term
app h = C.mkApps (C.Var h) . map Left

-- | Whether the right-hand side of a binding is a literal or a variable
litOrVar :: Term n -> Bool
litOrVar = \case
  Literal {} -> True
  Var {} -> True
  _ -> False

litOrVarC :: C.Term -> Bool
litOrVarC = \case
  C.Literal {} -> True
  C.Var {} -> True
  _ -> False

-- | Let-expressions to inline literals and variables in
inlineInputs :: [(String, C.Term)]
inlineInputs =
  [ ("letrec { x = 1; y = x; z = g y z } in z",
      C.Let (C.Rec [(vx, intLit 1), (vy, C.Var vx), (vz, app vg [C.Var vy, C.Var vz])]) (C.Var vz))
  , ("letrec { x = y; y = x } in x",
      C.Let (C.Rec [(vx, C.Var vy), (vy, C.Var vx)]) (C.Var vx))
  , ("letrec { a = f b; b = 1; c = f a } in c",
      C.Let (C.Rec [(va, app vf [C.Var vb]), (vb, intLit 1), (vc, app vf [C.Var va])]) (C.Var vc))
  , ("letrec { a = 1; b = f a } in g a b",
      C.Let (C.Rec [(va, intLit 1), (vb, app vf [C.Var va])]) (app vg [C.Var va, C.Var vb]))
  , ("let x = 1 in f x", C.Let (C.NonRec vx (intLit 1)) (app vf [C.Var vx]))
  , ("let x = f 2 in x", C.Let (C.NonRec vx (app vf [intLit 2])) (C.Var vx))
  , ("let x = 1 in 2", C.Let (C.NonRec vx (intLit 1)) (intLit 2))
  , ("letrec { x = f 1 } in 2", C.Let (C.Rec [(vx, app vf [intLit 1])]) (intLit 2))
  ]

-- | Let-expressions to remove unused bindings from
removeInputs :: [(String, C.Term)]
removeInputs =
  [ ("letrec { x = 1; y = f x; z = 2 } in y",
      C.Let (C.Rec [(vx, intLit 1), (vy, app vf [C.Var vx]), (vz, intLit 2)]) (C.Var vy))
  , ("letrec { x = 1 } in 2", C.Let (C.Rec [(vx, intLit 1)]) (intLit 2))
  , ("let x = 1 in x", C.Let (C.NonRec vx (intLit 1)) (C.Var vx))
  , ("let x = 1 in 2", C.Let (C.NonRec vx (intLit 1)) (intLit 2))
  , ("letrec { x = f y; y = f x; z = 3 } in z",
      C.Let (C.Rec [(vx, app vf [C.Var vy]), (vy, app vf [C.Var vx]), (vz, intLit 3)]) (C.Var vz))
  , ("letrec { x = f y; y = f x } in x",
      C.Let (C.Rec [(vx, app vf [C.Var vy]), (vy, app vf [C.Var vx])]) (C.Var vx))
  ]

-- | Let-expressions to inline or lift bindings of
inlineOrLiftInputs :: [(String, C.Term)]
inlineOrLiftInputs =
  [ ("letrec { r = \\z -> g (r z) s; c = 1 } in f (r c)",
      C.Let (C.Rec [(vr, C.Lam vz (app vg [app vr [C.Var vz], C.Var vs])), (vc, intLit 1)])
        (app vf [app vr [C.Var vc]]))
  , ("letrec { r = \\z -> g z c; c = 1; b = r 2 } in f b",
      C.Let (C.Rec [(vr, C.Lam vz (app vg [C.Var vz, C.Var vc])), (vc, intLit 1), (vb, app vr [intLit 2])])
        (app vf [C.Var vb]))
  , ("let r = \\z -> g z s in r 1",
      C.Let (C.NonRec vr (C.Lam vz (app vg [C.Var vz, C.Var vs]))) (app vr [intLit 1]))
  ]

-- | Join points, and functions that aren't
joinPointInputs :: [(String, C.Term)]
joinPointInputs =
  [ ("let j = \\z -> f z in case s of { 1 -> j 1; _ -> j 2 }",
      C.Let (C.NonRec j jRhs) (caseS (app j [intLit 1]) (app j [intLit 2])))
  , ("let j = \\z -> f z in case s of { 1 -> j 1; _ -> 3 }",
      C.Let (C.NonRec j jRhs) (caseS (app j [intLit 1]) (intLit 3)))
  , ("let j = \\z -> f z in f (j 1)",
      C.Let (C.NonRec j jRhs) (app vf [app j [intLit 1]]))
  , ("letrec { j = \\z -> f z; k = \\z -> j z } in case s of { 1 -> k 1; _ -> j 2 }",
      C.Let (C.Rec [(j, jRhs), (k, C.Lam vz (app j [C.Var vz]))])
        (caseS (app k [intLit 1]) (app j [intLit 2])))
  , ("letrec { j = \\z -> f z; k = f (j 1) } in case s of { 1 -> j k; _ -> j 2 }",
      C.Let (C.Rec [(j, jRhs), (k, app vf [app j [intLit 1]])])
        (caseS (app j [C.Var k]) (app j [intLit 2])))
  ]
 where
  j = localId User "j" 20 (intFunTy 1)
  k = localId User "k" 21 (intFunTy 1)
  jRhs = C.Lam vz (app vf [C.Var vz])
  caseS alt1 alt2 =
    C.Case (C.Var vs) intTy [(C.LitPat (C.IntLiteral 1), alt1), (C.DefaultPat, alt2)]

-- | 'isJoinPointIn' of the first binder agrees with Clash Core
joinPointAgrees :: C.Term -> Assertion
joinPointAgrees tm = case tm of
  C.Letrec ((j, _) : _) _ -> do
    (actual, _, _) <- runScopedOn def liftStateFixed tm $ \sc -> \case
      Let bind body -> openLetBindings sc bind body $ \_ _ lbs body1 -> case lbs of
        lb : _ -> pure (isJoinPointIn (lbName lb) lbs body1)
        [] -> error "no bindings"
      _ -> error "not a let-expression"
    actual @?= C.isJoinPointIn j tm
  _ -> assertFailure "not a let-expression"

-- | The right-hand side of a let-binding
lbRhsC :: C.LetBinding -> C.Term
lbRhsC = snd

-- | 'isJoinPointIn' of the first binder doesn't change when the binders of
-- the let-expression are renamed, e.g. to the name of that binder
joinPointScrambled :: C.Term -> Property
joinPointScrambled tm = withFreeVarsOf [tm] $ \env ->
  let sc = convScope env
      isJoin = \case
        Let bind body -> openLetBindings sc bind body $ \_ _ lbs body1 -> case lbs of
          lb : _ -> isJoinPointIn (lbName lb) lbs body1
          [] -> error "no bindings"
        _ -> error "not a let-expression"
      tm1 = fromClashTerm env tm
  in  forAllScrambled sc tm1 $ \tm2 -> isJoin tm2 === isJoin tm1

-- | Binders for terms and types get their types and kinds, and fresh names
mkBinderForTests :: [TestTree]
mkBinderForTests =
  [ testCase "a term" $ do
      (res, _, _) <- runScopedOn def liftStateFixed (app vf [C.Var vx]) $ \sc e ->
        pure $ mkBinderFor te sc (varName vx) (Left e) $ \sc' -> \case
          AbsId bnd ->
            Just (toClashType (idBinderType bnd), tmNameRaw (tmNameOf bnd) > scopeWatermark sc
                 , scopeWatermark sc' == tmNameRaw (tmNameOf bnd))
          AbsTy _ -> Nothing
      res @?= Just (intTy, True, True)
  , testCase "a type" $ do
      (res, _, _) <- runScopedOn def liftStateFixed (C.TyApp (C.Var vf) (C.mkFunTy intTy intTy)) $ \sc -> \case
        TyApp _ ty -> pure $ mkBinderFor te sc (varName vx) (Right ty) $ \_ -> \case
          AbsTy bnd -> Just (toClashType (tyBinderKind bnd))
          AbsId _ -> Nothing
        _ -> error "not a type application"
      res @?= Just C.liftedTypeKind
  ]
 where
  te = testTypeEnv

-- | 'dependencySortTyVars' agrees with 'C.dependencySortTyVars'
dependencySortAgrees :: [C.TyVar] -> Assertion
dependencySortAgrees tvs = withConvEnv (C.dependencySortTyVars tvs) [] $ \env -> do
  let occs = map (fromClashType env . C.VarTy) tvs
  map toClashType (dependencySortTyVars occs) @?= map C.VarTy (C.dependencySortTyVars tvs)

-- | 'bindPureHeap' binds the bindings of the heap over the result of a
-- rewrite that changes the term, inlining the work-free ones and removing the
-- unused ones, and doesn't bind them when it doesn't change it
bindPureHeapTests :: [TestTree]
bindPureHeapTests =
  [ testCase "changed" $
      run (\_ v -> changed v) >>= assertAlphaEq
        (C.Let (C.Rec [(vy, app vf [intLit 1])]) (C.Var vy)) . fst
  , testCase "unchanged" $ do
      (res, ch) <- run (\_ _ -> pure (Literal (C.IntLiteral 2)))
      assertAlphaEq (intLit 2) res
      assertBool "no change" (not ch)
  ]
 where
  -- The heap: { x = 1; y = f x; z = 2 }, and the value y
  heap = C.Let (C.Rec [(vx, intLit 1), (vy, app vf [C.Var vx]), (vz, intLit 2)]) (C.Var vy)
  run
    :: (forall l. Distinct l => Scoped.TransformContext l -> Term l -> NormalizeSession (Term l))
    -> IO (C.Term, Bool)
  run k = do
    (res, _, ch) <- runScopedOn def liftStateFixed heap $ \sc -> \case
      Let bind v -> toClashTerm <$> bindPureHeap (rootContext sc) bind v k
      _ -> error "not a let-expression"
    pure (res, ch)

-- | 'normalizeTermTypes' agrees with 'C.normalizeTermTypes'
normalizeTermTypesAgrees :: C.Term -> Assertion
normalizeTermTypesAgrees tm = withFreeVarsOf [tm] $ \env ->
  assertAlphaEqTyped (C.normalizeTermTypes testTyConMap tm)
    (toClashTerm (normalizeTermTypes testTypeEnv (convScope env) (fromClashTerm env tm)))
 where
  assertAlphaEqTyped expected actual =
    assertBool ("Expected:\n" <> showPprU expected <> "\nbut got:\n" <> showPprU actual)
      (aeqTermTyped expected actual)

tests :: TestTree
tests =
  testGroup "Clash.Tests.Rewrite.Scoped.Util"
    [ testGroup "liftBinding"
        [ testCase "avoids capturing free type variables" liftBindingTyVarCapture
        , testCase "abstracts over type variables in dependency order" liftBindingTyVarOrder
        , testCase "substitutes before abstracting" liftBindingSubstitutesBeforeAbstracting
        , testCase "keeps a local that shares a unique with the lifted function"
            liftBindingLocalSharesUniqueWithGlobal
        ]
    , testGroup "whnfRW uses the let-bindings of a scoped context" whnfScopedContextTests
    , testGroup "inlineBinders agrees with Clash Core"
        [ testCase name $
            assertLetHandlerAgrees def liftStateFixed
              (C.inlineBinders (\_ lb -> pure (litOrVarC (lbRhsC lb))))
              (inlineBinders (\_ lb -> pure (litOrVar (lbRhs lb))))
              tm
        | (name, tm) <- inlineInputs ]
    , testGroup "removeUnusedBinders agrees with Clash Core"
        [ testCase name $
            assertLetHandlerAgrees def liftStateFixed
              (\_ e bind body -> maybe (pure e) C.changed (C.removeUnusedBinders bind body))
              (\ctx e bind body -> maybe (pure e) changed (removeUnusedBinders (Scoped.tfScope ctx) bind body))
              tm
        | (name, tm) <- removeInputs ]
    , testGroup "inlineOrLiftBinders agrees with Clash Core"
        [ testCase name $ do
            supply <- newSupply
            assertLetHandlerAgrees def liftStateFixed{_uniqSupply = supply}
              (C.inlineOrLiftBinders (\_ -> pure True) (\_ lb -> litOrVarC (lbRhsC lb)))
              (inlineOrLiftBinders (\_ _ -> pure True) (\_ _ lb -> litOrVar (lbRhs lb)))
              tm
        | (name, tm) <- inlineOrLiftInputs ]
    , testGroup "cleanupHeapBindings agrees with Clash Core"
        [ testCase name $
            assertLetHandlerAgrees def liftStateFixed
              (\ctx _ bind body -> C.cleanupHeapBindings ctx (C.bindToList bind) body)
              (\ctx _ bind body -> cleanupHeapBindings ctx bind body)
              tm
        -- The evaluator allocates a recursive let-binding
        | (name, tm@(C.Let C.Rec {} _)) <- inlineInputs ++ removeInputs ]
    , testGroup "isJoinPointIn agrees with Clash Core"
        [ testCase name (joinPointAgrees tm) | (name, tm) <- joinPointInputs ]
    , testGroup "isJoinPointIn doesn't depend on the names of binders"
        [ testProperty name (joinPointScrambled tm) | (name, tm) <- joinPointInputs ]
    , testGroup "mkBinderFor" mkBinderForTests
    , testGroup "dependencySortTyVars agrees with Clash Core"
        [ testCase "a :: kk, kk" $ dependencySortAgrees [ak, kk]
        , testCase "kk, a :: kk" $ dependencySortAgrees [kk, ak]
        , testCase "b, a :: kk, c, kk" $ dependencySortAgrees [tb, ak, tc, kk]
        ]
    , testGroup "bindPureHeap" bindPureHeapTests
    , testGroup "normalizeTermTypes agrees with Clash Core"
        [ testCase "x :: Vec (2 + 3) Int" $ normalizeTermTypesAgrees (C.Var xv)
        , testCase "cast" $ normalizeTermTypesAgrees (C.Cast (C.Var xv) (varType xv) intTy)
        , testCase "f x" $ normalizeTermTypesAgrees (C.App (C.Var vf) (C.Var vx))
        ]
    , testGroup "isVoidWrapper agrees with Clash Core"
        [ testCase name $ do
            (actual, _, _) <- runScopedOn def liftStateFixed tm (\_ e -> pure (isVoidWrapper e))
            actual @?= C.isVoidWrapper tm
        | (name, tm) <-
            [ ("\\x -> f y", C.Lam vx (app vf [C.Var vy]))
            , ("\\x -> f x", C.Lam vx (app vf [C.Var vx]))
            , ("\\x -> 1", C.Lam vx (intLit 1))
            , ("f x", app vf [C.Var vx])
            ]
        ]
    , testGroup "isUntranslatable agrees with Clash Core"
        [ testCase name $ do
            (expected, _, _) <- runRewriteTest def liftStateFixed (C.isUntranslatable False tm)
            (actual, _, _) <- runScopedOn def liftStateFixed tm (isUntranslatable False)
            actual @?= expected
        | (name, tm) <- [("1", intLit 1), ("f", C.Var vf), ("f x", app vf [C.Var vx])]
        ]
    ]
 where
  kk = tyVar User "kk" 52
  ak = kindedTyVar User "a" 50 (C.VarTy kk)
  tb = tyVar User "b" 53
  tc = tyVar User "c" 54
  xv = localId User "x" 60 (vecTy (addTy (numTy 2) (numTy 3)) intTy)
