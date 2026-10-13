{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.Util": regression tests, and the results of
  its functions on a range of inputs.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Rewrite.Scoped.Util (tests) where

import Data.Default (def)
import qualified Data.IntMap as IntMap
import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (Property, testProperty, (===))

import qualified Clash.Core.Literal as C
import Clash.Core.Name (nameOcc)
import qualified Clash.Core.Basic as C (ConstTy (TyCon), LitTy (NumTy))
import Clash.Core.Pretty (PrettyOptions (..), PrettyPrec, showPpr')
import Clash.Driver.Types (Binding (..), DebugOpts (..), debugNone)
import Clash.Normalize.Types (NormalizeSession, NormalizeState)
import Clash.Rewrite.Types (RewriteState (..))
import Clash.Util.Supply (newSupply)

import Clash.Core.Scoped.Alpha (aeqTm, aeqTy)
import Clash.Core.Scoped.Builder (appTyB, constTyB, litTyB)
import Clash.Core.Scoped.FreeVars (teleNameBinders)
import Clash.Core.Scoped.Internal.Scope (scopeWatermark)
import Clash.Core.Scoped.NameMap (globalEnvElems, lookupGlobalEnv)
import Clash.Core.Scoped.Subst (unsinkTerm)
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.Term (AbsBinder (..), collectArgs)
import Clash.Rewrite.Scoped (rootContext, whnfRW)
import Clash.Rewrite.Scoped.Apply (applyDebug)
import qualified Clash.Rewrite.Scoped as Scoped
import Clash.Rewrite.Util

import Test.Clash.Rewrite
  ( addTyConName, assertErrorContainsIO, firstUniques, rewriteEnvWithDebug, runRewriteTest
  , showPprU )
import Test.Clash.Scoped

-- | Render a term as text, with types
pp :: PrettyPrec p => p -> String
pp = showPpr' (PrettyOptions False True False True)

-- * liftBinding

-- | The function being normalized
topG :: GlobalId
topG = testGlobalId "top_G42" "Int -> Int"

-- | The state for 'liftBinding': the function being normalized, and a unique
-- supply
liftState :: IO (RewriteState NormalizeState)
liftState = do
  supply <- newSupply
  pure (withCurFun topG def{_uniqSupply = supply})

-- | Lift the first binding of the let-expression of a term built in a scope,
-- with 'liftBinding'. Gives the term that replaces the binder (which mustn't
-- refer to the let-bindings), and the global it applies, with its body.
liftFirst
  :: Distinct n
  => RewriteState NormalizeState
  -> TestScope n
  -> Term n
  -> IO (Term n, GlobalId, Term 'VoidS)
liftFirst st ts tm = do
  (newExpr0, st1, _) <- runRewriteTest def st $ case tm of
    Let bind body -> openLetBindings (testScope ts) bind body $ \scL bs lbs _ -> case lbs of
      lb : _ -> unsinkTerm (teleNameBinders bs) <$> liftBinding scL lb
      [] -> error "liftFirst: no bindings"
    _ -> error "liftFirst: not a let-expression"
  newExpr <- maybe (assertFailure "the new expression refers to a let-binding") pure newExpr0
  case collectArgs newExpr of
    (GlobalVar g, _) -> case lookupGlobalEnv g (_bindings st1) of
      Just b -> pure (newExpr, g, bindingTerm b)
      Nothing -> assertFailure "Lifted function not found in global bindings"
    _ -> assertFailure ("Expected an application of a global, but got:\n" <> showPprU newExpr)

-- | Replace a global by another one, e.g. a global made by a transformation by
-- one that a test can write
renameGlobal :: GlobalId -> GlobalId -> Term n -> Term n
renameGlobal g g' = go
 where
  go :: Term x -> Term x
  go = \case
    GlobalVar h | h == g -> GlobalVar g'
    Lam b e -> Lam b (go e)
    TyLam b e -> TyLam b (go e)
    App e1 e2 -> App (go e1) (go e2)
    TyApp e ty -> TyApp (go e) ty
    Let (NonRec b x) e -> Let (NonRec b (go x)) (go e)
    Let (Rec bs xs) e -> Let (Rec bs (map go xs)) (go e)
    Case e ty alts -> Case (go e) ty [Alt p (go a) | Alt p a <- alts]
    e -> e

-- | The lifted function, as tests write it
rL :: GlobalId
rL = testGlobalId "rL_G99" "Int"

-- | Lifting
--
-- > r = \z -> k (/\a -> r z) x    where x :: a, and a is free
--
-- must not give
--
-- > top_r = /\a -> \x z -> k (/\a -> top_r @a x z) x
liftBindingTyVarCapture :: Assertion
liftBindingTyVarCapture = do
  st <- liftState
  withTerm "let { (r :: Int -> Int) = \\(z :: Int) -> (k_G33 :: Int -> Int -> Int) (\\(a :: Type) -> r z) (x :: a) } in r" $ \ts tm -> do
    (_, g, body) <- liftFirst st ts tm
    assertTermIs emptyTestScope
      "\\(a :: Type) (x :: a) (z :: Int) -> k_G33 (\\(b :: Type) -> rL_G99 @a x z) x"
      (renameGlobal g rL body)

-- | Lifting
--
-- > r = \z -> k (r z) x    where x :: a, a :: kk, and kk is free
--
-- must give @/\\kk a -> ..@. In scoped Core, a type variable has a raw name
-- above those of the type variables in its kind.
liftBindingTyVarOrder :: Assertion
liftBindingTyVarOrder = do
  st <- liftState
  withTestScope [tyVarD "kk", kindedTyVarD "a" "kk", varD "x" "a"] $ \ts -> do
    let tm = termIn ts "let { (r :: Int -> Int) = \\(z :: Int) -> (k_G33 :: Int -> Int -> Int) (r z) x } in r"
    (newExpr, _, body) <- liftFirst st ts tm
    case collectArgs newExpr of
      (_, [Right t1, Right t2, Left x]) -> do
        assertTypeIs ts "kk" t1
        assertTypeIs ts "a" t2
        assertTermIs ts "x" x
      (_, args) -> assertFailure ("unexpected arguments: " <> show (length args))
    assertLintClean emptyTestScope body

-- | 'liftBinding' replaces the recursive occurrences by an application of the
-- new function /before/ abstracting over the free variables.
liftBindingSubstitutesBeforeAbstracting :: Assertion
liftBindingSubstitutesBeforeAbstracting = do
  st <- liftState
  -- r = \z -> k (r z) x
  withTerm "let { (r :: Int -> Int) = \\(z :: Int) -> (k_G33 :: Int -> Int -> Int) (r z) (x :: Int) } in r" $ \ts tm -> do
    (newExpr, g, body) <- liftFirst st ts tm
    assertTermIs ts "rL_G99 x" (renameGlobal g rL newExpr)
    assertTermIs emptyTestScope "\\(x :: Int) (z :: Int) -> k_G33 (rL_G99 x z) x" (renameGlobal g rL body)

-- | The new global function gets a unique from the supply, which can be the
-- raw name of a free local variable of the lifted binding. The two must not be
-- confused.
liftBindingLocalSharesUniqueWithGlobal :: Assertion
liftBindingLocalSharesUniqueWithGlobal = do
  st <- liftState
  u : _ <- pure (firstUniques 1 (_uniqSupply st))
  withTestScope [atRaw (fromIntegral u) (varD "x" "Int")] $ \ts -> do
    -- r = \z -> k (r z) x
    let tm = termIn ts "let { (r :: Int -> Int) = \\(z :: Int) -> (k_G33 :: Int -> Int -> Int) (r z) x } in r"
    (_, g, body) <- liftFirst st ts tm
    assertEqual "The lifted function gets the raw name of x" u (globalIdUniq g)
    assertTermIs emptyTestScope "\\(x :: Int) (z :: Int) -> k_G33 (rL_G99 x z) x" (renameGlobal g rL body)

-- * whnfRW

whnfState :: RewriteState NormalizeState
whnfState = def{_globalHeap = (IntMap.empty, 0)}

-- | Reduce the subterm at a path of a closed term to WHNF with the scoped
-- 'whnfRW', in the scoped context of the subterm. Gives whether the result is
-- the subterm itself, and whether it is the expected term.
whnfAt :: [Step] -> TestTerm -> TestTerm -> IO (Bool, Bool)
whnfAt path tm expected = do
  (res, _, _) <- runRewriteTest def whnfState $
    atPath path (rootContext emptyScope) (closedTerm tm) $ \ctx e -> do
      e1 <- whnfRW False ctx e (\_ t -> changed t)
      pure (aeqTm e1 e, aeqTm (sinkClosed (closedTerm expected)) e1)
  pure res

-- | 'whnfRW' uses the let-bindings of scoped contexts, which are never
-- shadowed
whnfContextTests :: [TestTree]
whnfContextTests =
  [ testCase "let x = 3 in \\y -> x" $
      whnfAt [LetBodyStep, LamStep] "let { !(x :: Int) = 3 } in \\(y :: Int) -> x" "3"
        >>= assertBool "3" . snd
  , testCase "let x = 3 in \\x -> x" $
      whnfAt [LetBodyStep, LamStep] "let { !(x :: Int) = 3 } in \\(x :: Int) -> x" "3"
        >>= assertBool "unchanged" . fst
    -- The bindings of a recursive let-binding are not used in its right-hand
    -- sides
  , testCase "let x = 3 in letrec { x = 4; w = x } in w" $
      whnfAt [LetBodyStep, LetRhsStep 1]
        "let { !(x :: Int) = 3 } in let { (x :: Int) = 4; (w :: Int) = x } in w" "4"
        >>= assertBool "unchanged" . fst
  , testCase "let x = 3 in let y = x in y" $
      whnfAt [LetBodyStep, LetBodyStep] "let { !(x :: Int) = 3 } in let { !(y :: Int) = x } in y" "3"
        >>= assertBool "3" . snd
  ]

-- * Expected results

-- | The state for transformations that lift bindings, see 'liftState'
liftStateFixed :: RewriteState NormalizeState
liftStateFixed = withCurFun topG def

-- | Whether the right-hand side of a binding is a literal or a variable
litOrVar :: Term n -> Bool
litOrVar = \case
  Literal {} -> True
  Var {} -> True
  _ -> False

-- | Run a let-handler on a let-expression, in the root context of the scope
-- of its free variables. Gives the result, rendered as text, whether it
-- signaled a change, and the global bindings it added, rendered as text.
runLetHandler
  :: RewriteState NormalizeState
  -> (forall n l. Distinct n => Scoped.TransformContext n -> Term n -> Bind n l -> Term l -> NormalizeSession (Term n))
  -> TestTerm
  -> IO (String, Bool, [(String, String)])
runLetHandler st h t = withTerm t $ \ts -> \case
  e@(Let bind body) -> do
    (res, st1, ch) <- runRewriteTest def st (h (rootContext (testScope ts)) e bind body)
    let new = [ (Text.unpack (nameOcc (globalIdName (bindingId b))), pp (bindingTerm b))
              | b <- globalEnvElems (_bindings st1)
              , null (lookupGlobalEnv (bindingId b) (_bindings st)) ]
    pure (pp res, ch, new)
  _ -> assertFailure "not a let-expression"

-- | Let-expressions to inline literals and variables in. The free variables
-- spell out their uniques, which give their order in the scope (see
-- 'withTerm').
inlineInputs :: [TestTerm]
inlineInputs =
  [ "let { (x_4 :: Int) = 1; (y_5 :: Int) = x_4; (z_6 :: Int) = (g_11 :: Int -> Int -> Int) y_5 z_6 } in z_6"
  , "let { (x_4 :: Int) = y_5; (y_5 :: Int) = x_4 } in x_4"
  , "let { (a_1 :: Int) = (f_10 :: Int -> Int) b_2; (b_2 :: Int) = 1; (c_3 :: Int) = f_10 a_1 } in c_3"
  , "let { (a_1 :: Int) = 1; (b_2 :: Int) = (f_10 :: Int -> Int) a_1 } in (g_11 :: Int -> Int -> Int) a_1 b_2"
  , "let { !(x_4 :: Int) = 1 } in (f_10 :: Int -> Int) x_4"
  , "let { !(x_4 :: Int) = (f_10 :: Int -> Int) 2 } in x_4"
  , "let { !(x_4 :: Int) = 1 } in 2"
  , "let { (x_4 :: Int) = (f_10 :: Int -> Int) 1 } in 2"
  ]

-- | Let-expressions to remove unused bindings from
removeInputs :: [TestTerm]
removeInputs =
  [ "let { (x_4 :: Int) = 1; (y_5 :: Int) = (f_10 :: Int -> Int) x_4; (z_6 :: Int) = 2 } in y_5"
  , "let { (x_4 :: Int) = 1 } in 2"
  , "let { !(x_4 :: Int) = 1 } in x_4"
  , "let { !(x_4 :: Int) = 1 } in 2"
  , "let { (x_4 :: Int) = (f_10 :: Int -> Int) y_5; (y_5 :: Int) = f_10 x_4; (z_6 :: Int) = 3 } in z_6"
  , "let { (x_4 :: Int) = (f_10 :: Int -> Int) y_5; (y_5 :: Int) = f_10 x_4 } in x_4"
  ]

-- | Let-expressions to inline or lift bindings of
inlineOrLiftInputs :: [TestTerm]
inlineOrLiftInputs =
  [ "let { (r_7 :: Int -> Int) = \\(z_6 :: Int) -> (g_11 :: Int -> Int -> Int) (r_7 z_6) (s_8 :: Int); (c_3 :: Int) = 1 } in (f_10 :: Int -> Int) (r_7 c_3)"
  , "let { (r_7 :: Int -> Int) = \\(z_6 :: Int) -> (g_11 :: Int -> Int -> Int) z_6 c_3; (c_3 :: Int) = 1; (b_2 :: Int) = r_7 2 } in (f_10 :: Int -> Int) b_2"
  , "let { !(r_7 :: Int -> Int) = \\(z_6 :: Int) -> (g_11 :: Int -> Int -> Int) z_6 (s_8 :: Int) } in r_7 1"
  ]

-- | Join points, and functions that aren't, and whether the first binder is
-- one
joinPointInputs :: [(TestTerm, Bool)]
joinPointInputs =
  [ ("let { !(j :: Int -> Int) = \\(z :: Int) -> (f :: Int -> Int) z } in (case (s :: Int) of { 1 -> j 1; _ -> j 2 }) :: Int", True)
  , ("let { !(j :: Int -> Int) = \\(z :: Int) -> (f :: Int -> Int) z } in (case (s :: Int) of { 1 -> j 1; _ -> 3 }) :: Int", False)
  , ("let { !(j :: Int -> Int) = \\(z :: Int) -> (f :: Int -> Int) z } in f (j 1)", False)
  , ("let { (j :: Int -> Int) = \\(z :: Int) -> (f :: Int -> Int) z; (k :: Int -> Int) = \\(z :: Int) -> j z } in (case (s :: Int) of { 1 -> k 1; _ -> j 2 }) :: Int", True)
  , ("let { (j :: Int -> Int) = \\(z :: Int) -> (f :: Int -> Int) z; (k :: Int) = f (j 1) } in (case (s :: Int) of { 1 -> j k; _ -> j 2 }) :: Int", False)
    -- A binder with the name of the join point hides it
  , ("let { !(j :: Int -> Int) = (\\(j :: Int -> Int) -> j) (f :: Int -> Int) } in j 1", False)
  , ("let { (j :: Int -> Int) = (\\(j :: Int -> Int) -> j) (f :: Int -> Int) } in j 1", False)
  , ("let { !(j :: Int -> Int) = \\(z :: Int) -> (f :: Int -> Int) z } in (case (s :: Int) of { 1 -> j 1; _ -> let { !(j :: Int -> Int) = f } in j 2 }) :: Int", False)
  , ("let { !(j :: Int -> Int) = \\(z :: Int) -> (f :: Int -> Int) z } in (case (s :: Int) of { 1 -> j 1; _ -> (\\(j :: Int -> Int) -> j 2) f }) :: Int", False)
  ]

-- | 'isJoinPointIn' of the first binder of a let-expression
isJoinPointFirst :: Distinct n => Scope n -> Term n -> Bool
isJoinPointFirst sc = \case
  Let bind body -> openLetBindings sc bind body $ \_ _ lbs body1 -> case lbs of
    lb : _ -> isJoinPointIn (lbName lb) lbs body1
    [] -> error "no bindings"
  _ -> error "not a let-expression"

-- | 'isJoinPointIn' of the first binder doesn't change when the binders of
-- the let-expression are renamed, e.g. to the name of that binder
joinPointScrambled :: TestTerm -> Property
joinPointScrambled t = withTerm t $ \ts tm ->
  let sc = testScope ts
  in  forAllScrambled sc tm $ \tm2 -> isJoinPointFirst sc tm2 === isJoinPointFirst sc tm

-- | Binders for terms and types get their types and kinds, and fresh names
mkBinderForTests :: [TestTree]
mkBinderForTests =
  [ testCase "a term" $ withTerm "(f :: Int -> Int) (x :: Int)" $ \ts e -> do
      let sc = testScope ts
          res = mkBinderFor testTypeEnv sc (testOcc "x") (Left e) $ \sc' -> \case
            AbsId bnd ->
              Just (aeqTy (sinkClosed (closedType "Int")) (idBinderType bnd), tmNameRaw (tmNameOf bnd) > scopeWatermark sc
                   , scopeWatermark sc' == tmNameRaw (tmNameOf bnd))
            AbsTy _ -> Nothing
      res @?= Just (True, True, True)
  , testCase "a type" $ do
      let res = mkBinderFor testTypeEnv emptyScope (testOcc "x") (Right (closedType "Int -> Int")) $ \_ -> \case
            AbsTy bnd -> Just (aeqTy (sinkClosed (closedType "Type")) (tyBinderKind bnd))
            AbsId _ -> Nothing
      res @?= Just True
  ]
 where
  testOcc = \nm -> case termIn emptyTestScope (fromStringTerm ("\\(" <> nm <> " :: Int) -> " <> nm)) of
    Lam b _ -> idBinderOcc b
    _ -> error "testOcc"
  fromStringTerm = parseTestTerm

-- | 'dependencySortTyVars' sorts type variables after the ones in their
-- kinds, and keeps the order otherwise
dependencySortTests :: [TestTree]
dependencySortTests =
  [ testCase name $ withTestScope [tyVarD "kk", kindedTyVarD "a" "kk", tyVarD "b", tyVarD "c"] $ \ts ->
      [ nameOcc occ | VarTy occ _ _ <- dependencySortTyVars (map (typeIn ts) input) ] @?= expected
  | (name, input, expected) <-
      [ ("a :: kk, kk", ["a", "kk"], ["kk", "a"])
      , ("kk, a :: kk", ["kk", "a"], ["kk", "a"])
      , ("b, a :: kk, c, kk", ["b", "a", "c", "kk"], ["b", "kk", "a", "c"]) ] ]

-- | 'bindPureHeap' binds the bindings of the heap over the result of a
-- rewrite that changes the term, inlining the work-free ones and removing the
-- unused ones, and doesn't bind them when it doesn't change it
bindPureHeapTests :: [TestTree]
bindPureHeapTests =
  [ testCase "changed" $
      run (\_ v -> changed v) $ \ts res _ ->
        assertTermIs ts "let { (y :: Int) = f 1 } in y" res
  , testCase "unchanged" $
      run (\_ _ -> pure (Literal (C.IntLiteral 2))) $ \ts res ch -> do
        assertTermIs ts "2" res
        assertBool "no change" (not ch)
  ]
 where
  -- The heap: { x = 1; y = f x; z = 2 }, and the value y
  heap = "let { (x :: Int) = 1; (y :: Int) = (f :: Int -> Int) x; (z :: Int) = 2 } in y"
  run
    :: (forall l. Distinct l => Scoped.TransformContext l -> Term l -> NormalizeSession (Term l))
    -> (forall n. Distinct n => TestScope n -> Term n -> Bool -> Assertion)
    -> Assertion
  run k check = withTerm heap $ \ts -> \case
    Let bind v -> do
      (res, _, ch) <- runRewriteTest def liftStateFixed (bindPureHeap (rootContext (testScope ts)) bind v k)
      check ts res ch
    _ -> assertFailure "not a let-expression"

-- | 'normalizeTermTypes' normalizes the types of variable occurrences and of
-- casts
normalizeTermTypesTests :: [TestTree]
normalizeTermTypesTests =
  [ testCase "x :: Vec (2 + 3) Int" $ withVec $ \ts ->
      case normalizeTermTypes testTypeEnv (testScope ts) (termIn ts "x") of
        Var _ ty _ -> assertTypeIs ts "Vec 5 Int" ty
        e -> assertFailure ("not a variable: " <> showPprU e)
  , testCase "cast" $ withVec $ \ts -> case termIn ts "x" of
      x@(Var _ ty _) -> case normalizeTermTypes testTypeEnv (testScope ts) (Cast x ty (typeIn ts "Int")) of
        Cast (Var _ ty' _) t1 t2 -> do
          assertTypeIs ts "Vec 5 Int" ty'
          assertTypeIs ts "Vec 5 Int" t1
          assertTypeIs ts "Int" t2
        e -> assertFailure ("not a cast: " <> showPprU e)
      e -> assertFailure ("not a variable: " <> showPprU e)
  , testCase "f x" $ withTerm "(f :: Int -> Int) (x :: Int)" $ \ts e ->
      case normalizeTermTypes testTypeEnv (testScope ts) e of
        App (Var _ fTy _) (Var _ xTy _) -> do
          assertTypeIs ts "Int -> Int" fTy
          assertTypeIs ts "Int" xTy
        e' -> assertFailure ("unexpected term: " <> showPprU e')
  ]
 where
  withVec :: (forall n. Distinct n => TestScope n -> Assertion) -> Assertion
  withVec = withTestScope [varD "x" vecTy]
  -- Vec (2 + 3) Int
  vecTy = builtType
    (appTyB (appTyB (testTypeB "Vec")
      (appTyB (appTyB (constTyB (C.TyCon addTyConName)) (litTyB (C.NumTy 2))) (litTyB (C.NumTy 3))))
      (testTypeB "Int"))

-- | Debug a transformation that signals a change, with the given number of
-- transformations applied before it, under
-- @-fclash-debug-transformations-from=10@ and
-- @-fclash-debug-transformations-limit=5@.
applyDebugWithLimitFrom :: Word -> IO (Term 'VoidS)
applyDebugWithLimitFrom nApplied = do
  (e, _, _) <- runRewriteTest env st (applyDebug "t" (closedTerm "0") True (closedTerm "1"))
  pure e
 where
  -- 'applyWith' counts the transformation before debugging it
  st :: RewriteState NormalizeState
  st = def{_transformCounter = nApplied + 1}
  env = rewriteEnvWithDebug debugNone
    { dbg_transformationsFrom = Just 10
    , dbg_transformationsLimit = Just 5
    }

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
    , testGroup "whnfRW uses the let-bindings of a scoped context" whnfContextTests

    -- @-fclash-debug-transformations-limit@ counts from
    -- @-fclash-debug-transformations-from@. 'applyDebug' used to subtract the
    -- latter from the number of applied transformations as a 'Word', which
    -- wrapped around before reaching it, exceeding the limit straight away.
    , testGroup "applyDebug transformation limit counts from the first debugged transformation"
        [ testCase "no limit before the first debugged transformation" $
            applyDebugWithLimitFrom 0 >>= assertBool "result" . aeqTm (closedTerm "1")
        , testCase "limit not yet exceeded" $
            applyDebugWithLimitFrom 14 >>= assertBool "result" . aeqTm (closedTerm "1")
        , testCase "limit exceeded" $
            assertErrorContainsIO "-fclash-debug-transformations-limit exceeded" $
              applyDebugWithLimitFrom 15
        ]
    , testGroup "inlineBinders"
        [ testCase (show i) $
            runLetHandler liftStateFixed (inlineBinders (\_ lb -> pure (litOrVar (lbRhs lb)))) tm
              >>= (@?= expected)
        | (i, tm, expected) <- zip3 [0 :: Int ..] inlineInputs expectedInline ]
    , testGroup "removeUnusedBinders"
        [ testCase (show i) $
            runLetHandler liftStateFixed
              (\ctx e bind body -> maybe (pure e) changed (removeUnusedBinders (Scoped.tfScope ctx) bind body))
              tm
              >>= (@?= expected)
        | (i, tm, expected) <- zip3 [0 :: Int ..] removeInputs expectedRemove ]
    , testGroup "inlineOrLiftBinders"
        [ testCase (show i) $ do
            supply <- newSupply
            runLetHandler liftStateFixed{_uniqSupply = supply}
              (inlineOrLiftBinders (\_ _ -> pure True) (\_ _ lb -> litOrVar (lbRhs lb)))
              tm
              >>= (@?= expected)
        | (i, tm, expected) <- zip3 [0 :: Int ..] inlineOrLiftInputs expectedInlineOrLift ]
    , testGroup "cleanupHeapBindings"
        [ testCase (show i) $
            runLetHandler liftStateFixed (\ctx _ bind body -> cleanupHeapBindings ctx bind body) tm
              >>= (@?= expected)
        -- The evaluator allocates a recursive let-binding
        | (i, tm, expected) <- zip3 [0 :: Int ..] recursiveInputs expectedCleanup ]
    , testGroup "isJoinPointIn"
        [ testCase (show i) $ withTerm tm $ \ts e -> isJoinPointFirst (testScope ts) e @?= expected
        | (i, (tm, expected)) <- zip [0 :: Int ..] joinPointInputs ]
    , testGroup "isJoinPointIn doesn't depend on the names of binders"
        [ testProperty (show i) (joinPointScrambled tm) | (i, (tm, _)) <- zip [0 :: Int ..] joinPointInputs ]
    , testGroup "mkBinderFor" mkBinderForTests
    , testGroup "dependencySortTyVars" dependencySortTests
    , testGroup "bindPureHeap" bindPureHeapTests
    , testGroup "normalizeTermTypes" normalizeTermTypesTests
    , testGroup "isVoidWrapper"
        [ testCase tm $ withTerm (parseTestTerm tm) $ \_ e -> isVoidWrapper e @?= expected
        | (tm, expected) <-
            [ ("\\(x :: Int) -> (f :: Int -> Int) (y :: Int)", True)
            , ("\\(x :: Int) -> (f :: Int -> Int) x", False)
            , ("\\(x :: Int) -> 1", False)
            , ("(f :: Int -> Int) (x :: Int)", False :: Bool)
            ]
        ]
    , testGroup "isUntranslatable"
        [ testCase tm $ withTerm (parseTestTerm tm) $ \ts e -> do
            (actual, _, _) <- runRewriteTest def liftStateFixed (isUntranslatable False (testScope ts) e)
            actual @?= expected
        | (tm, expected) <-
            [("1", False), ("(f :: Int -> Int)", True), ("(f :: Int -> Int) (x :: Int)", False :: Bool)]
        ]
    ]
 where
  -- The inputs with a recursive let-expression, as the evaluator allocates them
  recursiveInputs = [ t | (i, t) <- zip [0 :: Int ..] (inlineInputs <> removeInputs), i `notElem` [4, 5, 6, 10, 11] ]

-- | The expected results of the handler on the inputs
expectedInline :: [(String, Bool, [(String, String)])]
expectedInline =
  [ ("letrec\n  z :: Int\n  = g 1# z\nin z",True,[])
  , ("letrec\n  y :: Int\n  = y\nin y",True,[])
  , ("letrec\n  a :: Int\n  = f 1#\n  c :: Int\n  = f a\nin c",True,[])
  , ("letrec\n  b :: Int\n  = f 1#\nin g 1# b",True,[])
  , ("f 1#",True,[])
  , ("let\n  x :: Int\n  = f 2#\nin x",False,[])
  , ("2#",True,[])
  , ("letrec\n  x :: Int\n  = f 1#\nin 2#",False,[])
  ]

-- | The expected results of the handler on the inputs
expectedRemove :: [(String, Bool, [(String, String)])]
expectedRemove =
  [ ("letrec\n  x :: Int\n  = 1#\n  y :: Int\n  = f x\nin y",True,[])
  , ("2#",True,[])
  , ("let\n  x :: Int\n  = 1#\nin x",False,[])
  , ("2#",True,[])
  , ("letrec\n  z :: Int\n  = 3#\nin z",True,[])
  , ("letrec\n  x :: Int\n  = f y\n  y :: Int\n  = f x\nin x",False,[])
  ]

-- | The expected results of the handler on the inputs
expectedInlineOrLift :: [(String, Bool, [(String, String)])]
expectedInlineOrLift =
  [ ("f (c$top_r s g 1#)",True,[("c$top_r","\955(s :: Int) ->\n\955(g :: Int -> Int -> Int) ->\n\955(z :: Int) ->\ng (c$top_r s g z) s")])
  , ("f (c$top_b g)",True,[("c$top_r","\955(g :: Int -> Int -> Int) ->\n\955(z :: Int) ->\ng z 1#"),("c$top_b","\955(g :: Int -> Int -> Int) ->\nc$top_r g 2#")])
  , ("c$top_r s g 1#",True,[("c$top_r","\955(s :: Int) ->\n\955(g :: Int -> Int -> Int) ->\n\955(z :: Int) ->\ng z s")])
  ]

-- | The expected results of the handler on the inputs
expectedCleanup :: [(String, Bool, [(String, String)])]
expectedCleanup =
  [ ("letrec\n  z :: Int\n  = g 1# z\nin z",True,[])
  , ("letrec\n  y :: Int\n  = y\nin y",True,[])
  , ("letrec\n  a :: Int\n  = f 1#\n  c :: Int\n  = f a\nin c",True,[])
  , ("letrec\n  b :: Int\n  = f 1#\nin g 1# b",True,[])
  , ("2#",False,[])
  , ("letrec\n  y :: Int\n  = f 1#\nin y",True,[])
  , ("2#",True,[])
  , ("3#",True,[])
  , ("letrec\n  x :: Int\n  = f y\n  y :: Int\n  = f x\nin x",False,[])
  ]
