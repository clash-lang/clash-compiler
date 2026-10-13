{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Util": the regression tests that the
  Clash Core implementation had, and tests whose expected results are those of
  "Clash.Normalize.Scoped.Util", which agreed with the Clash Core
  implementation.
  The tests of functions on terms and types run in the scope of the free
  variables of their input, and in a scope that the binders of the input
  clash with.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Normalize.Scoped.Util (tests) where

import Control.Monad.State.Strict (evalState)
import Data.Default (def)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Literal (Literal (..))
import qualified Clash.Core.Basic as C (IsMultiPrim (SingleResult), WorkInfo (WorkConstant))
import Clash.Driver.Types (Binding (..))
import Clash.Normalize.Types (NormalizeState (..), TermClassification (..))
import qualified Clash.Rewrite.Scoped as Scoped (changed)
import Clash.Rewrite.Types (RewriteState (..))

import Clash.Core.Scoped.Alpha (aeqTy)
import Clash.Core.Scoped.NameMap
import Clash.Core.Scoped.Syntax
import Clash.Core.Scoped.TermInfo (isClosed)
import Clash.Core.Scoped.Util (mkLets)
import qualified Clash.Normalize.Scoped.Util as Scoped
import Clash.Rewrite.Scoped.Types
  (Frame (..), ScopedRewrite, ScopedTransform (..), pushFrame, rootContext)
import qualified Clash.Rewrite.Scoped.Combinators as Scoped (bottomupR)

import Test.Clash.Rewrite
  ( TestDefs (..), mkGlobalBinding, parseTestTermWith, rewriteEnvWithTyCons, runRewriteTest )
import Test.Clash.Scoped
import Test.Clash.Scoped.Print (showTestType)

-- * Type equality constraints

-- | Assert that 'Scoped.substWithTyEq' gives the expected term, also in a
-- scope where the binders of the term clash with the scope
substGives :: TestTerm -> TestTerm -> Assertion
substGives input expected = withTerm input $ \ts e -> do
  check ts e
  withBinderClashes ts e check
 where
  check :: Distinct n => TestScope n -> Term n -> Assertion
  check ts e = do
    let e' = Scoped.substWithTyEq (testScope ts) e
    assertLintClean ts e'
    assertTermIs ts expected e'

-- | Assert that 'Scoped.tvSubstWithTyEq' gives the expected type, also in a
-- scope where the binders of the type clash with the scope
tvSubstGives :: TestType -> TestType -> Assertion
tvSubstGives input expected = withType input $ \ts ty -> do
  check ts ty
  withClashing ts [tyVarD ("clash_" <> show i) | i <- [900001 .. 900000 + forAlls ty]] ty check
 where
  check :: Distinct n => TestScope n -> Type n -> Assertion
  check ts ty = assertTypeIs ts expected (Scoped.tvSubstWithTyEq (testScope ts) ty)

  forAlls :: Type x -> Int
  forAlls = \case
    ForAllTy b t -> 1 + forAlls (tyBinderKind b) + forAlls t
    AppTy t1 t2 -> forAlls t1 + forAlls t2
    _ -> 0

substWithTyEqTests :: [TestTree]
substWithTyEqTests =
  -- The regression tests that the Clash Core implementation had
  [ testCase "substWithTyEq doesn't introduce free variables (#1060)" $ do
      let input = "\\(dom :: Type) (eq :: dom ~ \"System\") -> eq"
      substGives input
        "\\(eq :: \"System\" ~ \"System\") -> Clash.Normalize.Primitives.removedArg @(\"System\" ~ \"System\")"
      assertBool "closed" (isClosed (Scoped.substWithTyEq emptyScope (closedTerm input)))
  , testCase "substWithTyEq avoids capture (#1868)" $
      substGives
        "\\(a :: Type) (dom :: Type) (eq :: dom ~ a) (a :: Type) (x :: dom) -> x"
        "\\(a :: Type) (eq :: a ~ a) (b :: Type) (x :: a) -> x"
  , testCase "substWithTyEq keeps the order of TyLams and Lams (#1868)" $
      substGives
        "\\(a :: Type) (x :: a) (dom :: Type) (eq :: dom ~ \"System\") (y :: a) -> y"
        "\\(a :: Type) (x :: a) (eq :: \"System\" ~ \"System\") (y :: a) -> y"

  , testCase "substWithTyEq handles several constraints, either way around" $
      -- eq1 `seq` x, as an application
      substGives
        "\\(dom :: Type) (dom2 :: Type) (eq1 :: dom ~ \"System\") (eq2 :: \"System\" ~ dom2) (x :: Clash.Signal.Internal.Signal dom2 Int) -> x eq1"
        "\\(eq1 :: \"System\" ~ \"System\") (eq2 :: \"System\" ~ \"System\") (x :: Clash.Signal.Internal.Signal \"System\" Int) -> x (Clash.Normalize.Primitives.removedArg @(\"System\" ~ \"System\"))"
  , testCase "substWithTyEq without constraints changes nothing" $ do
      substGives "\\(a :: Type) (x :: a) -> x" "\\(a :: Type) (x :: a) -> x"
      substGives "3" "3"

  , testCase "substWithTyEq moves a type lambda past binders that don't mention it" $
      substGives
        "\\(dom :: Type) (y :: Int) (a :: Type) (eq :: dom ~ a) (x :: dom) -> x"
        "\\(y :: Int) (a :: Type) (eq :: a ~ a) (x :: a) -> x"

    -- Substituting only after the constraint would leave @dom@ free in the
    -- type of @c@, so 'Scoped.substWithTyEq' substitutes in it too: the type
    -- of @c@ is in scope of @dom@.
  , testCase "substWithTyEq substitutes in the binders before the constraint" $
      substGives
        "\\(dom :: Type) (c :: Clash.Signal.Internal.Signal dom Int) (eq :: dom ~ \"System\") -> c"
        "\\(c :: Clash.Signal.Internal.Signal \"System\" Int) (eq :: \"System\" ~ \"System\") -> c"
    -- The type of @y@ can't mention @a@
  , testCase "substWithTyEq keeps a constraint whose type isn't in scope" $
      substGives
        "\\(dom :: Type) (y :: dom) (a :: Type) (eq :: dom ~ a) -> y"
        "\\(dom :: Type) (y :: dom) (a :: Type) (eq :: dom ~ a) -> y"
  ]

tvSubstWithTyEqTests :: [TestTree]
tvSubstWithTyEqTests =
  -- The regression test that the Clash Core implementation had
  [ testCase "tvSubstWithTyEq avoids capture (#1868)" $
      tvSubstGives
        "forall (a :: Type) (dom :: Type). (dom ~ a) -> forall (a :: Type). dom -> a"
        "forall (a :: Type). (a ~ a) -> forall (c :: Type). a -> c"

    -- Substituting only after the constraint would leave @dom@ free in the
    -- constraint
  , testCase "tvSubstWithTyEq substitutes in the constraint" $
      tvSubstGives
        "forall (dom :: Type). (dom ~ \"System\") -> Clash.Signal.Internal.Signal dom Int"
        "(\"System\" ~ \"System\") -> Clash.Signal.Internal.Signal \"System\" Int"
  , testCase "tvSubstWithTyEq substitutes in the arguments before the constraint" $
      tvSubstGives
        "forall (dom :: Type). Clash.Signal.Internal.Signal dom Int -> (\"System\" ~ dom) -> Int"
        "Clash.Signal.Internal.Signal \"System\" Int -> (\"System\" ~ \"System\") -> Int"
  , testCase "tvSubstWithTyEq moves a forall past binders that don't mention it" $
      tvSubstGives
        "forall (dom :: Type) (a :: Type). (dom ~ a) -> dom"
        "forall (a :: Type). (a ~ a) -> a"
  , testCase "tvSubstWithTyEq without constraints changes nothing" $
      tvSubstGives "forall (a :: Type). a -> a" "forall (a :: Type). a -> a"
  ]

-- * Recursive globals

-- | The global @g = \\x -> g x@
gGlobal :: GlobalId
gGlobal = testGlobalId "g_G7" "Int -> Int"

recState :: RewriteState NormalizeState
recState = def {_scopedBindings = mkGlobalBindings [(gGlobal, tmG "\\(x :: Int) -> g_G7 x")]}
 where
  tmG = parseTestTermWith (TestDefs [] [] [gGlobal])

recursionTests :: [TestTree]
recursionTests =
  [ testCase "isNonRecursiveGlobalVar doesn't confuse a local with a global" $
      -- The local variable has the unique of the global
      withTestScope [varD "g_7" "Int -> Int"] $ \ts -> do
        ((localRes, globalRes, recRes), _, _) <- runRewriteTest def recState $ do
          l <- Scoped.isNonRecursiveGlobalVar (termIn ts "g_7")
          g <- Scoped.isNonRecursiveGlobalVar (GlobalVar gGlobal :: Term 'VoidS)
          r <- Scoped.isRecursiveBndr gGlobal
          pure (l, g, r)
        assertBool "local variable is a non-recursive global" (not localRes)
        assertBool "recursive global is non-recursive" (not globalRes)
        assertBool "recursive global is non-recursive" recRes
  , testCase "isRecursiveBndr of a global without a binding" $ do
      (r, _, _) <- runRewriteTest def (def :: RewriteState NormalizeState) $
        Scoped.isRecursiveBndr gGlobal
      r @?= False
  ]

-- * Inlining

inliningTests :: [TestTree]
inliningTests =
  [ testCase "addNewInline counts inlinings per function" $ do
      let f = testGlobalId "f_G10" "Int"
          cf = testGlobalId "cf_G11" "Int"
          res = flip evalState def $ do
            Scoped.addNewInline f cf
            Scoped.addNewInline f cf
            (,) <$> Scoped.alreadyInlined f cf <*> Scoped.alreadyInlined cf f
      res @?= (Just 2, Nothing)
    -- The tick names the inlined function after its unqualified name
  , testCase "mkInlineTick" $
      assertTermIs emptyTestScope "Core.prefixName @\"f\" 1"
        (Tick (Scoped.mkInlineTick (testGlobalId "Some.Module.f_G10" "Int")) (Literal (IntLiteral 1)))
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

-- | A primitive @incPrim :: Int -> Int@
incPrim :: PrimInfo
incPrim = mkPrimInfo "incPrim" (closedType "Int -> Int") C.WorkConstant C.SingleResult NoUnfolding

-- | Assert that 'Scoped.constantSpecInfo' gives the expected term, with its
-- new let-bindings, and whether it found a constant
constantSpecGives :: TestTerm -> TestTerm -> Bool -> Assertion
constantSpecGives input expected found =
  withTestScope [varD "x_1" "Int", varD "y_2" "Int"] $ \ts -> do
    ((e, f), _, _) <- runRewriteTest rewriteEnvWithTyCons st $
      Scoped.constantSpecInfo (rootContext (testScope ts)) (termIn ts input) >>= \case
        csr@(Scoped.ConstantSpecInfo _ binds t _) ->
          pure (mkLets binds t, Scoped.csrFoundConstant csr)
    f @?= found
    assertLintClean ts e
    assertTermIs ts expected e
 where
  st :: RewriteState NormalizeState
  st = withCurFun (testGlobalId "cur_G50" "Int") def

constantSpecTests :: [TestTree]
constantSpecTests =
  [ testCase "a literal is constant" $ constantSpecGives "3" "3" True
  , testCase "a variable is bound" $
      constantSpecGives "x_1" "let { !(c :: Int) = x_1 } in c" False
  , testCase "a constructor with a constant field" $
      constantSpecGives "MkPair 1 x_1" "let { !(c :: Int) = x_1 } in MkPair 1 c" True
  , testCase "a constructor without constant fields is bound as a whole" $
      constantSpecGives "MkPair x_1 y_2" "let { !(c :: Pair) = MkPair x_1 y_2 } in c" False
  , testCase "a primitive with a non-constant argument is bound as a whole" $
      constantSpecGives (tmP "MkPair (Prim.incPrim x_1) 2")
        (tmP "let { !(c :: Int) = Prim.incPrim x_1 } in MkPair c 2") True
  , testCase "a primitive with constant arguments is constant" $
      constantSpecGives (tmP "MkPair (Prim.incPrim 1) y_2")
        (tmP "let { !(c :: Int) = y_2 } in MkPair (Prim.incPrim 1) c") True
  , testCase "a closed lambda is constant" $
      constantSpecGives "\\(y :: Int) -> y" "\\(y :: Int) -> y" True
  , testCase "an open lambda is bound" $
      constantSpecGives "\\(y :: Int) -> x_1" "let { !(c :: Int -> Int) = \\(y :: Int) -> x_1 } in c" False
  ]
 where
  tmP = parseTestTermWith (TestDefs [] [incPrim] [])

-- * Call graphs

callGraphTests :: [TestTree]
callGraphTests =
  [ testCase "callGraph" $ do
      let cg = Scoped.callGraph bs f
      [ (globalIdUniq u, List.sort [(globalIdUniq v, n) | (v, n) <- globalEnvToList m])
        | (u, m) <- List.sortOn (globalIdUniq . fst) (globalEnvToList cg) ]
        @?= List.sortOn fst
              [ (globalIdUniq f, [(globalIdUniq g, 2)])
              , (globalIdUniq g, [(globalIdUniq h, 1)])
              , (globalIdUniq h, []) ]
      List.sort (map globalIdUniq (globalSetToList (Scoped.callGraphGlobals cg)))
        @?= List.sort (map globalIdUniq [f, g, h])
  ]
 where
  f = testGlobalId "f_G102" "Int -> Int"
  g = testGlobalId "g_G103" "Int -> Int"
  h = testGlobalId "h_G104" "Int -> Int"
  k = testGlobalId "k_G107" "Int -> Int"
  tmG = parseTestTermWith (TestDefs [] [] [f, g, h, k])
  -- f = \x -> g (g x); g = \x -> h x; h = \x -> x; k = \x -> f x
  bs = mkGlobalBindings
    [ (f, tmG "\\(x :: Int) -> g_G103 (g_G103 x)")
    , (g, tmG "\\(x :: Int) -> h_G104 x")
    , (h, tmG "\\(x :: Int) -> x")
    , (k, tmG "\\(x :: Int) -> f_G102 x") ]

-- * Classifying functions

classifyTests :: [TestTree]
classifyTests =
  [ testCase ("classifyFunction of " <> show i) $ withTerm (tmC tm) $ \_ s -> do
      show (Scoped.classifyFunction s) @?= show expected
      Scoped.isCheapFunction s @?= cheap
  | (i, (tm, expected, cheap)) <- zip [0 :: Int ..] terms
  ]
 where
  tmC = parseTestTermWith (TestDefs [] [somePrimS] [testGlobalId "f_G20" "Int -> Int"])
  somePrimS = mkPrimInfo "somePrim" (closedType "T") C.WorkConstant C.SingleResult NoUnfolding
  terms =
    [ ("\\(x :: Int) -> f_G20 x", TermClassification 1 0 0, True)
    , ("\\(x :: Int) -> (y_2 :: Int -> Int) x", TermClassification 1 0 0, True)
    , ("\\(x :: Int) -> let { !(y :: Int) = f_G20 x } in f_G20 y", TermClassification 1 0 0, True)
    , ("\\(x :: Int) -> (case x of { 1 -> f_G20 x; _ -> Prim.somePrim x }) :: Int", TermClassification 0 0 1, False)
    , ("let { (y :: Int) = Prim.somePrim y } in Prim.somePrim (f_G20 y)", TermClassification 0 1 0, False)
    , ("Core.deDup (Prim.somePrim (x_1 :: Int))", TermClassification 0 1 0, False)
    ]

-- * Normalizing global binders

-- | 'incLits' on scoped Core
incLitsScoped :: ScopedRewrite NormalizeState
incLitsScoped = Scoped.bottomupR $ ScopedTransform $ \_ -> \case
  Literal (IntLiteral i) -> Scoped.changed (Literal (IntLiteral (i + 1)))
  e -> pure e

-- | A state with the given bindings, normalized by 'incLitsScoped', in which
-- the first of them is the function being normalized
normState :: [(GlobalId, Term 'VoidS)] -> RewriteState NormalizeState
normState bs = st
  { _extra = (_extra st) { _scopedNormalizationStrategy = incLitsScoped } }
 where
  st = withCurFun curF def
    {_scopedBindings = globalEnvFromList [(g, mkGlobalBinding g t) | (g, t) <- bs]}
  curF = case bs of
    (i, _) : _ -> i
    [] -> error "normState: no bindings"

normalizeTests :: [TestTree]
normalizeTests =
  [ testCase "normalizeTopLvlBndr, and caches its result" $ do
      let st = normState [(f, fTm)]
      ((sco, sco2), st1, _) <- runRewriteTest def st $ do
        b1 <- Scoped.normalizeTopLvlBndr False f (mkGlobalBinding f fTm)
        -- From the cache, so it doesn't matter what the binding is
        b2 <- Scoped.normalizeTopLvlBndr False f (mkGlobalBinding f (closedTerm "0"))
        pure (b1, b2)
      let expected = tm "\\(x :: Int) -> let { !(y :: Int) = 2 } in y"
      assertTermIs emptyTestScope expected (bindingTerm sco)
      assertTermIs emptyTestScope expected (bindingTerm sco2)
      bindingRecursive sco @?= False
      assertBool "type" (aeqTy (closedType "Int -> Int") (globalIdType (bindingId sco)))
      case lookupGlobalEnv f (_scopedNormalized (_extra st1)) of
        Just b -> assertTermIs emptyTestScope expected (bindingTerm b)
        Nothing -> assertFailure "not cached"
      -- The strategy ran once
      _transformCounter st1 @?= 1

  , testCase "normalizeTopLvlBndr of a recursive binder" $ do
      let rec = closedTerm (tm "\\(x :: Int) -> f_G10 1")
          st = normState [(f, rec)]
      (sco, _, _) <- runRewriteTest def st $
        Scoped.normalizeTopLvlBndr False f (mkGlobalBinding f rec)
      assertTermIs emptyTestScope (tm "\\(x :: Int) -> f_G10 2") (bindingTerm sco)
      bindingRecursive sco @?= True

  , testCase "normalizeTopLvlBndr substitutes equality constraints of top entities" $ do
      let topTy = "forall (dom :: Type). (dom ~ \"System\") -> Clash.Signal.Internal.Signal dom Int -> Clash.Signal.Internal.Signal dom Int"
          top = testGlobalId "top_G30" topTy
          topTm = closedTerm "\\(dom :: Type) (eq :: dom ~ \"System\") (x :: Clash.Signal.Internal.Signal dom Int) -> x"
          st = normState [(top, topTm)]
      (sco, _, _) <- runRewriteTest def st $
        Scoped.normalizeTopLvlBndr True top (mkGlobalBinding top topTm)
      assertTermIs emptyTestScope
        "\\(eq :: \"System\" ~ \"System\") (x :: Clash.Signal.Internal.Signal \"System\" Int) -> x"
        (bindingTerm sco)
      assertBool ("type: " <> showTestType (globalIdType (bindingId sco)))
        (aeqTy
          (closedType "(\"System\" ~ \"System\") -> Clash.Signal.Internal.Signal \"System\" Int -> Clash.Signal.Internal.Signal \"System\" Int")
          (globalIdType (bindingId sco)))

  , testCase "normalizeWorkFreeApp, and caches its result" $ do
      -- f2 = \x -> \y -> f x 1, applied to 1
      let st = normState [(f2, f2Tm)]
      ((sco, sco2), st1, _) <- runRewriteTest def st $ do
        let b = mkGlobalBinding f2 f2Tm
            sArgs = [Left (Literal (IntLiteral 1))] :: [Either (Term 'VoidS) (Type 'VoidS)]
        r1 <- Scoped.normalizeWorkFreeApp f2 b sArgs
        r2 <- Scoped.normalizeWorkFreeApp f2 b sArgs
        pure (r1, r2)
      let expected = tm "(\\(x :: Int) (y :: Int) -> (f_G10 :: Int -> Int -> Int) x 2) 2"
      assertTermIs emptyTestScope expected sco
      assertTermIs emptyTestScope expected sco2
      case Map.elems (_scopedWorkFreeAppCache (_extra st1)) of
        [t] -> assertTermIs emptyTestScope expected t
        _ -> assertFailure "not cached"
      _transformCounter st1 @?= 1
      -- The application was normalized as the body of a binder of its own,
      -- which isn't added to the normalized binders
      assertBool "normalized binders" (nullGlobalEnv (_scopedNormalized (_extra st1)))
  ]
 where
  f = testGlobalId "f_G10" "Int -> Int"
  f2 = testGlobalId "f2_G11" "Int -> Int -> Int"
  tm = parseTestTermWith (TestDefs [] [] [f])
  fTm = closedTerm (tm "\\(x :: Int) -> let { !(y :: Int) = 1 } in y")
  f2Tm = closedTerm (tm "\\(x :: Int) (y :: Int) -> (f_G10 :: Int -> Int -> Int) x 1")

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
