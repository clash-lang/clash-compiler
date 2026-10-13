{-|
  Copyright  :  (C) 2026, QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Utilities to write unit tests on scoped Clash Core ("Clash.Core").

  = Inputs in scoped Core

  Tests write their inputs, and the terms they expect, as strings in the
  language of 'Test.Clash.Rewrite.parseTestTerm' (see its section in
  "Test.Clash.Rewrite"), and build them in a /test scope/ ('TestScope'): a
  scope with variables that the test declares, which the strings refer to by
  their identifiers. Binders get names from just above the watermark of the
  scope, in depth-first order, as the conversion from GHC Core gives them.

  A closed term:

  > closedTerm "\\(x :: Int) -> x"

  An open term: 'withTerm' declares the free local variables of a term, in
  the order of their keys (the uniques of their identifiers), type variables
  first, and builds the term in their scope. Here @x_2@ gets name 1 and
  @f_3@ name 2. Terms built in the same scope with 'termIn' refer to the same
  variables, so they can be compared with the result:

  > withTerm "f_3 (x_2 :: Int)" $ \ts tm ->
  >   assertTermIs ts "f_3 x_2" tm

  An explicit scope, whose variables get the names 1, 2, ... in order, unless
  'atRaw' gives one a name of choice:

  > withTestScope [tyVarD "a_1", varD "x_2" "a_1", atRaw 10 (varD "y_3" "Int")] $ \ts ->
  >   termIn ts "\\(z :: a_1) -> x_2"

  Running a transformation in the root context of the scope ('runRewriteIn'),
  or at a subterm ('runRewriteAt'), with global bindings from
  'mkGlobalBindings' if it needs them. Results that refer to new global
  bindings compare after 'globalsByOcc'.

  > withTerm "let { !(x :: Int) = 1 } in 2" $ \ts e -> do
  >   (e', _, changed) <- runRewriteIn def def ts $(asRewriteQ deadCode) e
  >   assertTermIs ts "2" e'
  >   changed @?= True

  Evaluating it in the scope: 'Test.Clash.Core.Evaluator.whnfIn'. Types
  work like terms: 'typeIn', 'closedType' and 'withType'. A Netlist test gets
  the component context of a scope from 'testCompCtx'.

  What the parser can't parse, such as primitives, is built with the builders
  of "Clash.Core.Builder". A parsed term is a builder too:

  > termIn ts (builtTerm (appB (primB (testPrim "f" "Int -> Int")) (testTermB "x_2")))

  = Shadowing

  In scoped Core, a term built in a scope has binders with names above the
  names of the scope. A binder can only clash with a name of its scope once
  the term is 'sink'ed into a larger scope: that is the situation in which a
  function that goes under binders must rename them. A test of shadowing:

  1. builds its input in a scope;
  2. extends the scope with variables that get the names of the first binders
     of the input, and sinks the input into it, with 'withClashing'; or with
     'sunkD', whose name is above the names of all binders;
  3. asserts the precondition, that a binder clashes ('assertClashes');
  4. runs the function, and asserts that the result only mentions variables
     in scope ('assertLintClean'), and is alpha-equivalent to the expected
     term.

  E.g. @let x = 1 in let x = x in x@, where the inner binder shadows the outer
  one in the source, and the outer binder clashes with the new variable @z@:

  > withTerm "let { !(x :: Int) = 1 } in let { !(x :: Int) = x } in x" $ \ts0 e0 ->
  >   withClashing ts0 [varD "z" "Int"] e0 $ \ts e -> do
  >     assertClashes ts e
  >     e' <- deadCodeBottomup ts e
  >     assertLintClean ts e'
  >     assertTermAlphaEq e e'

  An identifier that is bound again, like @x@ above, refers to the inner
  binder in its scope, as in Haskell. The builder gives the two binders
  different names: shadowing in the source of a test is not shadowing in
  scoped Core.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Clash.Core
  ( -- * Test scopes
    TestScope (..)
  , emptyTestScope
  , withTestScope
  , extendTestScope
  , termIn
  , typeIn
  , closedTerm
  , closedType
  , withTerm
  , withTerms
  , withType
    -- ** Parsing, from "Test.Clash.Rewrite"
  , TestTerm (..)
  , TestType (..)
  , ScopeDecl
  , parseTestTerm
  , parseTestType
  , termQQ
  , typeQQ
  , builtTerm
  , builtType
  , tyVarD
  , kindedTyVarD
  , varD
  , atRaw
  , testGlobalId
    -- * Shadowing
  , withClashing
  , sunkD
  , withTypeAndSunk
  , testScopeRaws
  , clashingBinders
  , assertClashes
    -- * Checking results
  , testLintScope
  , assertLintClean
  , assertTermAlphaEq
  , assertTermIs
  , assertTypeIs
    -- * Running scoped transformations in a test scope
  , runRewriteIn
  , runRewriteAt
  , assertRewritesTo
  , assertRewritesAt
  , assertNoRewrite
  , assertNoRewriteAt
  , withBinderClashes
  , withCapture
  , assertCaptureRewritesTo
  , withCurFun
  , withTopEntities
  , assertSpecializesTo
  , mapGlobals
  , newBindings
  , mkGlobalBindings
  , globalsByOcc
  , testCompCtx
  , testPrim
    -- * Converting
  , closeFreeVars
  , sinkInto
  , atScope
  , testTypeEnv
    -- * Assertions
  , nameSetRaws
    -- * Beta reduction
  , betaTm
  , betaTy
    -- * Invariants
  , LintScope
  , lintTerm
  , lowBinders
  , bindersNot
  , binderRaws
  , reopenTerm
  , rewriteLetRecs
  , scrambleBinders
  , forAllScrambled
    -- * Properties
  , withTests
    -- * Running scoped transformations
  , runRewriteClosed
  , Step (..)
  , atPath
    -- * Tests
  , testScopeTests
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, state)
import Control.Monad.Trans.Writer.Strict (Writer, runWriter, tell)
import Data.Default (def)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Hashable as Hashable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Stack (HasCallStack)
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase, (@?=))
import Test.Tasty.QuickCheck
  (Gen, Property, QuickCheckTests (..), Testable, choose, forAllShow, frequency)

import qualified Clash.Core.Literal as C
import qualified Clash.Core.Name as C
import qualified Clash.Core.Basic as C

import Clash.Core.Alpha (aeqTm, aeqTy)
import Clash.Core.Builder
  (appB, primB, runClosedTerm, runClosedType, runTermIn, runTypeIn)
import Clash.Core.FreeVars (HasNameBinder (..), freeTmVars, freeTyVars, freeTyVarsOfType)
import Clash.Core.Internal.Names
  ( TmName (UnsafeTmName), TmNameBinder (UnsafeTmNameBinder), TyName (UnsafeTyName)
  , TyNameBinder (UnsafeTyNameBinder))
import Clash.Core.Internal.Scope
  (Name (UnsafeName), NameBinder (UnsafeNameBinder), nameId, nameOf, scopeWatermark)
import Clash.Core.NameMap
import Clash.Core.Pretty (PrettyOptions (..), showPpr')
import Clash.Core.Subst
import Clash.Core.Syntax
import Clash.Core.Term (mkLams, mkTyLams)
import Clash.Core.TypeEnv (TypeEnv, mkTypeEnv)
import qualified Clash.Netlist.Id as Id
import Clash.Netlist.Types (CompCtx, LocalId (..), compCtxFromList)
import Clash.Rewrite
  ( Rewrite, Transform (..), Frame (LetBinding), pushFrame, rootContext
  , underAlt, underLam, underLet, underLetRec, underTyLam)
import qualified Clash.Rewrite as Scoped
import Clash.Rewrite.Types (RewriteEnv, RewriteMonad, RewriteState (..))
import qualified Clash.Rewrite.Util as Util (changed)
import qualified Clash.Rewrite.Types as C

import Clash.Driver.Types (Binding (..))

import Test.Clash.Rewrite
  ( DeclVar (..), ScopeDecl (..), TestTerm (..), TestType (..),
    assertErrorContains, atRaw, builtTerm, builtType, freeVarType, kindedTyVarD,
    justDataCon, mkGlobalBinding, nothingDataCon, parseTestTerm, parseTestType,
    runRewriteTest, showPprU, termQQ, testGlobalId, testTyConMap, tyVarD, typeQQ,
    varD, vecTcNm, consDc, withCurFun )
import Test.Clash.Rewrite (ghcName)
import Clash.Core.Evaluator.Types (Env (..), PrimReductions (..))
import Test.Clash.Core.Evaluator (whnfIn)
import Test.Clash.Core.Print (globalIdent, showTestTerm)

-- * Test scopes

-- | The scope of a test: a scope with the variables that the test declared
-- ('ScopeDecl'), by key (see 'Clash.Core.Builder.Key'), so that terms
-- parsed from strings can refer to them, see 'termIn'.
data TestScope n = TestScope
  { testScope :: !(Scope n)
  , testTyVars :: !(IntMap (Type n))
  -- ^ The type variables, by key
  , testVars :: !(IntMap (Term n))
  -- ^ The term variables, by key
  }

-- | The empty scope, to build closed terms in
emptyTestScope :: TestScope 'VoidS
emptyTestScope = TestScope emptyScope IntMap.empty IntMap.empty

-- | A scope with the given variables, in order: the kind or type of each may
-- mention the ones before it. Each gets the next name (the first gets 1),
-- unless it is declared 'atRaw'.
withTestScope
  :: HasCallStack => [ScopeDecl] -> (forall n. Distinct n => TestScope n -> r) -> r
withTestScope = extendTestScope emptyTestScope

-- | Extend a scope with more variables, see 'withTestScope'. Values built in
-- the original scope can be 'sink'ed into the extended one, where their
-- binders may clash with the new variables, see 'withClashing'.
extendTestScope
  :: forall n r
   . (HasCallStack, Distinct n)
  => TestScope n
  -> [ScopeDecl]
  -> (forall l. DExt n l => TestScope l -> r)
  -> r
extendTestScope ts [] k = k ts
extendTestScope ts (d : ds) k =
  declare ts d $ \(ts1 :: TestScope i) ->
    extendTestScope ts1 ds $ \(ts2 :: TestScope l) ->
      withExtTrans @n @i @l (k ts2)

-- | Extend a scope with a variable
declare
  :: (HasCallStack, Distinct n)
  => TestScope n -> ScopeDecl -> (forall l. DExt n l => TestScope l -> r) -> r
declare ts@(TestScope sc tys tms) (ScopeDecl raw v) k = case v of
  DeclTyVar key nm ki ->
    withFreshTyBinder sc0 nm (typeIn ts ki) $ \sc' b ->
      k (TestScope sc' (IntMap.insert key (tyBinderVar b) (sink1 tys)) (sink1 tms))
  DeclId key nm ty ->
    withFreshIdBinder sc0 nm (maybe freeVarTy (typeIn ts) ty) $ \sc' b ->
      k (TestScope sc' (sink1 tys) (IntMap.insert key (idBinderVar b) (sink1 tms)))
 where
  sc0 = case raw of
    Nothing -> sc
    Just r
      | r > scopeWatermark sc -> raiseWatermark (r - 1) sc
      | otherwise -> error
          ("atRaw: the raw name " <> show r <> " is not above the names in scope, up to "
            <> show (scopeWatermark sc))

  freeVarTy = ConstTy (C.TyCon freeVarType)

-- | Build a term in a scope. Its free variables must be variables of the
-- scope; its binders get names from just above the watermark of the scope.
termIn :: (HasCallStack, Distinct n) => TestScope n -> TestTerm -> Term n
termIn (TestScope sc tys tms) t = runTermIn sc tys tms (testTermB t)

-- | Build a type in a scope, see 'termIn'
typeIn :: (HasCallStack, Distinct n) => TestScope n -> TestType -> Type n
typeIn (TestScope sc tys _) t = runTypeIn sc tys (testTypeB t)

-- | Build a closed term
closedTerm :: HasCallStack => TestTerm -> Term 'VoidS
closedTerm = runClosedTerm . testTermB

-- | Build a closed type
closedType :: HasCallStack => TestType -> Type 'VoidS
closedType = runClosedType . testTypeB

-- | Build a term in a scope with its free variables, see 'testTermFree'
withTerm
  :: HasCallStack
  => TestTerm -> (forall n. Distinct n => TestScope n -> Term n -> r) -> r
withTerm t k = withTerms [t] $ \ts -> \case
  [tm] -> k ts tm
  _ -> error "withTerm: impossible"

-- | Build a type in a scope with its free type variables, see 'testTypeFree'
withType
  :: HasCallStack
  => TestType -> (forall n. Distinct n => TestScope n -> Type n -> r) -> r
withType t k = withTestScope (testTypeFree t) $ \ts -> k ts (typeIn ts t)

-- | Build terms in a scope with the free variables of all of them, see
-- 'testTermFree'. A variable gets the type of the first of its occurrences
-- that has one.
withTerms
  :: HasCallStack
  => [TestTerm] -> (forall n. Distinct n => TestScope n -> [Term n] -> r) -> r
withTerms tms k =
  withTestScope (mergeFree (concatMap testTermFree tms)) $ \ts -> k ts (map (termIn ts) tms)
 where
  mergeFree ds =
    IntMap.elems (IntMap.fromListWith (\_ d -> d) [ (key, d) | d@(ScopeDecl _ (DeclTyVar key _ _)) <- ds ]) <>
    IntMap.elems (IntMap.fromListWith typed [ (key, d) | d@(ScopeDecl _ (DeclId key _ _)) <- ds ])

  -- The first declaration with a type, given a later and an earlier one
  typed later earlier = case earlier of
    ScopeDecl _ (DeclId _ _ Nothing) -> later
    _ -> earlier

-- * Shadowing

-- | Sink a term, or a type, into its scope extended with the given variables. Its
-- binders keep their names, so the first ones clash with the new variables:
-- the new variables get the names just above the watermark of the scope, and
-- so do the first binders of a term built there ('termIn'). A function that
-- goes under these binders in the extended scope must rename them. So this
-- builds the scenarios of shadowing, see the module header.
withClashing
  :: (HasCallStack, Distinct n, Sinkable e)
  => TestScope n
  -> [ScopeDecl]
  -> e n
  -> (forall l. DExt n l => TestScope l -> e l -> r)
  -> r
withClashing ts ds tm k = extendTestScope ts ds $ \ts' -> k ts' (sink tm)

-- | A variable with a raw name above the names of the binders of the terms of
-- the tests, of type @Int@: every binder of a term in a scope with it is below
-- the watermark of the scope, so going under it renames it.
sunkD :: ScopeDecl
sunkD = atRaw 1000000 (varD "sunk_S1000000" "Int")

-- | Run an assertion on a type in the scope of its free variables (see
-- 'withType'), and again sunk into that scope extended with 'sunkD', where
-- all of its binders are below the watermark of the scope. So a function that
-- goes under the binders of the type must rename them in the second run.
withTypeAndSunk
  :: HasCallStack
  => TestType -> (forall n. Distinct n => TestScope n -> Type n -> Assertion) -> Assertion
withTypeAndSunk t k = withType t $ \ts ty -> do
  k ts ty
  withClashing ts [sunkD] ty k

-- | The raw names of the variables of a scope
testScopeRaws :: TestScope n -> IntSet
testScopeRaws (TestScope _ tys tms) =
  IntSet.fromList ([tyNameRaw n | VarTy _ _ n <- IntMap.elems tys] <> [tmNameRaw n | Var _ _ n <- IntMap.elems tms])

-- | The raw names of the binders of a term that are names of variables of the
-- scope, see 'binderRaws'
clashingBinders :: TestScope n -> Term n -> [Int]
clashingBinders ts tm = filter (`IntSet.member` testScopeRaws ts) (binderRaws tm)

-- | Assert that some binder of a term clashes with the variables of the
-- scope: the precondition of a test of shadowing, see 'withClashing'
assertClashes :: HasCallStack => TestScope n -> Term n -> Assertion
assertClashes ts tm =
  assertBool ("no binder clashes with the variables of the scope in:\n" <> showPprU tm)
    (not (null (clashingBinders ts tm)))

-- * Checking results

-- | The names of the variables of a scope, for 'lintTerm'
testLintScope :: TestScope n -> LintScope
testLintScope (TestScope _ tys tms) =
  LintScope
    (IntSet.fromList [tyNameRaw n | VarTy _ _ n <- IntMap.elems tys])
    (IntSet.fromList [tmNameRaw n | Var _ _ n <- IntMap.elems tms])

-- | Assert that a term in a scope only mentions its variables, and its own
-- bound ones, see 'lintTerm'
assertLintClean :: HasCallStack => TestScope n -> Term n -> Assertion
assertLintClean ts tm = case lintTerm (testLintScope ts) tm of
  [] -> pure ()
  errs -> assertFailure (unlines errs <> "in:\n" <> showPprU tm)

-- | Assert that two terms are alpha-equivalent, see
-- 'Clash.Core.Alpha.aeqTm'. Shows both, with their raw names, if they
-- aren't.
assertTermAlphaEq :: HasCallStack => Term n -> Term n -> Assertion
assertTermAlphaEq expected actual =
  assertBool
    ("Expected (up to alpha equivalence):\n" <> showTermT expected
      <> "\nbut got:\n" <> showTermT actual
      <> "\nwhich is, in the language of the tests:\n" <> showTestTerm actual)
    (aeqTm expected actual)

-- | Pretty print a term like 'showPprU', with its ticks, which
-- 'Clash.Core.Alpha.aeqTm' compares
showTermT :: Term n -> String
showTermT = showPpr' PrettyOptions
  { displayUniques = True
  , displayTypes = False
  , displayQualifiers = False
  , displayTicks = True
  }

-- | Assert that a term is alpha-equivalent to the given one, built in the
-- same scope
assertTermIs :: (HasCallStack, Distinct n) => TestScope n -> TestTerm -> Term n -> Assertion
assertTermIs ts expected = assertTermAlphaEq (termIn ts expected)

-- | Assert that a type is alpha-equivalent to the given one, built in the
-- same scope
assertTypeIs :: (HasCallStack, Distinct n) => TestScope n -> TestType -> Type n -> Assertion
assertTypeIs ts expected0 actual =
  assertBool
    ("Expected (up to alpha equivalence):\n" <> showPprU expected
      <> "\nbut got:\n" <> showPprU actual)
    (aeqTy expected actual)
 where
  expected = typeIn ts expected0

-- * Running transformations

-- | Run a scoped transformation on a term, in the root context of its scope.
-- Gives the result, the final state, and whether a change was signaled.
runRewriteIn
  :: Distinct n
  => RewriteEnv
  -> RewriteState extra
  -> TestScope n
  -> Rewrite extra
  -> Term n
  -> IO (Term n, RewriteState extra, Bool)
runRewriteIn env st ts t e =
  runRewriteTest env st (runTransform t (rootContext (testScope ts)) e)

-- | Run a scoped transformation on the subterm at a path (see 'atPath'), in
-- its context, and put the result back. Gives the whole term, the final state,
-- and whether a change was signaled. The binders on the path are opened, so
-- they may be renamed. E.g. to run a transformation in the body of a lambda:
--
-- > withTerm "\\(x :: Int) -> f_1 x" $ \ts e -> do
-- >   (e', _, _) <- runRewriteAt def def ts [LamStep] topLetR e
-- >   assertTermIs ts "\\(x :: Int) -> let { (r :: Int) = f_1 x } in r" e'
runRewriteAt
  :: forall n extra
   . (HasCallStack, Distinct n)
  => RewriteEnv
  -> RewriteState extra
  -> TestScope n
  -> [Step]
  -> Rewrite extra
  -> Term n
  -> IO (Term n, RewriteState extra, Bool)
runRewriteAt env st ts path0 t e0 =
  runRewriteTest env st (go path0 (rootContext (testScope ts)) e0)
 where
  go :: Distinct x => [Step] -> Scoped.TransformContext x -> Term x -> RewriteMonad extra (Term x)
  go [] ctx e = runTransform t ctx e
  go (step : path) ctx e = case (step, e) of
    (LamStep, Lam b body) -> underLam ctx b body $ \ctx' b' body' -> Lam b' <$> go path ctx' body'
    (TyLamStep, TyLam b body) -> underTyLam ctx b body $ \ctx' b' body' -> TyLam b' <$> go path ctx' body'
    (LetBodyStep, Let bind body) -> underLet ctx bind body $ \ctx' bind' body' -> Let bind' <$> go path ctx' body'
    (LetRhsStep 0, Let (NonRec b x) body) ->
      (\x' -> Let (NonRec b x') body) <$> go path (pushFrame (LetBinding b) ctx) x
    (LetRhsStep i, Let (Rec bs xs) body) | i < length xs ->
      underLetRec ctx bs xs body $ \_ ctxRhs bs' xs' body' -> do
        x' <- go path (ctxRhs i) (xs' !! i)
        pure (Let (Rec bs' (replaceAt i x' xs')) body')
    (AltStep i, Case subj ty alts) | i < length alts -> do
      alt' <- underAlt ctx (alts !! i) $ \ctx' p rhs -> Alt p <$> go path ctx' rhs
      pure (Case subj ty (replaceAt i alt' alts))
    (AppFunStep, App f a) -> (`App` a) <$> go path (pushFrame Scoped.AppFun ctx) f
    (AppArgStep, App f a) -> App f <$> go path (pushFrame (Scoped.appArgFrame f) ctx) a
    (TickStep, Tick tk a) -> Tick tk <$> go path (pushFrame (Scoped.TickC tk) ctx) a
    (CastStep, Cast a t1 t2) -> (\a' -> Cast a' t1 t2) <$> go path (pushFrame Scoped.CastBody ctx) a
    (SubjStep, Case a ty alts) -> (\a' -> Case a' ty alts) <$> go path (pushFrame Scoped.CaseScrut ctx) a
    _ -> error ("runRewriteAt: " <> show step <> " doesn't fit the term:\n" <> showPprU e)

  replaceAt :: Int -> a -> [a] -> [a]
  replaceAt i x xs = take i xs <> [x] <> drop (i + 1) xs

-- | Assert that a scoped transformation, run at the root of a term in a
-- scope with the given variables, gives the expected term (built in the same
-- scope), with a lint-clean result, and that it signals a change. Runs it twice:
-- in that scope, and in the scope extended such that the binders of the term
-- clash with its variables ('withBinderClashes'), so that a transformation that
-- moves a term under one of them has to rename it.
assertRewritesTo
  :: HasCallStack
  => RewriteEnv
  -> RewriteState extra
  -> Rewrite extra
  -> [ScopeDecl]
  -> TestTerm
  -- ^ The term
  -> TestTerm
  -- ^ The expected result
  -> Assertion
assertRewritesTo env st = assertRewritesAt env st []

-- | 'assertRewritesTo', for a transformation run at the subterm at a path
-- (see 'runRewriteAt')
assertRewritesAt
  :: HasCallStack
  => RewriteEnv
  -> RewriteState extra
  -> [Step]
  -> Rewrite extra
  -> [ScopeDecl]
  -> TestTerm
  -> TestTerm
  -> Assertion
assertRewritesAt env st path rw decls input expected =
  withTestScope decls $ \ts0 -> do
    let e0 = termIn ts0 input
    check ts0 e0
    withBinderClashes ts0 e0 check
 where
  check :: Distinct n => TestScope n -> Term n -> Assertion
  check ts e = do
    (e', _, ch) <- runRewriteAt env st ts path rw e
    assertLintClean ts e'
    assertTermIs ts expected e'
    assertBool "no change signaled" ch

-- | Assert that a scoped transformation, run at the root of a term in a
-- scope with the given variables, leaves it as it is and signals no change.
-- Like 'assertRewritesTo', also in a scope that the binders of the term clash
-- with.
assertNoRewrite
  :: HasCallStack
  => RewriteEnv
  -> RewriteState extra
  -> Rewrite extra
  -> [ScopeDecl]
  -> TestTerm
  -> Assertion
assertNoRewrite env st = assertNoRewriteAt env st []

-- | 'assertNoRewrite', for a transformation run at the subterm at a path (see
-- 'runRewriteAt')
assertNoRewriteAt
  :: HasCallStack
  => RewriteEnv
  -> RewriteState extra
  -> [Step]
  -> Rewrite extra
  -> [ScopeDecl]
  -> TestTerm
  -> Assertion
assertNoRewriteAt env st path rw decls input =
  withTestScope decls $ \ts0 -> do
    let e0 = termIn ts0 input
    check ts0 e0
    withBinderClashes ts0 e0 check
 where
  check :: Distinct n => TestScope n -> Term n -> Assertion
  check ts e = do
    (e', _, ch) <- runRewriteAt env st ts path rw e
    assertTermAlphaEq e e'
    assertBool "a change signaled" (not ch)

-- | Run an assertion on a term sunk into its scope extended with a variable
-- for each of its binders, of type @Int@ (see 'withClashing'), after
-- asserting that its binders clash with them. Does nothing if the term has no
-- binders.
withBinderClashes
  :: (HasCallStack, Distinct n)
  => TestScope n
  -> Term n
  -> (forall l. Distinct l => TestScope l -> Term l -> Assertion)
  -> Assertion
withBinderClashes ts e k
  | null (binderRaws e) = pure ()
  | otherwise =
      withClashing ts [varD ("clash_" <> show i) "Int" | i <- [900001 .. 900000 + length (binderRaws e)]] e $ \ts' e' -> do
        assertClashes ts' e'
        k ts' e'

-- | An environment in which the given globals are the top entities
-- ('Clash.Rewrite.Types.topEntities')
withTopEntities :: [GlobalId] -> RewriteEnv -> RewriteEnv
withTopEntities gs env = env {C._topEntities = globalSetFromList gs}

-- | The scenario of a capture bug: a term with a binder that has the name of a
-- variable that the term mentions outside of the scope of the binder. Builds a
-- part of the term in a scope with the given variables, then extends the scope
-- with more variables, which get the names of the first binders of that part
-- (see 'withClashing'), and builds the term from the part, sunk into the
-- extended scope, where it can mention the new variables. Asserts that the
-- term is such a scenario, and runs an assertion on it.
--
-- E.g. a binder of the inner case-expression of @case (case a of {MkT x ->
-- f}) of {_ -> x}@ that has the name of the free @x@:
--
-- > withCapture [varD "a" "T", varD "f" "Int -> Int"]
-- >   "(case a of { MkT (x :: Int) -> f }) :: Int -> Int"
-- >   [varD "x_3" "Int"]
-- >   (\ts inner -> Case inner (typeIn ts "Int") [Alt DefaultPat (termIn ts "x_3")])
-- >   $ \ts e -> ..
withCapture
  :: HasCallStack
  => [ScopeDecl]
  -- ^ The variables of the scope of the part
  -> TestTerm
  -- ^ The part
  -> [ScopeDecl]
  -- ^ The variables that the scope is extended with
  -> (forall l. Distinct l => TestScope l -> Term l -> Term l)
  -- ^ Build the term from the part, in the extended scope
  -> (forall l. Distinct l => TestScope l -> Term l -> Assertion)
  -> Assertion
withCapture decls part new mk k =
  withTestScope decls $ \ts0 ->
    withClashing ts0 new (termIn ts0 part) $ \ts partL -> do
      let e = mk ts partL
          free = IntSet.fromList (nameSetRaws (freeTmVars e) <> nameSetRaws (freeTyVars e))
      assertBool ("no binder has the name of a variable that the term mentions:\n" <> showPprU e)
        (any (`IntSet.member` free) (binderRaws e))
      k ts e

-- | Assert that a scoped transformation, run at the root of the term of a
-- capture scenario (see 'withCapture'), gives the expected term, built in the
-- extended scope, with a lint-clean result
assertCaptureRewritesTo
  :: HasCallStack
  => RewriteEnv
  -> RewriteState extra
  -> Rewrite extra
  -> [ScopeDecl]
  -> TestTerm
  -> [ScopeDecl]
  -> (forall l. Distinct l => TestScope l -> Term l -> Term l)
  -> TestTerm
  -- ^ The expected result
  -> Assertion
assertCaptureRewritesTo env st rw decls part new mk expected =
  withCapture decls part new mk $ \ts e -> do
    (e', _, _) <- runRewriteIn env st ts rw e
    assertLintClean ts e'
    assertTermIs ts expected e'

-- | The global bindings ('Clash.Rewrite.Types.bindings') of the second
-- state that the first doesn't have
newBindings :: RewriteState extra -> RewriteState extra -> [Binding (Term 'VoidS)]
newBindings st0 st1 =
  [ b | b <- globalEnvElems (_bindings st1)
      , Maybe.isNothing (lookupGlobalEnv (bindingId b) (_bindings st0)) ]

-- | Assert that a scoped transformation, run at the root of a term in a
-- scope with the given variables, creates new global bindings, and gives the
-- expected term. The expected term refers to the new global functions by
-- placeholders, given with the expected bodies of their bindings, in the
-- order of creation (by unique). A placeholder has the name of the new
-- function, see 'Test.Clash.Core.Print.globalIdent', and any unique. Like
-- 'assertRewritesTo', it also runs in a scope that the binders of the term
-- clash with.
assertSpecializesTo
  :: HasCallStack
  => RewriteEnv
  -> RewriteState extra
  -> Rewrite extra
  -> [ScopeDecl]
  -> TestTerm
  -- ^ The term
  -> TestTerm
  -- ^ The expected result
  -> [(GlobalId, TestTerm)]
  -- ^ The placeholders of the new global functions, and their expected bodies
  -> Assertion
assertSpecializesTo env st rw decls input expected newFuns =
  withTestScope decls $ \ts0 -> do
    let e0 = termIn ts0 input
    check ts0 e0
    withBinderClashes ts0 e0 check
 where
  check :: Distinct n => TestScope n -> Term n -> Assertion
  check ts e = do
    (e', st1, ch) <- runRewriteIn env st ts rw e
    let new = List.sortOn (globalIdUniq . bindingId) (newBindings st st1)
        rename g = maybe g snd (List.find ((== globalIdUniq g) . globalIdUniq . fst)
                     (zip (map bindingId new) (map fst newFuns)))
    assertEqual "the names of the new functions"
      [ C.nameOcc (globalIdName g) | (g, _) <- newFuns ]
      [ Text.pack (globalIdent (bindingId b)) | b <- new ]
    assertLintClean ts e'
    assertTermIs ts expected (mapGlobals rename e')
    sequence_
      [ assertTermIs emptyTestScope body (mapGlobals rename (bindingTerm b))
      | (b, (_, body)) <- zip new newFuns ]
    assertBool "no change signaled" ch

-- | Apply a function to every global variable of a term
mapGlobals :: (GlobalId -> GlobalId) -> Term n -> Term n
mapGlobals f = go
 where
  go :: Term x -> Term x
  go = \case
    GlobalVar g -> GlobalVar (f g)
    Lam b e -> Lam b (go e)
    TyLam b e -> TyLam b (go e)
    App e1 e2 -> App (go e1) (go e2)
    TyApp e ty -> TyApp (go e) ty
    Let (NonRec b x) e -> Let (NonRec b (go x)) (go e)
    Let (Rec bs xs) e -> Let (Rec bs (map go xs)) (go e)
    Case e ty alts -> Case (go e) ty [Alt p (go rhs) | Alt p rhs <- alts]
    Cast e t1 t2 -> Cast (go e) t1 t2
    Tick (Attributes ty x) e -> Tick (Attributes ty (go x)) (go e)
    Tick t e -> Tick t (go e)
    e -> e

-- | Give every global variable a unique derived from its occurrence name, so
-- that terms that refer to new globals made by different runs, which get
-- different uniques, can be compared.
globalsByOcc :: Term n -> Term n
globalsByOcc = go
 where
  go :: Term x -> Term x
  go = \case
    GlobalVar g ->
      let nm = globalIdName g
          u = fromIntegral (Hashable.hash (C.nameOcc nm))
      in  GlobalVar g {globalIdName = nm {C.nameUniq = u}}
    Lam b e -> Lam b (go e)
    TyLam b e -> TyLam b (go e)
    App e1 e2 -> App (go e1) (go e2)
    TyApp e ty -> TyApp (go e) ty
    Let (NonRec b x) e -> Let (NonRec b (go x)) (go e)
    Let (Rec bs xs) e -> Let (Rec bs (map go xs)) (go e)
    Case e ty alts -> Case (go e) ty [Alt p (go rhs) | Alt p rhs <- alts]
    Cast e t1 t2 -> Cast (go e) t1 t2
    Tick (Attributes ty x) e -> Tick (Attributes ty (go x)) (go e)
    Tick t e -> Tick t (go e)
    e -> e

-- | The context of a component whose local variables are the term variables
-- of a test scope, with their occurrence names as their HDL identifiers. The
-- context has the type environment 'testTypeEnv', like
-- 'Test.Clash.Rewrite.runNetlistTest'.
testCompCtx :: Distinct n => TestScope n -> CompCtx n
testCompCtx ts =
  compCtxFromList (testScope ts) testTypeEnv
    [ (LocalId o ty n, Id.unsafeMake (C.nameOcc o)) | Var o ty n <- IntMap.elems (testVars ts) ]

-- | A primitive with the given name and closed type, which does no work, has
-- a single result, and no unfolding. The parser doesn't parse primitives, so
-- tests build them with 'Clash.Core.Builder.primB':
--
-- > termIn ts (builtTerm (appB (primB (testPrim "f" "Int -> Int")) (testTermB "x_1")))
testPrim :: HasCallStack => Text -> TestType -> PrimInfo
testPrim nm ty = mkPrimInfo (ghcName nm) (closedType ty) C.WorkConstant C.SingleResult NoUnfolding

-- | Global bindings of closed terms, for 'Clash.Rewrite.Types.bindings'
mkGlobalBindings :: HasCallStack => [(GlobalId, TestTerm)] -> GlobalEnv (Binding (Term 'VoidS))
mkGlobalBindings bs = globalEnvFromList [(g, mkGlobalBinding g (closedTerm t)) | (g, t) <- bs]

-- * Converting

-- | The environment of the types of 'testTyConMap'
testTypeEnv :: TypeEnv
testTypeEnv = mkTypeEnv testTyConMap

-- | Abstract a term over its free variables: type lambdas for the type
-- variables, in the order of their raw names, and lambdas for the term
-- variables inside those. The kind of a type variable may only mention type
-- variables with smaller raw names, as in the scopes of 'withTestScope'.
closeFreeVars :: HasCallStack => Term n -> Term 'VoidS
closeFreeVars tm =
  case closeOver tys tms (\_ tvTele idTele cl -> (\t -> mkTyLams (mkLams t idTele) tvTele) <$> closureTerm cl tm) of
    Just (Just closed) -> closed
    _ -> error "closeFreeVars: can't close over the free variables"
 where
  (tys0, tms) = freeVarOccs tm
  tys = List.sortOn (\case VarTy _ _ n -> tyNameRaw n; _ -> 0) tys0

-- | The raw names in a set, in ascending order
nameSetRaws :: ScopedName k => NameSet k n -> [Int]
nameSetRaws = map scopedNameRaw . nameSetToList

-- | Beta-reduce an application of a lambda. Errors on other terms.
betaTm :: (HasCallStack, Distinct n) => Scope n -> Term n -> Term n
betaTm sc = \case
  App (Lam b e) arg -> substTm (extendTmSubst (idSubst sc) b arg) e
  _ -> error "betaTm: not an application of a lambda"

-- | Beta-reduce a type application of a type lambda. Errors on other terms.
betaTy :: (HasCallStack, Distinct n) => Scope n -> Term n -> Term n
betaTy sc = \case
  TyApp (TyLam b e) ty -> substTm (extendTySubst (idSubst sc) b ty) e
  _ -> error "betaTy: not a type application of a type lambda"

-- * Generated free variables

-- | 'sink' into the given scope
sinkInto :: (Sinkable e, DExt n l) => Scope l -> e n -> e l
sinkInto _ = sink

-- | A value in the scope of the given scope, e.g. to fix the scope of
-- 'sinkClosed'
atScope :: Scope n -> e n -> e n
atScope _ e = e

-- * Invariants

-- | The raw names of the type and of the term variables in scope, for
-- 'lintTerm'
data LintScope = LintScope !IntSet !IntSet

bindLintTy :: Int -> LintScope -> LintScope
bindLintTy k (LintScope tys tms) = LintScope (IntSet.insert k tys) (IntSet.delete k tms)

bindLintTm :: Int -> LintScope -> LintScope
bindLintTm k (LintScope tys tms) = LintScope (IntSet.delete k tys) (IntSet.insert k tms)

-- | The raw name a binder binds
binderRaw :: HasNameBinder b => b n l -> Int
binderRaw = nameId . nameOf . nameBinderOf

-- | Problems with the names of a term in a scope with the given variables:
-- occurrences of names that are not in scope, or that are not of their sort.
-- A binder makes a name of the other sort inaccessible. Looks at all types,
-- including those of variable occurrences.
lintTerm :: LintScope -> Term n -> [String]
lintTerm ls0 = goTm ls0
 where
  goTy :: LintScope -> Type x -> [String]
  goTy ls@(LintScope tys _) = \case
    VarTy _ k n ->
      ["type variable " <> show (tyNameRaw n) <> " not in scope" | not (IntSet.member (tyNameRaw n) tys)]
        <> goTy ls k
    ForAllTy b t -> goTy ls (tyBinderKind b) <> goTy (bindLintTy (binderRaw b) ls) t
    AppTy t1 t2 -> goTy ls t1 <> goTy ls t2
    AnnType _ t -> goTy ls t
    ConstTy {} -> []
    LitTy {} -> []

  goTm :: LintScope -> Term x -> [String]
  goTm ls@(LintScope _ tms) = \case
    Var _ ty n ->
      ["term variable " <> show (tmNameRaw n) <> " not in scope" | not (IntSet.member (tmNameRaw n) tms)]
        <> goTy ls ty
    GlobalVar {} -> []
    Data {} -> []
    Literal {} -> []
    Prim {} -> []
    Lam b e -> goTy ls (idBinderType b) <> goTm (bindLintTm (binderRaw b) ls) e
    TyLam b e -> goTy ls (tyBinderKind b) <> goTm (bindLintTy (binderRaw b) ls) e
    App e1 e2 -> goTm ls e1 <> goTm ls e2
    TyApp e ty -> goTm ls e <> goTy ls ty
    Let (NonRec b x) e ->
      goTy ls (idBinderType b) <> goTm ls x <> goTm (bindLintTm (binderRaw b) ls) e
    Let (Rec bs xs) e -> goIdTele ls bs (\ls' -> concatMap (goTm ls') (e:xs))
    Case e ty alts -> goTm ls e <> goTy ls ty <> concatMap (goAlt ls) alts
    Cast e t1 t2 -> goTm ls e <> goTy ls t1 <> goTy ls t2
    Tick t e -> goTick ls t <> goTm ls e

  goAlt :: LintScope -> Alt x -> [String]
  goAlt ls (Alt p e) = case p of
    DataPat _ tvs ids -> goTyTele ls tvs (\ls1 -> goIdTele ls1 ids (\ls2 -> goTm ls2 e))
    LitPat _ -> goTm ls e
    DefaultPat -> goTm ls e

  goTick :: LintScope -> TickInfo x -> [String]
  goTick ls = \case
    NameMod _ ty -> goTy ls ty
    Attributes ty e -> goTy ls ty <> goTm ls e
    _ -> []

  goIdTele :: LintScope -> Tele IdBinder x y -> (LintScope -> [String]) -> [String]
  goIdTele ls TNil k = k ls
  goIdTele ls (TCons b bs) k = goTy ls (idBinderType b) <> goIdTele (bindLintTm (binderRaw b) ls) bs k

  goTyTele :: LintScope -> Tele TyBinder x y -> (LintScope -> [String]) -> [String]
  goTyTele ls TNil k = k ls
  goTyTele ls (TCons b bs) k = goTy ls (tyBinderKind b) <> goTyTele (bindLintTy (binderRaw b) ls) bs k

-- | The binders of a term that are not above the watermark of the scope at
-- them, given the watermark of the scope of the term. Includes the binders of
-- the types in the term, except those of the types of variable occurrences,
-- which are those of their binders.
--
-- Values that the builders of "Clash.Core.Builder" build, and
-- 'freshen'ed ones, have no such binders. Others may: e.g. a value 'sink'ed
-- into a larger scope.
lowBinders :: Int -> Term n -> [Int]
lowBinders = bindersNot (<)

-- | The binders of a term whose names don't satisfy the predicate, given the
-- watermark of the scope at them, and that of the scope of the term. See
-- 'lowBinders'.
bindersNot :: (Int -> Int -> Bool) -> Int -> Term n -> [Int]
bindersNot ok = goTm
 where
  check :: Int -> Int -> [Int]
  check w k = [k | not (ok w k)]

  goTy :: Int -> Type x -> [Int]
  goTy w = \case
    VarTy {} -> []
    ForAllTy b t -> goTy w (tyBinderKind b) <> check w (binderRaw b) <> goTy (max w (binderRaw b)) t
    AppTy t1 t2 -> goTy w t1 <> goTy w t2
    AnnType _ t -> goTy w t
    ConstTy {} -> []
    LitTy {} -> []

  goTm :: Int -> Term x -> [Int]
  goTm w = \case
    Var {} -> []
    GlobalVar {} -> []
    Data {} -> []
    Literal {} -> []
    Prim {} -> []
    Lam b e -> goTy w (idBinderType b) <> check w (binderRaw b) <> goTm (max w (binderRaw b)) e
    TyLam b e -> goTy w (tyBinderKind b) <> check w (binderRaw b) <> goTm (max w (binderRaw b)) e
    App e1 e2 -> goTm w e1 <> goTm w e2
    TyApp e ty -> goTm w e <> goTy w ty
    Let (NonRec b x) e ->
      goTm w x <> goTy w (idBinderType b) <> check w (binderRaw b) <> goTm (max w (binderRaw b)) e
    Let (Rec bs xs) e -> goTele idBinderType bs w (\w' -> concatMap (goTm w') (e:xs))
    Case e ty alts -> goTm w e <> goTy w ty <> concatMap (goAlt w) alts
    Cast e t1 t2 -> goTm w e <> goTy w t1 <> goTy w t2
    Tick t e -> goTick w t <> goTm w e

  goAlt :: Int -> Alt x -> [Int]
  goAlt w (Alt p e) = case p of
    DataPat _ tvs ids -> goTele tyBinderKind tvs w (\w1 -> goTele idBinderType ids w1 (\w2 -> goTm w2 e))
    LitPat _ -> goTm w e
    DefaultPat -> goTm w e

  goTick :: Int -> TickInfo x -> [Int]
  goTick w = \case
    NameMod _ ty -> goTy w ty
    Attributes ty e -> goTy w ty <> goTm w e
    _ -> []

  goTele
    :: HasNameBinder b
    => (forall x' y'. b x' y' -> Type x') -> Tele b x y -> Int -> (Int -> [Int]) -> [Int]
  goTele _ TNil w k = k w
  goTele payload (TCons b bs) w k =
    goTy w (payload b) <> check w (binderRaw b) <> goTele payload bs (max w (binderRaw b)) k

-- | The raw names of the binders of a term, in the order in which the builders
-- of "Clash.Core.Builder" number them: depth-first, left to right, a
-- binder after the binders in its type, and a non-recursive let-binding after
-- its right-hand side. Leaves out the binders of the types of variable
-- occurrences, which are those of their binders.
binderRaws :: Term n -> [Int]
binderRaws = goTm
 where
  goTy :: Type x -> [Int]
  goTy = \case
    VarTy {} -> []
    ForAllTy b t -> goTy (tyBinderKind b) <> [binderRaw b] <> goTy t
    AppTy t1 t2 -> goTy t1 <> goTy t2
    AnnType _ t -> goTy t
    ConstTy {} -> []
    LitTy {} -> []

  goTm :: Term x -> [Int]
  goTm = \case
    Var {} -> []
    GlobalVar {} -> []
    Data {} -> []
    Literal {} -> []
    Prim {} -> []
    Lam b e -> goTy (idBinderType b) <> [binderRaw b] <> goTm e
    TyLam b e -> goTy (tyBinderKind b) <> [binderRaw b] <> goTm e
    App e1 e2 -> goTm e1 <> goTm e2
    TyApp e ty -> goTm e <> goTy ty
    Let (NonRec b x) e -> goTm x <> goTy (idBinderType b) <> [binderRaw b] <> goTm e
    Let (Rec bs xs) e -> goTele idBinderType bs <> concatMap goTm xs <> goTm e
    Case e ty alts -> goTm e <> goTy ty <> concatMap goAlt alts
    Cast e t1 t2 -> goTm e <> goTy t1 <> goTy t2
    Tick t e -> goTick t <> goTm e

  goAlt :: Alt x -> [Int]
  goAlt (Alt p e) = case p of
    DataPat _ tvs ids -> goTele tyBinderKind tvs <> goTele idBinderType ids <> goTm e
    LitPat _ -> goTm e
    DefaultPat -> goTm e

  goTick :: TickInfo x -> [Int]
  goTick = \case
    NameMod _ ty -> goTy ty
    Attributes ty e -> goTy ty <> goTm e
    _ -> []

  goTele :: HasNameBinder b => (forall x' y'. b x' y' -> Type x') -> Tele b x y -> [Int]
  goTele _ TNil = []
  goTele payload (TCons b bs) = goTy (payload b) <> [binderRaw b] <> goTele payload bs

-- | Rebuild a term, going under every binder with 'openIdBinder',
-- 'openTyBinder', 'openBind' and 'openAlt', also in the types in it. Gives the
-- problems with the binders that these produce: binders that are not above the
-- watermark of the scope, or that the watermark of the extended scope is
-- below. Doesn't go into the types of the binders of telescopes ('Rec' and
-- 'DataPat'), as these are in scopes that the traversal doesn't have, nor into
-- the types of variable occurrences, which are those of their binders (or of
-- free variables).
reopenTerm :: Distinct n => Scope n -> Term n -> ([String], Term n)
reopenTerm sc0 tm0 = swap (runWriter (goTm sc0 tm0))
 where
  swap (a, b) = (b, a)

  checkOpened :: Scope x -> Scope y -> [Int] -> Writer [String] ()
  checkOpened sc sc' ks = do
    let w = scopeWatermark sc
        w' = scopeWatermark sc'
    tell ["binder " <> show k <> " not above the watermark " <> show w | k <- ks, k <= w]
    tell ["binders " <> show ks <> " not increasing" | not (and (zipWith (<) ks (drop 1 ks)))]
    tell [ "watermark " <> show w' <> " of the extended scope is below binder " <> show k
         | k <- ks, k > w' ]

  goTy :: Distinct x => Scope x -> Type x -> Writer [String] (Type x)
  goTy sc = \case
    VarTy occ k n -> VarTy occ <$> goTy sc k <*> pure n
    ForAllTy b t -> openTyBinder sc b t $ \sc' b' t' -> do
      checkOpened sc sc' [binderRaw b']
      k <- goTy sc (tyBinderKind b')
      ForAllTy b' {tyBinderKind = k} <$> goTy sc' t'
    AppTy t1 t2 -> AppTy <$> goTy sc t1 <*> goTy sc t2
    AnnType a t -> AnnType a <$> goTy sc t
    ty@ConstTy {} -> pure ty
    ty@LitTy {} -> pure ty

  goTm :: Distinct x => Scope x -> Term x -> Writer [String] (Term x)
  goTm sc = \case
    tm@Var {} -> pure tm
    tm@GlobalVar {} -> pure tm
    tm@Data {} -> pure tm
    tm@Literal {} -> pure tm
    tm@Prim {} -> pure tm
    Lam b e -> openIdBinder sc b e $ \sc' b' e' -> do
      checkOpened sc sc' [binderRaw b']
      ty <- goTy sc (idBinderType b')
      Lam b' {idBinderType = ty} <$> goTm sc' e'
    TyLam b e -> openTyBinder sc b e $ \sc' b' e' -> do
      checkOpened sc sc' [binderRaw b']
      k <- goTy sc (tyBinderKind b')
      TyLam b' {tyBinderKind = k} <$> goTm sc' e'
    App e1 e2 -> App <$> goTm sc e1 <*> goTm sc e2
    TyApp e ty -> TyApp <$> goTm sc e <*> goTy sc ty
    Let bs e -> openBind sc bs e $ \sc' bs' e' -> case bs' of
      NonRec b x -> do
        checkOpened sc sc' [binderRaw b]
        ty <- goTy sc (idBinderType b)
        x' <- goTm sc x
        Let (NonRec b {idBinderType = ty} x') <$> goTm sc' e'
      Rec bndrs xs -> do
        checkOpened sc sc' (teleRaws bndrs)
        xs' <- mapM (goTm sc') xs
        Let (Rec bndrs xs') <$> goTm sc' e'
    Case e ty alts -> Case <$> goTm sc e <*> goTy sc ty <*> mapM (goAlt sc) alts
    Cast e t1 t2 -> Cast <$> goTm sc e <*> goTy sc t1 <*> goTy sc t2
    Tick t e -> Tick <$> goTick sc t <*> goTm sc e

  goAlt :: Distinct x => Scope x -> Alt x -> Writer [String] (Alt x)
  goAlt sc alt = openAlt sc alt $ \sc' p e -> do
    case p of
      DataPat _ tvs ids -> checkOpened sc sc' (teleRaws tvs <> teleRaws ids)
      _ -> pure ()
    Alt p <$> goTm sc' e

  goTick :: Distinct x => Scope x -> TickInfo x -> Writer [String] (TickInfo x)
  goTick sc = \case
    NameMod m ty -> NameMod m <$> goTy sc ty
    Attributes ty e -> Attributes <$> goTy sc ty <*> goTm sc e
    t -> pure t

  teleRaws :: HasNameBinder b => Tele b x y -> [Int]
  teleRaws TNil = []
  teleRaws (TCons b bs) = binderRaw b : teleRaws bs

-- | Rewrite every recursive let-expression of a term, innermost first, in the
-- scope at it. Goes under binders with 'openIdBinder', 'openTyBinder',
-- 'openBind' and 'openAlt', which rename binders that clash with the scope.
-- The function gets the opened let-expression: its binders, and its
-- right-hand sides and body, already rewritten. Doesn't go into types, nor
-- into ticks.
rewriteLetRecs
  :: forall m n
   . (Monad m, Distinct n)
  => (forall x l. (Distinct x, DExt x l) => Scope x -> Tele IdBinder x l -> [Term l] -> Term l -> m (Term x))
  -> Scope n
  -> Term n
  -> m (Term n)
rewriteLetRecs f = goTm
 where
  goTm :: Distinct x => Scope x -> Term x -> m (Term x)
  goTm sc = \case
    Lam b e -> openIdBinder sc b e $ \sc' b' e' -> Lam b' <$> goTm sc' e'
    TyLam b e -> openTyBinder sc b e $ \sc' b' e' -> TyLam b' <$> goTm sc' e'
    App e1 e2 -> App <$> goTm sc e1 <*> goTm sc e2
    TyApp e ty -> (`TyApp` ty) <$> goTm sc e
    Let bs e -> openBind sc bs e $ \sc' bs' e' -> case bs' of
      NonRec b x -> (\x' e'' -> Let (NonRec b x') e'') <$> goTm sc x <*> goTm sc' e'
      Rec bndrs xs -> do
        xs' <- mapM (goTm sc') xs
        e'' <- goTm sc' e'
        f sc bndrs xs' e''
    Case e ty alts ->
      Case <$> goTm sc e <*> pure ty
        <*> mapM (\alt -> openAlt sc alt $ \sc' p e' -> Alt p <$> goTm sc' e') alts
    Cast e t1 t2 -> (\e' -> Cast e' t1 t2) <$> goTm sc e
    Tick t e -> Tick t <$> goTm sc e
    tm -> pure tm

-- * Scrambling binders

-- | Rename the binders of a term, keeping its meaning: a binder gets a random
-- name, often that of a variable in scope (given the watermark of its scope)
-- or of another binder, of either sort, as long as no occurrence in its scope
-- refers to another variable with that name. Binders of the same telescope
-- keep different names.
--
-- Values like the result arise from sinking: e.g. in @'Lam' b ('sink' v)@,
-- where @b@ is a fresh binder, binders in @v@ may have the name of @b@. The
-- trusted core has to handle them, but conversion never produces them.
scrambleBinders :: Int -> Term n -> Gen (Term n)
scrambleBinders w tm0 = evalStateT (goTm IntMap.empty tm0) 1000000
 where
  rn :: IntMap Int -> Int -> Int
  rn ren k = IntMap.findWithDefault k k ren

  -- A new name for a binder: not one of the forbidden ones
  pick :: IntSet -> Int -> Scramble Int
  pick forbidden old = do
    k <- lift (frequency [(3, choose (1, w + 6)), (1, pure old)])
    if IntSet.member k forbidden
      then if IntSet.member old forbidden then state (\f -> (f, f + 1)) else pure old
      else pure k

  -- The names of a set of names in the scope above a binder, renamed
  outer :: IntMap Int -> [Int] -> IntSet
  outer ren = IntSet.fromList . map (rn ren)

  frees :: Term x -> [Int]
  frees e = nameSetRaws (freeTmVars e) <> nameSetRaws (freeTyVars e)

  freesTy :: Type x -> [Int]
  freesTy = nameSetRaws . freeTyVarsOfType

  tyName :: IntMap Int -> TyName x -> TyName x
  tyName ren n = UnsafeTyName (UnsafeName (rn ren (tyNameRaw n)))

  tmName :: IntMap Int -> TmName x -> TmName x
  tmName ren n = UnsafeTmName (UnsafeName (rn ren (tmNameRaw n)))

  tyBinder :: Int -> TyBinder x y -> Type x -> TyBinder x y
  tyBinder k b kind = b {tyBinderKind = kind, tyBinderName = UnsafeTyNameBinder (UnsafeNameBinder (UnsafeName k))}

  idBinder :: Int -> IdBinder x y -> Type x -> IdBinder x y
  idBinder k b ty = b {idBinderType = ty, idBinderName = UnsafeTmNameBinder (UnsafeNameBinder (UnsafeName k))}

  goTy :: IntMap Int -> Type x -> Scramble (Type x)
  goTy ren = \case
    VarTy occ k n -> VarTy occ <$> goTy ren k <*> pure (tyName ren n)
    ForAllTy b t -> do
      kind <- goTy ren (tyBinderKind b)
      let o = binderRaw b
      k <- pick (outer ren (filter (/= o) (freesTy t))) o
      ForAllTy (tyBinder k b kind) <$> goTy (IntMap.insert o k ren) t
    AppTy t1 t2 -> AppTy <$> goTy ren t1 <*> goTy ren t2
    AnnType a t -> AnnType a <$> goTy ren t
    ty@ConstTy {} -> pure ty
    ty@LitTy {} -> pure ty

  goTm :: IntMap Int -> Term x -> Scramble (Term x)
  goTm ren = \case
    Var occ ty n -> Var occ <$> goTy ren ty <*> pure (tmName ren n)
    tm@GlobalVar {} -> pure tm
    tm@Data {} -> pure tm
    tm@Literal {} -> pure tm
    tm@Prim {} -> pure tm
    Lam b e -> do
      ty <- goTy ren (idBinderType b)
      let o = binderRaw b
      k <- pick (outer ren (filter (/= o) (frees e))) o
      Lam (idBinder k b ty) <$> goTm (IntMap.insert o k ren) e
    TyLam b e -> do
      kind <- goTy ren (tyBinderKind b)
      let o = binderRaw b
      k <- pick (outer ren (filter (/= o) (frees e))) o
      TyLam (tyBinder k b kind) <$> goTm (IntMap.insert o k ren) e
    App e1 e2 -> App <$> goTm ren e1 <*> goTm ren e2
    TyApp e ty -> TyApp <$> goTm ren e <*> goTy ren ty
    Let (NonRec b x) e -> do
      ty <- goTy ren (idBinderType b)
      x' <- goTm ren x
      let o = binderRaw b
      k <- pick (outer ren (filter (/= o) (frees e))) o
      Let (NonRec (idBinder k b ty) x') <$> goTm (IntMap.insert o k ren) e
    tm@(Let (Rec bs xs) e) -> do
      let forbidden = outer ren (frees tm)
      goIdTele forbidden ren bs $ \ren' bs' ->
        Let <$> (Rec bs' <$> mapM (goTm ren') xs) <*> goTm ren' e
    Case e ty alts -> Case <$> goTm ren e <*> goTy ren ty <*> mapM (goAlt ren ty) alts
    Cast e t1 t2 -> Cast <$> goTm ren e <*> goTy ren t1 <*> goTy ren t2
    Tick t e -> Tick <$> goTick ren t <*> goTm ren e

  goAlt :: IntMap Int -> Type x -> Alt x -> Scramble (Alt x)
  goAlt ren ty alt@(Alt p e) = case p of
    DataPat dc tvs ids -> do
      -- The free variables of the alternative, and the case type: more than
      -- needed
      let forbidden = outer ren (frees (Case (Literal (C.IntLiteral 0)) ty [alt]))
      goTyTele forbidden ren tvs $ \ren1 tvs' ->
        -- The fields must not capture the existentials in their types
        goIdTele (forbidden <> teleNew tvs') ren1 ids $ \ren2 ids' ->
          Alt (DataPat dc tvs' ids') <$> goTm ren2 e
    LitPat _ -> Alt p <$> goTm ren e
    DefaultPat -> Alt p <$> goTm ren e

  -- The new names of the binders of a telescope
  teleNew :: HasNameBinder b => Tele b x y -> IntSet
  teleNew TNil = IntSet.empty
  teleNew (TCons b bs) = IntSet.insert (binderRaw b) (teleNew bs)

  goTyTele
    :: IntSet -> IntMap Int -> Tele TyBinder x y
    -> (IntMap Int -> Tele TyBinder x y -> Scramble r) -> Scramble r
  goTyTele _ ren TNil k = k ren TNil
  goTyTele forbidden ren (TCons b bs) k = do
    kind <- goTy ren (tyBinderKind b)
    let o = binderRaw b
    new <- pick forbidden o
    goTyTele (IntSet.insert new forbidden) (IntMap.insert o new ren) bs $ \ren' bs' ->
      k ren' (TCons (tyBinder new b kind) bs')

  goIdTele
    :: IntSet -> IntMap Int -> Tele IdBinder x y
    -> (IntMap Int -> Tele IdBinder x y -> Scramble r) -> Scramble r
  goIdTele _ ren TNil k = k ren TNil
  goIdTele forbidden ren (TCons b bs) k = do
    ty <- goTy ren (idBinderType b)
    let o = binderRaw b
    new <- pick forbidden o
    goIdTele (IntSet.insert new forbidden) (IntMap.insert o new ren) bs $ \ren' bs' ->
      k ren' (TCons (idBinder new b ty) bs')

  goTick :: IntMap Int -> TickInfo x -> Scramble (TickInfo x)
  goTick ren = \case
    NameMod m ty -> NameMod m <$> goTy ren ty
    Attributes ty e -> Attributes <$> goTy ren ty <*> goTm ren e
    t -> pure t

-- | A property of the values that 'scrambleBinders' makes of a term in a
-- scope
forAllScrambled :: Testable prop => Scope n -> Term n -> (Term n -> prop) -> Property
forAllScrambled sc tm =
  forAllShow (scrambleBinders (scopeWatermark sc) tm) (\t -> "scrambled:\n" <> showTermT t)

-- | 'scrambleBinders' with a supply of fresh names
type Scramble = StateT Int Gen

-- * Properties

-- | Run the QuickCheck properties of a test tree with the given number of
-- tests, unless the number of tests is given on the command line: the
-- unittests run 10000 tests of every property by default (see
-- @unittests.hs@), which is too slow for properties on generated terms. So
-- @--quickcheck-tests 10000@ runs the given number of tests too.
withTests :: Int -> TestTree -> TestTree
withTests n = adjustOption $ \(QuickCheckTests k) ->
  QuickCheckTests (if k == 10000 then n else k)

-- | Run a scoped transformation on a closed term, in the root context, in the
-- default (silent) environment. Gives the result, the number of
-- transformations applied, and whether a change was signaled.
runRewriteClosed :: Rewrite () -> Term 'VoidS -> IO (Term 'VoidS, Word, Bool)
runRewriteClosed t e = do
  (e', st, changed) <- runRewriteTest def def (runTransform t (rootContext emptyScope) e)
  pure (e', _transformCounter st, changed)

-- | A step down a term, to one of its subterms, see 'atPath'
data Step
  = LamStep
  -- ^ The body of a lambda
  | TyLamStep
  -- ^ The body of a type lambda
  | LetBodyStep
  -- ^ The body of a let-expression
  | LetRhsStep Int
  -- ^ The right-hand side with the given index of a let-expression
  | AltStep Int
  -- ^ The right-hand side of the alternative with the given index
  | AppFunStep
  -- ^ The function of an application
  | AppArgStep
  -- ^ The argument of an application
  | TickStep
  -- ^ The body of a tick
  | CastStep
  -- ^ The body of a cast
  | SubjStep
  -- ^ The subject of a case-expression
  deriving (Show)

-- | Go down a term along a path, opening the binders on the way, and continue
-- with the context of the subterm, and the subterm. Errors if the path doesn't
-- fit the term.
atPath
  :: HasCallStack
  => Distinct n
  => [Step]
  -> Scoped.TransformContext n
  -> Term n
  -> (forall l. Distinct l => Scoped.TransformContext l -> Term l -> r)
  -> r
atPath [] ctx e k = k ctx e
atPath (st : sts) ctx e k = case (st, e) of
  (LamStep, Lam b body) -> underLam ctx b body $ \ctx' _ body' -> atPath sts ctx' body' k
  (TyLamStep, TyLam b body) -> underTyLam ctx b body $ \ctx' _ body' -> atPath sts ctx' body' k
  (LetBodyStep, Let bind body) -> underLet ctx bind body $ \ctx' _ body' -> atPath sts ctx' body' k
  (LetRhsStep 0, Let (NonRec b x) _) -> atPath sts (pushFrame (LetBinding b) ctx) x k
  (LetRhsStep i, Let (Rec bs xs) body) | i < length xs ->
    underLetRec ctx bs xs body $ \_ ctxRhs _ xs' _ -> atPath sts (ctxRhs i) (xs' !! i) k
  (AltStep i, Case _ _ alts) | i < length alts ->
    underAlt ctx (alts !! i) $ \ctx' _ e' -> atPath sts ctx' e' k
  (AppFunStep, App f _) -> atPath sts (pushFrame Scoped.AppFun ctx) f k
  (AppArgStep, App f a) -> atPath sts (pushFrame (Scoped.appArgFrame f) ctx) a k
  _ -> error ("atPath: " <> show st <> " doesn't fit the term:\n" <> showTermT e)

-- * Tests of the test scopes

-- | Tests of the test scopes, of 'parseTestTerm', and of the helpers for
-- shadowing
testScopeTests :: TestTree
testScopeTests = testGroup "Test.Clash.Core"
  [ testGroup "parseTestTerm"
      [ -- The binders of a closed term get names in depth-first order, as
        -- GHC2Core and the decoder of "Clash.Core.Binary" give them
        testCase "numbers binders depth-first" $
          sequence_
            [ binderRaws (closedTerm (parseTestTerm s)) @?= raws
            | (s, raws) <-
                [ ("\\(x :: Int) (y :: Int) -> x", [1, 2])
                , ("\\(a :: Type) (x :: a) -> x", [1, 2])
                , ("\\(f :: forall a b. Pair) -> f @Int", [1, 2, 3])
                , ("let { x_0, x_1 :: Int; x_0 = 5; x_1 = x_0 } in x_1", [1, 2])
                , ("(\\(x :: Int) -> x) ((\\(y :: Int) -> y) 1)", [1, 2])
                ]
            ]

      , testCase "free variables are declared in the order of their keys" $
          withTerm "f_5 (x_3 :: Int)" $ \ts tm -> do
            testScopeRaws ts @?= IntSet.fromList [1, 2]
            case (termIn ts "x_3", tm) of
              (Var _ ty x, App (Var _ _ f) (Var _ _ x')) -> do
                tmNameRaw x @?= 1
                tmNameRaw f @?= 2
                tmNameRaw x' @?= 1
                assertTypeIs ts "Int" ty
              _ -> assertFailure ("unexpected shape:\n" <> showPprU tm)

      , testCase "free type variables come first" $
          withTerm "\\(y_1 :: a_7) -> y_1" $ \ts tm -> case tm of
            Lam b (Var {}) -> do
              assertTypeIs ts "a_7" (idBinderType b)
              tmNameRaw (tmNameOf b) @?= 2
            _ -> assertFailure ("unexpected shape:\n" <> showPprU tm)

      , testCase "a free variable gets the type of its first annotated occurrence" $
          withTerms ["f_1 (x_2 :: Int)", "(f_1 :: Int -> Int) x_2"] $ \ts _ ->
            case termIn ts "f_1" of
              Var _ ty _ -> assertTypeIs ts "Int -> Int" ty
              tm -> assertFailure ("unexpected shape:\n" <> showPprU tm)

      , testCase "a bang binding is non-recursive" $
          withTerm "\\(x_1 :: Int) -> let { !(x_1 :: Int) = f_2 x_1 } in x_1" $ \ts tm ->
            case tm of
              Lam bx (Let (NonRec bx' (App _ (Var _ _ x))) (Var _ _ x')) -> do
                tmNameRaw x @?= tmNameRaw (tmNameOf bx)
                tmNameRaw x' @?= tmNameRaw (tmNameOf bx')
                assertBool "different binders" (tmNameRaw x /= tmNameRaw x')
                assertLintClean ts tm
              _ -> assertFailure ("unexpected shape:\n" <> showPprU tm)

      , testCase "bang bindings bind one after the other" $
          case closedTerm "let { !(x :: Int) = 1; !(y :: Int) = x } in y" of
            Let (NonRec _ _) (Let (NonRec _ (Var {})) (Var {})) -> pure ()
            tm -> assertFailure ("unexpected shape:\n" <> showPprU tm)

      , testCase "a let with some bang bindings is an error" $
          assertErrorContains "some, but not all"
            (closedTerm "let { !(x :: Int) = 1; (y :: Int) = x } in y")

      , testCase "case-expressions" $
          withTerm "(case (m_1 :: Maybe Int) of { Just (x_2 :: Int) -> x_2; Nothing -> 0 }) :: Int" $
            \ts tm -> case tm of
              Case (Var {}) ty [Alt (DataPat dcJ TNil (TCons _ TNil)) (Var {}), Alt (DataPat dcN TNil TNil) (Literal _)] -> do
                assertTypeIs ts "Int" ty
                dcJ @?= justDataCon
                dcN @?= nothingDataCon
                assertLintClean ts tm
              _ -> assertFailure ("unexpected shape:\n" <> showPprU tm)

      , testCase "literal and default alternatives" $
          case closedTerm "\\(x :: Int) -> (case x of { 1 -> 2; -1 -> 3; _ -> x }) :: Int" of
            Lam _ (Case _ _ [Alt (LitPat l1) _, Alt (LitPat l2) _, Alt DefaultPat _]) -> do
              l1 @?= C.IntLiteral 1
              l2 @?= C.IntLiteral (-1)
            tm -> assertFailure ("unexpected shape:\n" <> showPprU tm)

      , testCase "global variables" $
          case closedTerm "f_G9 :: Int -> Int" of
            GlobalVar g -> do
              g @?= testGlobalId "f_G9" "Int -> Int"
              assertBool "type" (aeqTy (closedType "Int -> Int") (globalIdType g))
            tm -> assertFailure ("unexpected shape:\n" <> showPprU tm)

      , testCase "type applications of literals, and constructor operators" $
          case closedTerm "\\(v :: Vec 0 Int) (x :: Int) (co :: Int) -> Cons @1 @Int @0 co x v" of
            Lam _ (Lam _ (Lam _ (App (App (App (TyApp (TyApp (TyApp (Data dc) (LitTy (C.NumTy 1))) _) _) _) _) _))) ->
              dc @?= consDc
            tm -> assertFailure ("unexpected shape:\n" <> showPprU tm)

      , testCase "type constructors by name" $ do
          assertBool "Vec"
            (aeqTy (closedType "forall (n :: Nat). Vec n Int -> Clash.Sized.Vector.Vec 2 Int")
              (closedType [typeQQ|
                forall (n :: Nat).
                  Clash.Sized.Vector.Vec n Int -> Clash.Sized.Vector.Vec 2 Int |]))
          case closedType "Vec 2 Maybe_9" of
            AppTy (AppTy (ConstTy (C.TyCon vec)) (LitTy (C.NumTy 2))) (ConstTy (C.TyCon m)) -> do
              vec @?= vecTcNm
              C.nameUniq m @?= 9
            ty -> assertFailure ("unexpected shape:\n" <> showPprU ty)
      ]

  , testGroup "scopes"
      [ testCase "declared variables get increasing names" $
          withTestScope [tyVarD "a_1", varD "x_2" "a_1", atRaw 10 (varD "y_3" "Int")] $ \ts -> do
            testScopeRaws ts @?= IntSet.fromList [1, 2, 10]
            case termIn ts "x_2" of
              Var _ ty _ -> assertTypeIs ts "a_1" ty
              tm -> assertFailure ("unexpected shape:\n" <> showPprU tm)

      , testCase "a raw name below the watermark is an error" $
          assertErrorContains "is not above the names in scope"
            (withTestScope [varD "x_1" "Int", atRaw 1 (varD "y_2" "Int")] $ \ts -> closeFreeVars (termIn ts "y_2"))

      , testCase "a variable that is not in scope is an error" $
          assertErrorContains "variable not in scope"
            (withTestScope [varD "x_1" "Int"] $ \ts -> closeFreeVars (termIn ts "y_2"))
      ]

  , testGroup "running transformations"
      [ testCase "runRewriteAt runs a transformation in the body of a lambda" $
          withTerm "\\(x :: Int) -> \\(y :: Int) -> y" $ \ts e -> do
            let inc = Transform $ \_ _ -> Util.changed (Literal (C.IntLiteral 1))
            (e', _, ch) <- runRewriteAt def (def :: RewriteState ()) ts [LamStep, LamStep] inc e
            assertTermIs ts "\\(x :: Int) (y :: Int) -> 1" e'
            ch @?= True
      , testCase "whnfIn evaluates in a test scope" $
          withTerm "(case 1 of { 1 -> (y :: Int); _ -> 2 }) :: Int" $ \ts e ->
            assertTermIs ts "y"
              (whnfIn (Env testTypeEnv (PrimReductions HashMap.empty Nothing))
                emptyGlobalEnv False (testScope ts) e)
      , testCase "testPrim" $
          withTerm "x_1 :: Int" $ \ts x -> case termIn ts (builtTerm (appB (primB (testPrim "f" "Int -> Int")) (testTermB "x_1"))) of
            App (Prim p) x' -> do
              primName p @?= "f"
              assertTermAlphaEq x x'
            tm -> assertFailure ("unexpected shape:\n" <> showPprU tm)
      , testCase "globalsByOcc compares globals by name" $
          assertTermAlphaEq
            (globalsByOcc (closedTerm "f_G1 :: Int"))
            (globalsByOcc (closedTerm "f_G2 :: Int"))
      ]

  , testGroup "shadowing"
      [ testCase "withClashing gives the first binders the names of new variables" $
          withTerm "\\(y :: Int) -> (x_1 :: Int)" $ \ts0 tm0 ->
            withClashing ts0 [varD "z_2" "Int"] tm0 $ \ts tm -> do
              clashingBinders ts0 tm0 @?= []
              clashingBinders ts tm @?= [2]
              assertClashes ts tm
              assertLintClean ts tm
              case tm of
                Lam b body -> openIdBinder (testScope ts) b body $ \_ b' _ ->
                  assertBool "renamed" (tmNameRaw (tmNameOf b') > 2)
                _ -> assertFailure ("unexpected shape:\n" <> showPprU tm)

      , testCase "in the scope of sunkD, all binders are below the watermark" $
          withTerm "\\(x :: Int) (y :: Int) -> f_1 x y" $ \ts0 tm0 ->
            withClashing ts0 [sunkD] tm0 $ \ts tm ->
              lowBinders (scopeWatermark (testScope ts)) tm @?= binderRaws tm
      ]
  ]
