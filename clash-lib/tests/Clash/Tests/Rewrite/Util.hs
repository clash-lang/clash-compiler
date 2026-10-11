{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Rewrite.Util"
-}

module Clash.Tests.Rewrite.Util
  ( tests
  , whnfContextTests
  ) where

import qualified Control.Lens as Lens
import Data.Default (def)
import Data.Either (rights)
import qualified Data.IntMap as IntMap

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.FreeVars (termFreeTyVars)
import Clash.Core.Literal (Literal (IntLiteral))
import Clash.Core.Name (NameSort (User), noSrcSpan)
import Clash.Core.Term
  ( Bind (..), Context, CoreContext (..), Pat (..), Term (..), collectArgs
  , mkApps )
import Clash.Core.Type (Type (VarTy))
import Clash.Core.Var (Id, Var (varUniq))
import Clash.Core.VarEnv (emptyInScopeSet, lookupVarEnv)
import Clash.Driver.Types
  ( Binding (..), ClashEnv (..), ClashOpts (..), DebugOpts (..), debugNone
  , debugSilent )
import Clash.Normalize.Types (NormRewrite, NormalizeState)
import Clash.Rewrite.Types
  (RewriteEnv (..), RewriteState (..), TransformContext (..))
import Clash.Rewrite.Util
  (changed, isJoinPointIn, liftBinding, lintInput, whnfRW)
import Clash.Util.Supply (newSupply)

import Test.Clash.Rewrite
  ( argUniques, assertAlphaEq, assertErrorContainsIO, assertNoFreeLocals
  , firstUniques, globalId, heapLookupEvaluator, inScopeOf, intFunTy, intId
  , intLit, intTy, kindedTyVar, localId, mkBindingMap, pairDataCon, replaceTerm
  , rewriteEnvWithDebug, runRewriteTest, runSingleTransformation, showPprU
  , tyVar )

-- * Invariant checks of 'Clash.Rewrite.Util.apply'

-- | #490 made 'Clash.Rewrite.Util.apply' check that a transformation does not
-- introduce let-binders (or pattern binders) that shadow each other. This runs
-- a transformation that does exactly that, which should be reported.
--
-- https://github.com/clash-lang/clash-compiler/pull/490
-- https://github.com/clash-lang/clash-compiler/commit/f4db6b8bd189de434aa0f160b963ecda4bde7fc1
applyShadowCheck :: Assertion
applyShadowCheck =
  assertErrorContainsIO "accidentally creates shadowing" $
    runSingleTransformation def def emptyInScopeSet bad (intLit 0)
 where
  bad :: NormRewrite
  bad _ _ = changed (Let (Rec [(x, intLit 1), (x, intLit 2)]) (Var x))
  x = intId User "x" 1

-- | #1837: with @-fclash-debug-invariants@, 'Clash.Rewrite.Util.apply' should
-- error when a transformation changes a term without signalling so. It used to
-- compare the original term against itself (it discarded the result of
-- transformations that did not signal a change), so it never fired.
--
-- https://github.com/clash-lang/clash-compiler/pull/1837
applyUnsignalledChange :: Assertion
applyUnsignalledChange =
  assertErrorContainsIO "Expression changed without notice" $
    runSingleTransformation def st emptyInScopeSet sneaky (intLit 0)
 where
  sneaky :: NormRewrite
  sneaky _ _ = pure (intLit 1)
  -- 'applyDebug' skips its checks until the first transformation that signals
  -- a change, so pretend one already did
  st = def { _transformCounter = 1 }

-- | Run a rewrite that replaces a term by the given variable, under the given
-- transformation name, with the given variables in scope. The free variable
-- check of 'Clash.Rewrite.Util.applyDebug' then checks the result.
replaceBy :: String -> [Id] -> Term -> Id -> IO Term
replaceBy name inScope before new =
  replaceTerm def def name (TransformContext (inScopeOf inScope) []) before (Var new)

-- * Scope lint of 'Clash.Rewrite.Util.applyDebug'

-- | Local variables of type @Int@
xInt, yInt :: Id
xInt = intId User "x" 1
yInt = intId User "y" 2

-- | Replace a term in an empty context, see 'replaceTerm'
replaceClosed :: RewriteEnv -> Term -> Term -> IO Term
replaceClosed env = replaceTerm env def "someTransformation" (TransformContext emptyInScopeSet [])

-- | Debug options that enable the scope lint, including the checks for
-- shadowing
shadowEnv :: RewriteEnv
shadowEnv = rewriteEnvWithDebug debugSilent{dbg_invariantsShadowing = True}

-- | The free variable check only looks at local variables, so it misses a
-- reference to a global that does not exist, but the scope lint does not
lintUnknownGlobal :: Assertion
lintUnknownGlobal =
  assertErrorContainsIO "Global variable g[5] is not in the binding map" $
    replaceTerm shadowEnv st "someTransformation"
      (TransformContext emptyInScopeSet []) (intLit 0) (Var g5)
 where
  g5 = globalId User "g" 5 intTy
  -- The binder being normalized need not be in the binding map
  st :: RewriteState NormalizeState
  st = def{_curFun = (globalId User "top" 6 intTy, noSrcSpan)}

-- | The check for accidental shadowing used to only look at the binders of
-- let-expressions that are not in the right-hand side of a let-binding
lintDuplicateInLetRhs :: Assertion
lintDuplicateInLetRhs =
  assertErrorContainsIO "is bound more than once" $
    replaceClosed shadowEnv (intLit 0)
      (Let (Rec [(xInt, Let (Rec [(yInt, intLit 1), (yInt, intLit 2)]) (Var yInt))]) (Var xInt))

-- | Shadowing is only reported with @-fclash-debug-invariants-shadowing@
lintShadowing :: [TestTree]
lintShadowing =
  [ testCase "is not reported by default" $
      replaceClosed def (intLit 0) shadowing >>= assertAlphaEq shadowing
  , testCase "is reported with -fclash-debug-invariants-shadowing" $
      assertErrorContainsIO "Binder x[1] shadows x[1]" $
        replaceClosed shadowEnv (intLit 0) shadowing
  , testCase "of a binder in the context is reported" $
      assertErrorContainsIO "bound in the context of the term" $
        replaceTerm shadowEnv def "someTransformation"
          (TransformContext (inScopeOf [xInt]) [LamBody xInt]) (intLit 0) (Lam xInt (Var xInt))
  , testCase "is not blamed on a transformation that did not introduce it" $
      replaceClosed shadowEnv shadowing shadowing1 >>= assertAlphaEq shadowing1
  , testCase "of a global by a local binder is reported" $
      assertErrorContainsIO "has the unique of a global binder" $
        replaceTerm shadowEnv def{_bindings = mkBindingMap [(fGlobal, intLit 0)]}
          "someTransformation" (TransformContext emptyInScopeSet [])
          (intLit 0) (Lam fLocal (Var fLocal))
  ]
 where
  shadowing = Lam xInt (Lam xInt (Var xInt))
  shadowing1 = Lam xInt (Lam xInt (intLit 1))
  fGlobal = globalId User "f" 6 intTy
  fLocal = intId User "f" 6

-- | 'lintInput' checks the input of normalization
lintInputReported :: Assertion
lintInputReported =
  assertErrorContainsIO "Error in the input of normalization of f" $ do
    (t, _, _) <- runRewriteTest def st (lintInput "normalization" "f" tm >> pure tm)
    pure t
 where
  tm = Lam xInt (Var yInt)
  st :: RewriteState NormalizeState
  st = def{_curFun = (globalId User "f" 6 (intFunTy 1), noSrcSpan)}

-- | Run a transformation that signals a change, with the given number of
-- transformations applied before it, under
-- @-fclash-debug-transformations-from=10@ and
-- @-fclash-debug-transformations-limit=5@.
applyWithLimitFrom :: Word -> IO Term
applyWithLimitFrom nApplied =
  runSingleTransformation env st emptyInScopeSet rw (intLit 0)
 where
  rw :: NormRewrite
  rw _ctx _e = changed (intLit 1)
  st = def { _transformCounter = nApplied }
  env = def { _clashEnv = clashEnv { envOpts = opts { opt_debug = debug } } }
  clashEnv = _clashEnv def
  opts = envOpts clashEnv
  debug = debugNone
    { dbg_transformationsFrom = Just 10
    , dbg_transformationsLimit = Just 5
    }

-- * Join points

-- | The binder of the let-expressions in 'joinPointInputs'
joinPointJ :: Id
joinPointJ = localId User "j" 20 (intFunTy 1)

-- | Let-expressions binding 'joinPointJ', and whether it is a join point in
-- them.
-- 'isJoinPointIn' counted the occurrences of a variable bound by a binder
-- that shadows @j@ as tail calls of @j@. 'Clash.Normalize.Transformations.Inline.inlineOrLiftNonRep'
-- then lifted @j@ instead of inlining it.
joinPointInputs :: [(String, Term, Bool)]
joinPointInputs =
  [ ( "let j = \\z -> f z in case s of { 1 -> j 1; _ -> j 2 }"
    , Let (NonRec j jRhs) (caseS (app j [intLit 1]) (app j [intLit 2]))
    , True )
  , ( "let j = (\\j -> j) f in j 1"
    , Let (NonRec j (App (Lam j (Var j)) (Var f))) (app j [intLit 1])
    , False )
  , ( "letrec { j = (\\j -> j) f } in j 1"
    , Let (Rec [(j, App (Lam j (Var j)) (Var f))]) (app j [intLit 1])
    , False )
  , ( "let j = \\z -> f z in case s of { 1 -> j 1; _ -> let j = f in j 2 }"
    , Let (NonRec j jRhs)
        (caseS (app j [intLit 1]) (Let (NonRec j (Var f)) (app j [intLit 2])))
    , False )
  , ( "let j = \\z -> f z in case s of { 1 -> j 1; _ -> (\\j -> j 2) f }"
    , Let (NonRec j jRhs)
        (caseS (app j [intLit 1]) (App (Lam j (app j [intLit 2])) (Var f)))
    , False )
  ]
 where
  j = joinPointJ
  f = localId User "f" 21 (intFunTy 1)
  s = intId User "s" 22
  z = intId User "z" 23
  jRhs = Lam z (app f [Var z])
  app g args = mkApps (Var g) (map Left args)
  caseS alt1 alt2 =
    Case (Var s) intTy [(LitPat (IntLiteral 1), alt1), (DefaultPat, alt2)]

-- * liftBinding

-- | 'liftBinding' abstracts the lifted function over the free type variables
-- of the binding, and replaces the recursive occurrences by the new function
-- applied to them. That substitution must know these type variables are in
-- scope, or it doesn't rename a type binder that would capture them. Lifting
--
-- > r = \z -> k (/\a -> r z) x    where x :: a, and a is free
--
-- must not give
--
-- > top_r = /\a -> \x z -> k (/\a -> top_r @a x z) x
liftBindingTyVarCapture :: Assertion
liftBindingTyVarCapture = do
  supply <- newSupply
  let a = tyVar User "a" 50
      r = localId User "r" 40 (intFunTy 1)
      z = intId User "z" 41
      x = localId User "x" 31 (VarTy a)
      k = globalId User "k" 33 (intFunTy 2)
      cf = globalId User "top" 42 (intFunTy 1)
      e = Lam z (mkApps (Var k) [Left (TyLam a (App (Var r) (Var z))), Left (Var x)])
      st :: RewriteState NormalizeState
      st = def{_curFun = (cf, noSrcSpan), _uniqSupply = supply}
  ((_, newExpr), st1, _) <- runRewriteTest def st (liftBinding (r, e))
  case collectArgs newExpr of
    (Var rLifted, _) ->
      case lookupVarEnv rLifted (_bindings st1) of
        Nothing -> assertFailure "Lifted function not found in global bindings"
        Just b -> do
          let b' = tyVar User "b" 51
              rApp = mkApps (Var rLifted) [Right (VarTy a), Left (Var x), Left (Var z)]
          assertAlphaEq
            (TyLam a (Lam x (Lam z (mkApps (Var k)
              [Left (TyLam b' rApp), Left (Var x)]))))
            (bindingTerm b)
    _ -> assertFailure ("Expected an application of a global, but got:\n" <> showPprU newExpr)

-- | 'liftBinding' must abstract over a free type variable before abstracting
-- over the type variables in its kind. Lifting
--
-- > r = \z -> k (r z) x    where x :: a, a :: kk, and kk is free
--
-- must give @/\\kk a -> ..@, not @/\\a kk -> ..@, even though the unique of @a@
-- is smaller than the unique of @kk@.
liftBindingTyVarOrder :: Assertion
liftBindingTyVarOrder = do
  supply <- newSupply
  let kk = tyVar User "kk" 51
      a = kindedTyVar User "a" 50 (VarTy kk)
      r = localId User "r" 40 (intFunTy 1)
      z = intId User "z" 41
      x = localId User "x" 31 (VarTy a)
      k = globalId User "k" 33 (intFunTy 2)
      cf = globalId User "top" 42 (intFunTy 1)
      e = Lam z (mkApps (Var k) [Left (App (Var r) (Var z)), Left (Var x)])
      st :: RewriteState NormalizeState
      st = def{_curFun = (cf, noSrcSpan), _uniqSupply = supply}
  ((_, newExpr), st1, _) <- runRewriteTest def st (liftBinding (r, e))
  case collectArgs newExpr of
    (Var rLifted, args) -> do
      assertEqual ("Type arguments of the lifted function in:\n" <> showPprU newExpr)
        [VarTy kk, VarTy a] (rights args)
      case lookupVarEnv rLifted (_bindings st1) of
        Nothing -> assertFailure "Lifted function not found in global bindings"
        Just b -> case Lens.toListOf termFreeTyVars (bindingTerm b) of
          [] -> pure ()
          tvs -> assertFailure
            ("Unexpected free type variables " <> showPprU tvs <> " in:\n"
              <> showPprU (bindingTerm b))
    _ -> assertFailure ("Expected an application of a global, but got:\n" <> showPprU newExpr)

-- | 'liftBinding' lifts a (recursive) let-binding to a global function,
-- abstracting over the free variables of the binding. It must replace the
-- recursive occurrences by an application of the new function /before/
-- abstracting over the free variables. Doing it afterwards puts the applied
-- free variables under the new lambdas, which a capture-avoiding substitution
-- then renames, leaving them free.
--
-- https://github.com/clash-lang/clash-compiler/commit/e4cdac6ea66d1a12d7f63ef5e2e3722021680a4b
liftBindingSubstitutesBeforeAbstracting :: Assertion
liftBindingSubstitutesBeforeAbstracting = do
  supply <- newSupply
  let r = localId User "r" 40 (intFunTy 1)
      z = intId User "z" 41
      x = intId User "x" 31
      k = globalId User "k" 33 (intFunTy 2)
      cf = globalId User "top" 42 (intFunTy 1)
      -- r = \z -> k (r z) x
      e = Lam z (mkApps (Var k) [Left (App (Var r) (Var z)), Left (Var x)])
      st :: RewriteState NormalizeState
      st = def{_curFun = (cf, noSrcSpan), _uniqSupply = supply}
  ((_, newExpr), st1, _) <- runRewriteTest def st (liftBinding (r, e))
  case collectArgs newExpr of
    (Var rLifted, args) -> do
      assertEqual ("Arguments of the lifted function in:\n" <> showPprU newExpr)
        [Just (varUniq x)] (argUniques args)
      case lookupVarEnv rLifted (_bindings st1) of
        Nothing -> assertFailure "Lifted function not found in global bindings"
        Just b -> assertNoFreeLocals (bindingTerm b)
    _ -> assertFailure ("Expected an application of a global, but got:\n" <> showPprU newExpr)

-- | 'liftBinding' gives the new global function a fresh unique, which only
-- avoids the uniques of the other global functions. It can therefore be the
-- unique of a free local variable of the lifted binding. The substitution of
-- the recursive occurrences used to replace that local variable by the new
-- global function, as it found the global function in its in-scope set.
liftBindingLocalSharesUniqueWithGlobal :: Assertion
liftBindingLocalSharesUniqueWithGlobal = do
  supply <- newSupply
  u : _ <- pure (firstUniques 1 supply)
  let r = localId User "r" 40 (intFunTy 1)
      z = intId User "z" 41
      x = intId User "x" u
      k = globalId User "k" 33 (intFunTy 2)
      cf = globalId User "top" 42 (intFunTy 1)
      -- r = \z -> k (r z) x
      e = Lam z (mkApps (Var k) [Left (App (Var r) (Var z)), Left (Var x)])
      st :: RewriteState NormalizeState
      st = def{_curFun = (cf, noSrcSpan), _uniqSupply = supply}
  ((_, newExpr), st1, _) <- runRewriteTest def st (liftBinding (r, e))
  case collectArgs newExpr of
    (Var rLifted, _) ->
      case lookupVarEnv rLifted (_bindings st1) of
        Nothing -> assertFailure "Lifted function not found in global bindings"
        Just b ->
          -- \x z -> k (rLifted x z) x
          assertAlphaEq
            (Lam x (Lam z (mkApps (Var k)
              [ Left (mkApps (Var rLifted) [Left (Var x), Left (Var z)])
              , Left (Var x) ])))
            (bindingTerm b)
    _ -> assertFailure ("Expected an application of a global, but got:\n" <> showPprU newExpr)

-- * whnfRW

-- | Reduce a term to WHNF with 'whnfRW', in a context given innermost frame
-- first, with the given variables in scope. 'heapLookupEvaluator' only
-- replaces variables by their heap bindings, so the result shows which
-- let-bindings of the context 'whnfRW' put on the heap.
whnfInContext :: [Id] -> Context -> Term -> IO Term
whnfInContext inScope ctx e = do
  (t, _, _) <- runRewriteTest env st $
    whnfRW False (TransformContext (inScopeOf inScope) ctx) e (\_ t -> changed t)
  pure t
 where
  env = def{_evaluator = heapLookupEvaluator}
  st :: RewriteState NormalizeState
  st = def{_globalHeap = (IntMap.empty, 0)}

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Rewrite.Util"
    [ testCase "apply reports transformations that introduce shadowing (#490)"
        applyShadowCheck
    , testCase "apply errors on unsignalled change (#1837)"
        applyUnsignalledChange
    , testGroup "isJoinPointIn accounts for shadowing binders"
        [ testCase name $
            assertEqual "isJoinPointIn" expected (isJoinPointIn joinPointJ tm)
        | (name, tm, expected) <- joinPointInputs
        ]

    -- Since #2571, the evaluator uses the let-bindings in the context of the
    -- term it rewrites. Transformations using 'whnfRW' can therefore introduce
    -- variables that are bound in that context, but are not free in the term
    -- they rewrite. #2624 relaxed the free variable check of 'applyDebug'
    -- accordingly for 'caseCon', and #3207 did so for 'reduceConst' and
    -- 'constantSpec'. The check is not relaxed for other transformations, nor
    -- for variables that are not bound in the context.
    -- https://github.com/clash-lang/clash-compiler/pull/3207
    --
    -- Note that the relaxed check misses variables captured by a binder in the
    -- context: https://github.com/clash-lang/clash-compiler/issues/3208
    , testGroup "applyDebug free variable check (#3207)" $
        [ testCase (name <> " may introduce variables bound in the context") $
            replaceBy name [x, y] (Var x) y >>= assertAlphaEq (Var y)
        | name <- ["reduceConst", "constantSpec"]
        ] <>
        [ testCase "reduceConst may not introduce variables not bound in the context" $
            assertErrorContainsIO "It introduces free variables" $
              replaceBy "reduceConst" [x] (Var x) y
        , testCase "other transformations may not introduce variables bound in the context" $
            assertErrorContainsIO "It introduces free variables" $
              replaceBy "caseCase" [x, y] (Var x) y
        ]
    , testGroup "applyDebug scope lint"
        [ testCase "reports globals that are not in the binding map"
            lintUnknownGlobal
        , testCase "reports duplicate binders in the right-hand side of a let-binding"
            lintDuplicateInLetRhs
        , testGroup "shadowing" lintShadowing
        , testCase "lintInput reports violations in the input of normalization"
            lintInputReported
        ]

    -- @-fclash-debug-transformations-limit@ counts from
    -- @-fclash-debug-transformations-from@. 'Clash.Rewrite.Util.applyDebug'
    -- used to subtract the latter from the number of applied transformations
    -- as a 'Word', which wrapped around before reaching it, exceeding the
    -- limit straight away.
    , testGroup "applyDebug transformation limit counts from the first debugged transformation"
        [ testCase "no limit before the first debugged transformation" $
            applyWithLimitFrom 0 >>= assertAlphaEq (intLit 1)
        , testCase "limit not yet exceeded" $
            applyWithLimitFrom 14 >>= assertAlphaEq (intLit 1)
        , testCase "limit exceeded" $
            assertErrorContainsIO "-fclash-debug-transformations-limit exceeded" $
              applyWithLimitFrom 15
        ]
    , testCase "liftBinding avoids capturing free type variables"
        liftBindingTyVarCapture
    , testCase "liftBinding abstracts over type variables in dependency order"
        liftBindingTyVarOrder
    , testCase "liftBinding substitutes before abstracting"
        liftBindingSubstitutesBeforeAbstracting
    , testCase "liftBinding keeps a local that shares a unique with the lifted function"
        liftBindingLocalSharesUniqueWithGlobal

    -- Since #2571, 'whnfRW' puts the let-bindings of the context on the heap
    -- of the evaluator. Binders between such a let-binding and the term can
    -- shadow its binder, or the free variables of its right-hand side, in
    -- which case it must not be used.
    , testGroup "whnfRW uses the context's let-bindings (#2571)"
        (whnfContextTests whnfInContext)
    ]
 where
  x = intId User "x" 1
  y = intId User "y" 2

-- | The tests of the let-bindings of the context that 'whnfRW' uses, given a
-- function that reduces a term to WHNF in a context, given innermost frame
-- first, with the given variables in scope (see 'whnfInContext')
whnfContextTests :: ([Id] -> Context -> Term -> IO Term) -> [TestTree]
whnfContextTests whnfIn =
  [ testCase "let x = 3 in \\y -> x" $
      whnfIn [x, y] [LamBody y, LetBody [(x, intLit 3)]] (Var x)
        >>= assertAlphaEq (intLit 3)
  , testCase "let x = 3 in \\x -> x" $
      whnfIn [x] [LamBody x, LetBody [(x, intLit 3)]] (Var x)
        >>= assertAlphaEq (Var x)
  , testCase "let x = y in \\y -> x" $
      whnfIn [x, y] [LamBody y, LetBody [(x, Var y)]] (Var x)
        >>= assertAlphaEq (Var x)
  , testCase "let x = y in case s of MkPair y z -> x" $
      whnfIn [x, y, z, s]
        [ CaseAlt (DataPat pairDataCon [] [y, z])
        , LetBody [(x, Var y)] ]
        (Var x)
        >>= assertAlphaEq (Var x)
  , testCase "let x = 3 in letrec { x = 4; w = x } in .." $
      whnfIn [x, w]
        [LetBinding w [x, w], LetBody [(x, intLit 3)]] (Var x)
        >>= assertAlphaEq (Var x)
  , testCase "let x = y in let y = 5 in x" $
      whnfIn [x, y]
        [LetBody [(y, intLit 5)], LetBody [(x, Var y)]] (Var x)
        >>= assertAlphaEq (Var x)
  , testCase "let x = (v :: a) in /\\a -> x" $
      whnfIn [x, v]
        [TyLamBody a, LetBody [(x, Var v)]] (Var x)
        >>= assertAlphaEq (Var x)
  , testCase "let x = 3 in let w = x in \\y -> w" $
      whnfIn [x, w, y]
        [LamBody y, LetBody [(w, Var x)], LetBody [(x, intLit 3)]] (Var w)
        >>= assertAlphaEq (intLit 3)
  ]
 where
  x = intId User "x" 1
  y = intId User "y" 2
  z = intId User "z" 3
  s = intId User "s" 4
  w = intId User "w" 5
  a = tyVar User "a" 6
  v = localId User "v" 7 (VarTy a)
