{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.Specialize"
-}

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.Specialize (tests) where

import qualified Control.Lens as Lens
import Data.Default (def)
import qualified Data.Map.Strict as Map

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (Internal, User), noSrcSpan)
import Clash.Core.Term (Bind (..), Pat (..), Term (..), collectArgs, mkApps)
import Clash.Core.Type (mkFunTy)
import Clash.Core.Var (Id, Var (varUniq))
import Clash.Core.VarEnv (lookupVarEnv)
import Clash.Driver.Types (Binding (..))
import Clash.Normalize.Transformations.Inline (bindConstantVar)
import Clash.Normalize.Transformations.Specialize (appProp, nonRepSpec, specialize)
import Clash.Normalize.Types (NormRewrite, NormalizeState, specialisationCache)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteState (..), extra)
import Clash.Rewrite.Util (runRewrite)
import Clash.Util.Supply (newSupply)
import qualified Clash.Core.Type as Type
import qualified Clash.Core.VarEnv as VarEnv

import qualified Test.Clash.Rewrite as Rewrite
import Test.Clash.Rewrite
  ( argUniques, assertAlphaEq, assertNoFreeLocals, firstUniques, globalId, inScopeOf
  , intFunTy, intId, intLit, intTy, localId, mkBindingMap, pairDataCon, pairTy
  , parseToTerm, parseToTermQQ, runRewriteTest, runSingleTransformation
  , showPprU )

appPropR :: NormRewrite
appPropR = $(asRewriteQ appProp)

bindConstantVarR :: NormRewrite
bindConstantVarR = $(asRewriteQ bindConstantVar)

nonRepSpecR :: NormRewrite
nonRepSpecR = $(asRewriteQ nonRepSpec)

-- | Run 'appProp' on a term, with the given variables in scope
runAppProp :: [Id] -> Term -> IO Term
runAppProp inScope = runSingleTransformation def def (inScopeOf inScope) appPropR

-- | Run 'bindConstantVar' on @let a = .. in \\x -> e@, and 'appProp' on the
-- body of the resulting lambda
bindConstantVarThenAppProp :: [Id] -> Term -> IO Term
bindConstantVarThenAppProp inScope term = do
  inlined <- runSingleTransformation def st (inScopeOf inScope) bindConstantVarR term
  case inlined of
    Lam x body -> runAppProp (x : inScope) body
    _ -> assertFailure ("Unexpected result of bindConstantVar: " <> showPprU inlined)
 where
  -- 'bindConstantVar' looks at the function being normalized
  st = def { _curFun = (globalId User "top" 200 intTy, noSrcSpan) }

-- * appProp (#492, #1040, #1035, #3479)
--
-- Up to #1040, 'appProp' relied on its input being deshadowed. Several passes
-- broke that invariant: the flattening stage (fixed in #492 by deshadowing
-- there), and 'bindConstantVar' (#1035). #1040 made 'appProp' deshadow the
-- function part of the application itself, see Note [AppProp no shadowing],
-- and removed the deshadowing from the flattening stage and the inlining
-- transformations again. The tests below run 'appProp' on terms in which a
-- binder in the function part has the same unique as a free variable of an
-- argument.

-- | Case 3 of Note [AppProp no shadowing]:
--
-- > (let x = w in \y -> y) x  ==>  let x' = w in x
--
-- Without deshadowing, the let-binding captures the argument: @let x = w in x@.
-- https://github.com/clash-lang/clash-compiler/pull/492
-- https://github.com/clash-lang/clash-compiler/pull/1040
appPropLetCapture :: IO Term
appPropLetCapture = runAppProp [intId User "x" 1, intId User "w" 3] [parseToTermQQ|
  (let { (x_1 :: Int) = (w_3 :: Int) } in \(y_4 :: Int) -> y_4) (x_1 :: Int)
|]

-- | Case 2 of Note [AppProp no shadowing]: a lambda applied to an argument that
-- performs work becomes a let-binding, so its binder must not capture the free
-- variables of the remaining arguments:
--
-- > (\x -> \y -> y) (f x) (f x)  ==>  let x' = f x in let y = f x in y
--
-- https://github.com/clash-lang/clash-compiler/pull/1040
appPropLamCapture :: IO Term
appPropLamCapture = runAppProp [x, f] (mkApps fun [Left fx, Left fx])
 where
  x = intId User "x" 1
  y = intId User "y" 2
  f = localId User "f" 3 (intFunTy 1)
  fx = App (Var f) (Var x)
  fun = Lam x (Lam y (Var y))

-- | The expected result of 'appPropLamCapture'
appPropLamCaptureExpected :: Term
appPropLamCaptureExpected = Let (NonRec x9 fx) (Let (NonRec y fx) (Var y))
 where
  x9 = intId User "x" 9
  y = intId User "y" 2
  f = localId User "f" 3 (intFunTy 1)
  fx = App (Var f) (Var (intId User "x" 1))

-- | Case 1 of Note [AppProp no shadowing]: arguments are pushed into case
-- alternatives, so pattern binders must not capture their free variables:
--
-- > (case s of MkPair a b -> \k -> k) b  ==>  case s of MkPair a b' -> b
--
-- https://github.com/clash-lang/clash-compiler/pull/1040
appPropCaseCapture :: IO Term
appPropCaseCapture = runAppProp [s, b] (App fun (Var b))
 where
  s = localId User "s" 5 pairTy
  a = intId User "a" 1
  b = intId User "b" 2
  k = intId User "k" 6
  fun = Case (Var s) (intFunTy 1) [(DataPat pairDataCon [] [a, b], Lam k (Var k))]

-- | The expected result of 'appPropCaseCapture'
appPropCaseCaptureExpected :: Term
appPropCaseCaptureExpected =
  Case (Var s) intTy
    [(DataPat pairDataCon [] [intId User "a" 1, intId User "b" 9], Var b)]
 where
  s = localId User "s" 5 pairTy
  b = intId User "b" 2

-- | Issue #1035: 'bindConstantVar' inlines a let-bound lambda without
-- deshadowing it, so the result shadows:
--
-- > let a = \k x -> k in \x -> a x p  ==>  \x -> (\k x -> k) x p
--
-- This broke the no-shadowing invariant 'appProp' relied on. #1036 (and #1037,
-- which absorbed it) made substitution deshadow, but were closed in favour of
-- #1040, which made 'appProp' deshadow instead. 'appProp' should reduce the
-- body of the resulting lambda to the outer @x@, not to @p@.
--
-- https://github.com/clash-lang/clash-compiler/issues/1035
bindConstantVarAppProp :: IO Term
bindConstantVarAppProp = bindConstantVarThenAppProp [p] term
 where
  k = intId User "k" 6
  x = intId User "x" 1
  p = intId User "p" 5
  a = localId User "a" 3 (intFunTy 2)
  term =
    Let (NonRec a (Lam k (Lam x (Var k))))
      (Lam x (mkApps (Var a) [Left (Var x), Left (Var p)]))

-- | Issue #990: inlining let-bindings ('inlineBinders', used by
-- 'bindConstantVar') introduced shadowing, after which 'appProp' let-bound an
-- argument with a binder that captured the free variables of the other
-- arguments. The resulting HDL contained a signal assigned to itself. #991
-- deshadowed after inlining let-bindings; #1040 replaced that by deshadowing
-- in 'appProp'.
--
-- > let a = \x y -> y in \x -> a (f x) (f x)
-- >   ==> (bindConstantVar)  \x -> (\x y -> y) (f x) (f x)
-- >   ==> (appProp)          \x -> let x' = f x in let y = f x in y
--
-- https://github.com/clash-lang/clash-compiler/issues/990
-- https://github.com/clash-lang/clash-compiler/pull/991
bindConstantVarAppPropLet :: IO Term
bindConstantVarAppPropLet = bindConstantVarThenAppProp [f] term
 where
  x = intId User "x" 1
  y = intId User "y" 2
  f = localId User "f" 3 (intFunTy 1)
  a = localId User "a" 4 (intFunTy 2)
  fx = App (Var f) (Var x)
  term =
    Let (NonRec a (Lam x (Lam y (Var y))))
      (Lam x (mkApps (Var a) [Left fx, Left fx]))

-- | @(\\f -> f) (\\k x -> k) x y@ reduces to the free variable @x@. The first
-- argument binds @x_2@, which is also free in the second argument. 'appProp'
-- substitutes work-free arguments using 'unsafeSubstTm', which does not avoid
-- capture: after substituting the first argument for @f_1@, that argument is
-- the head of the application, and substituting @x_2@ for @k_4@ puts it under
-- the binder @x_2@.
--
-- https://github.com/clash-lang/clash-compiler/pull/3479
appPropCapture :: IO Term
appPropCapture = runAppProp [intId User "x" 2, intId User "y" 5] [parseToTermQQ|
  (\(f_1 :: Int) -> f_1) (\(k_4 :: Int) (x_2 :: Int) -> k_4) (x_2 :: Int) (y_5 :: Int)
|]

-- | 'appProp' substitutes work-free arguments into the body of the lambda it
-- applies them to. Such an argument can then end up as the function part of
-- the application, but unlike the original function part, it was never
-- deshadowed. Its binders can then capture the free variables of the
-- remaining arguments, which 'appProp' moves under them.
--
-- The tests below apply @\\f -> f@ to a work-free function of the given number
-- of arguments that binds @x@, followed by arguments in which @x@ is free.
--
-- https://github.com/clash-lang/clash-compiler/pull/3479
appPropSubstitutedArg :: [Id] -> Int -> Term -> [Term] -> IO Term
appPropSubstitutedArg inScope arity arg args =
  runAppProp inScope (mkApps (Lam f (Var f)) (map Left (arg : args)))
 where
  f = localId User "f" 20 (intFunTy arity)

-- | Lambda path: the binder @x@ of the argument becomes a let-binder for the
-- argument @g x@, which performs work:
--
-- > (\f -> f) (\x k -> k) (g x) x  ==>  let x' = g x in x
appPropSubstitutedLam :: IO Term
appPropSubstitutedLam =
  appPropSubstitutedArg [g, x] 2 (Lam x (Lam k (Var k))) [App (Var g) (Var x), Var x]
 where
  g = localId User "g" 4 (intFunTy 1)
  k = intId User "k" 6
  x = intId User "x" 7

-- | Let path:
--
-- > (\f -> f) (let x = w in \k -> k) x  ==>  let x' = w in x
appPropSubstitutedLet :: IO Term
appPropSubstitutedLet =
  appPropSubstitutedArg [w, x] 1 (Let (NonRec x (Var w)) (Lam k (Var k))) [Var x]
 where
  k = intId User "k" 6
  x = intId User "x" 7
  w = intId User "w" 8

-- | Case path:
--
-- > (\f -> f) (case s of MkPair a b -> \k -> k) b  ==>  case s of MkPair a b' -> b
appPropSubstitutedCase :: IO Term
appPropSubstitutedCase =
  appPropSubstitutedArg [s, b] 1 fun [Var b]
 where
  s = localId User "s" 5 pairTy
  a = intId User "a" 1
  b = intId User "b" 2
  k = intId User "k" 6
  fun = Case (Var s) (intFunTy 1) [(DataPat pairDataCon [] [a, b], Lam k (Var k))]

-- | 'bindConstantVar' inlines let-bound lambdas without deshadowing them, so
-- it creates such arguments:
--
-- > let a = \x k -> k in \x -> (\f -> f) a (g x) x
-- >   ==> (bindConstantVar)  \x -> (\f -> f) (\x k -> k) (g x) x
-- >   ==> (appProp)          \x -> let x' = g x in x
bindConstantVarAppPropSubstituted :: IO Term
bindConstantVarAppPropSubstituted = bindConstantVarThenAppProp [g] term
 where
  g = localId User "g" 4 (intFunTy 1)
  k = intId User "k" 6
  x = intId User "x" 7
  a = localId User "a" 11 (intFunTy 2)
  f = localId User "f" 20 (intFunTy 2)
  term =
    Let (NonRec a (Lam x (Lam k (Var k))))
      (Lam x (mkApps (Lam f (Var f))
        [Left (Var a), Left (App (Var g) (Var x)), Left (Var x)]))

-- | The expected result of 'appPropSubstitutedLam' and
-- 'bindConstantVarAppPropSubstituted'
appPropSubstitutedLamExpected :: Term
appPropSubstitutedLamExpected = Let (NonRec x9 (App (Var g) (Var x))) (Var x)
 where
  g = localId User "g" 4 (intFunTy 1)
  x = intId User "x" 7
  x9 = intId User "x" 9

-- * specialize

-- | Global function @f :: Int -> Int@ with body @\\a -> a@
fId :: Id
fId = globalId User "f" 10 (intFunTy 1)

fBinding :: (Id, Term)
fBinding = (fId, Lam a (Var a))
 where
  a = intId User "a" 20

-- | Global function @g :: Int -> Int -> Int@. It has no binding: we only
-- specialize on applications of it.
gId :: Id
gId = globalId User "g" 11 (intFunTy 2)

-- | Specialize @f@ on @g x y@. Returns the result, the body of the specialized
-- function, and the final state.
specializeF
  :: RewriteState NormalizeState
  -> Id
  -> Id
  -> IO (Term, Maybe Term, RewriteState NormalizeState)
specializeF st x y = do
  let tm = App (Var fId) (mkApps (Var gId) [Left (Var x), Left (Var y)])
  (res, st1, _) <-
    runRewriteTest def st (runRewrite "specialize" (inScopeOf [x, y]) specialize tm)
  let newBody = case collectArgs res of
        (Var fSpec, _) -> bindingTerm <$> lookupVarEnv fSpec (_bindings st1)
        _ -> Nothing
  pure (res, newBody, st1)

-- | 'specArgBndrsAndVars' used to determine whether a free variable of the
-- argument specialized on is global by looking up its unique in the global
-- bindings. A local variable with the same unique as a global binder was
-- therefore not abstracted over, and ended up free in the specialized function.
-- Clash creates such variables when it turns a group of mutually recursive
-- global binders into a let-expression.
--
-- https://github.com/clash-lang/clash-compiler/pull/712
specializeLocalShadowingGlobal :: Assertion
specializeLocalShadowingGlobal = do
  supply <- newSupply
  let xGlobal = globalId User "x" 12 intTy
      xLocal = intId User "x" 12
      yLocal = intId User "y" 13
      bs = mkBindingMap [fBinding, (xGlobal, intLit 1)]
  (res, newBody, _) <- specializeF def{_bindings = bs, _uniqSupply = supply} xLocal yLocal
  case (collectArgs res, newBody) of
    ((Var fSpec, args), Just body) -> do
      assertBool ("Expected a new specialized function, but got:\n" <> showPprU res)
        (varUniq fSpec `notElem` [varUniq fId, varUniq xGlobal])
      assertEqual ("Arguments of the specialized function in:\n" <> showPprU res)
        (map (Just . varUniq) [xLocal, yLocal])
        (argUniques args)
      assertNoFreeLocals body
    _ -> assertFailure ("Expected an application of a new global, but got:\n" <> showPprU res)

-- | 'specialize' creates binders for the arguments that precede the one it
-- specializes on. It used to give them the types of those arguments, which
-- can mention type variables bound at the call site. Specializing
-- @f \@a (x :: a) (g u v)@, with @f :: forall b. b -> Int -> Int@, created:
--
-- > f_g = /\b' -> \(x' :: a) (u' :: Int) (v' :: Int) -> f b' x' (g u' v')
--
-- in which @a@ is free. 'argCastSpec' passes applications with type arguments
-- to 'specialize', for example under a case alternative binding an
-- existential type variable.
specializeArgBinderTypes :: Assertion
specializeArgBinderTypes = do
  supply <- newSupply
  let a = Rewrite.tyVar User "a" 50
      b = Rewrite.tyVar User "b" 51
      fPoly = globalId User "fPoly" 14 (Type.ForAllTy b (Type.mkFunTy (Type.VarTy b) (intFunTy 1)))
      fPolyBody = TyLam b (Lam (localId User "y" 21 (Type.VarTy b)) (Lam z (Var z)))
      z = intId User "z" 22
      x = localId User "x" 12 (Type.VarTy a)
      u = intId User "u" 15
      v = intId User "v" 16
      tm = App
             (mkApps (Var fPoly) [Right (Type.VarTy a), Left (Var x)])
             (mkApps (Var gId) [Left (Var u), Left (Var v)])
      st0 = def{_bindings = mkBindingMap [(fPoly, fPolyBody)], _uniqSupply = supply}
      is = VarEnv.extendInScopeSet (inScopeOf [x, u, v]) a
  (res, st1, _) <-
    runRewriteTest def st0 (runRewrite "specialize" is specialize tm)
  case collectArgs res of
    (Var fSpec, _) | Just body <- bindingTerm <$> lookupVarEnv fSpec (_bindings st1) ->
      Rewrite.assertNoFreeTyVars body
    _ -> assertFailure ("Expected an application of a new global, but got:\n" <> showPprU res)

-- | 'specArgBndrsAndVars' used to collect the free variables of the argument
-- specialized on in a set ordered by unique. Specializing @f (g x_123 y_456)@
-- and @f (g x_456 y_123)@ then yielded the binders @[x, y]@ and @[y, x]@, so the
-- second specialization missed the specialization cache.
--
-- https://github.com/clash-lang/clash-compiler/pull/1087
specializeCacheFreeVarOrder :: Assertion
specializeCacheFreeVarOrder = do
  supply <- newSupply
  let st0 = def{_bindings = mkBindingMap [fBinding], _uniqSupply = supply}
      x123 = intId User "x" 123
      y456 = intId User "y" 456
      x456 = intId User "x" 456
      y123 = intId User "y" 123
  (res1, _, st1) <- specializeF st0 x123 y456
  (res2, _, st2) <- specializeF st1 x456 y123
  let cacheSize = Map.size (Lens.view (extra . specialisationCache) st2)
  assertEqual "Number of specializations" 1 cacheSize
  case (collectArgs res1, collectArgs res2) of
    ((Var f1, _), (Var f2, args2)) -> do
      assertEqual "Specialized functions" (showPprU f1) (showPprU f2)
      assertEqual "Arguments of the second specialization"
        (map (Just . varUniq) [x456, y123])
        (argUniques args2)
    _ -> assertFailure
      ("Expected applications of globals, but got:\n" <> showPprU res1
        <> "\nand:\n" <> showPprU res2)

-- | 'specialize' abstracts the specialized function over the free type
-- variables of the argument it specializes on. 'specArgBndrsAndVars' used to
-- order them by occurrence, without regard for their kinds. Specializing
-- @f (case (x :: a) of {})@, with @a :: k@, created:
--
-- > f_spec = /\(a :: k) -> /\k -> \(x :: a) -> f (case x of {})
--
-- in which the @k@ in the kind of @a@ is free.
specializeTyVarKindOrder :: Assertion
specializeTyVarKindOrder = do
  supply <- newSupply
  let k = Rewrite.tyVar User "k" 51
      a = Rewrite.kindedTyVar User "a" 50 (Type.VarTy k)
      x = localId User "x" 12 (Type.VarTy a)
      tm = App (Var fId) (Case (Var x) intTy [])
      st0 = def{_bindings = mkBindingMap [fBinding], _uniqSupply = supply}
      is = VarEnv.extendInScopeSetList (inScopeOf [x]) [k, a]
  (res, st1, _) <-
    runRewriteTest def st0 (runRewrite "specialize" is specialize tm)
  case collectArgs res of
    (Var fSpec, _) | Just body <- bindingTerm <$> lookupVarEnv fSpec (_bindings st1) ->
      Rewrite.assertNoFreeTyVars body
    _ -> assertFailure ("Expected an application of a new global, but got:\n" <> showPprU res)

-- | 'mkFunction', used by 'specialize' to create the specialized function, used
-- to give the new function a fresh unique without checking whether a global
-- binder with that unique already exists, overwriting it.
--
-- https://github.com/clash-lang/clash-compiler/pull/790
specializeDoesNotOverwriteBinders :: Assertion
specializeDoesNotOverwriteBinders = do
  supply <- newSupply
  let -- Global binders occupying the uniques the supply hands out first
      victims =
        [ (globalId User "victim" u intTy, intLit (toInteger n))
        | (n, u) <- zip [0 :: Int ..] (firstUniques 8 supply) ]
      x = intId User "x" 12
      y = intId User "y" 13
      bs = mkBindingMap (fBinding : victims)
  (res, _, st1) <- specializeF def{_bindings = bs, _uniqSupply = supply} x y
  case collectArgs res of
    (Var fSpec, _) ->
      assertBool ("Specialized function reuses a unique of an existing binder:\n" <> showPprU res)
        (varUniq fSpec `notElem` map (varUniq . fst) (fBinding : victims))
    _ -> assertFailure ("Expected an application of a global, but got:\n" <> showPprU res)
  mapM_
    (\(v, t) -> case lookupVarEnv v (_bindings st1) of
        Just b | showPprU (bindingId b) == showPprU v, bindingTerm b == t -> pure ()
        b -> assertFailure
          ("Binder " <> showPprU v <> " was overwritten by: " <> maybe "nothing" (showPprU . bindingId) b))
    victims

-- * Local heads that share a unique with a global binder
--
-- The binding map and the specialisation history are keyed on uniques alone,
-- but a local variable can have the same unique as a global binder. Clash
-- creates such variables when it turns a group of mutually recursive global
-- binders into a let-expression, and new local and global binders are made
-- unique with respect to their own namespace only. 'specialize' used to look up
-- the head of an application, and the head of the argument specialized on, in
-- the binding map regardless of whether it was a global.

-- | A local variable with the same unique as the global 'fId'
fLocal :: Id
fLocal = localId User "f" 10 (intFunTy 1)

-- | Specialize a local function that shares its unique with a global function.
-- There is nothing to specialize, but 'specialize' used to create a new
-- function from the body of the global.
specializeLocalHead :: Assertion
specializeLocalHead = do
  supply <- newSupply
  let x = intId User "x" 12
      y = intId User "y" 13
      tm = App (Var fLocal) (mkApps (Var gId) [Left (Var x), Left (Var y)])
      st0 = def{_bindings = mkBindingMap [fBinding], _uniqSupply = supply}
  (res, st1, _) <-
    runRewriteTest def st0 (runRewrite "specialize" (inScopeOf [fLocal, x, y]) specialize tm)
  assertAlphaEq tm res
  assertEqual "Number of global binders" 1 (length (_bindings st1))

-- | Global function @fHo :: (Int -> Int) -> Int@ with body @\\h -> h 1@
fHoId :: Id
fHoId = globalId User "fHo" 30 (mkFunTy (intFunTy 1) intTy)

fHoBody :: Term
fHoBody = Lam h (App (Var h) (intLit 1))
 where
  h = localId User "h" 40 (intFunTy 1)

-- | Run a transformation on @fHo (k x)@, where @k@ is a local function that
-- shares its unique with the global function @k@, given the binding of that
-- global. Returns the body of the new specialized function.
specializeOnLocalArgHead :: NormRewrite -> NameSort -> Term -> IO (Maybe Term)
specializeOnLocalArgHead rw kSort kBody = do
  supply <- newSupply
  let kGlobal = globalId kSort "k" 31 (intFunTy 2)
      st0 = def
        { _bindings = mkBindingMap [(fHoId, fHoBody), (kGlobal, kBody)]
        , _uniqSupply = supply }
      tm = App (Var fHoId) (App (Var kLocal) (Var xArg))
  (res, st1, _) <-
    runRewriteTest def st0 (runRewrite "specialize" (inScopeOf [kLocal, xArg]) rw tm)
  case collectArgs res of
    (Var fSpec, args) | fSpec /= fHoId -> do
      assertEqual ("Arguments of the specialized function in:\n" <> showPprU res)
        (map (Just . varUniq) [kLocal, xArg])
        (argUniques args)
      pure (bindingTerm <$> lookupVarEnv fSpec (_bindings st1))
    _ -> assertFailure ("Expected an application of a new global, but got:\n" <> showPprU res)

kLocal, xArg :: Id
kLocal = localId User "k" 31 (intFunTy 2)
xArg = intId User "x" 12

-- | The expected body of the function created by 'specializeOnLocalArgHead':
-- it abstracts over the local @k@ rather than inlining the body of the global
-- @k@.
specializeOnLocalArgHeadExpected :: Term
specializeOnLocalArgHeadExpected =
  Lam kLocal (Lam xArg (App fHoBody (App (Var kLocal) (Var xArg))))

-- | The body of the global @k@: @\\p q -> q@
kGlobalBody :: Term
kGlobalBody = Lam (intId User "p" 41) (Lam q (Var q))
 where
  q = intId User "q" 42

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.Specialize"
    [ testCase "appProp deshadows let-bindings in the function part (#492, #1040)" $
        appPropLetCapture >>= assertAlphaEq [parseToTermQQ|
          let { (z_9 :: Int) = (w_3 :: Int) } in (x_1 :: Int)
        |]
    , testCase "appProp let-binds lambda arguments without capture (#1040)" $
        appPropLamCapture >>= assertAlphaEq appPropLamCaptureExpected
    , testCase "appProp pushes arguments into alternatives without capture (#1040)" $
        appPropCaseCapture >>= assertAlphaEq appPropCaseCaptureExpected
    , testCase "appProp handles shadowing introduced by bindConstantVar (#1035)" $
        bindConstantVarAppProp >>= assertAlphaEq (parseToTerm "x_1 :: Int")
    , testCase "appProp does not let-bind a captured argument after inlining (#991)" $
        bindConstantVarAppPropLet >>= assertAlphaEq appPropLamCaptureExpected
    , testCase "appProp does not capture free variables (#3479)" $
        appPropCapture >>= assertAlphaEq (parseToTerm "x_2 :: Int")
    , testCase "appProp deshadows substituted arguments, lambda" $
        appPropSubstitutedLam >>= assertAlphaEq appPropSubstitutedLamExpected
    , testCase "appProp deshadows substituted arguments, let" $
        appPropSubstitutedLet >>= assertAlphaEq
          (Let (NonRec (intId User "x" 9) (Var (intId User "w" 8))) (Var (intId User "x" 7)))
    , testCase "appProp deshadows substituted arguments, case" $
        appPropSubstitutedCase >>= assertAlphaEq appPropCaseCaptureExpected
    , testCase "appProp deshadows substituted arguments after bindConstantVar" $
        bindConstantVarAppPropSubstituted >>= assertAlphaEq appPropSubstitutedLamExpected
    , testCase "specialize abstracts over locals that shadow a global (#712)"
        specializeLocalShadowingGlobal
    , testCase "specialize gives argument binders the types of the parameters"
        specializeArgBinderTypes
    , testCase "specialize orders free variables by occurrence (#1087)"
        specializeCacheFreeVarOrder
    , testCase "specialize orders type variables after the variables in their kinds"
        specializeTyVarKindOrder
    , testCase "specialize does not overwrite existing binders (#790)"
        specializeDoesNotOverwriteBinders
    , testCase "specialize does not specialize a local head that shares a unique with a global"
        specializeLocalHead
    , testCase "specialize does not inline a global into a local argument head that shares its unique" $
        specializeOnLocalArgHead specialize User kGlobalBody
          >>= maybe (assertFailure "No specialized function") (assertAlphaEq specializeOnLocalArgHeadExpected)
    , testCase "nonRepSpec does not inline a global into a local argument head that shares its unique" $
        specializeOnLocalArgHead nonRepSpecR Internal kGlobalBody
          >>= maybe (assertFailure "No specialized function") (assertAlphaEq specializeOnLocalArgHeadExpected)
    ]
