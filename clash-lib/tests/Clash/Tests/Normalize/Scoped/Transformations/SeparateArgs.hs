{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.SeparateArgs": agreement
  of the port of 'Clash.Normalize.Transformations.SeparateArgs.separateArguments'
  with that of Clash Core.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.SeparateArgs (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.HasType (inferCoreTypeOf)
import Clash.Core.Name (NameSort (User))
import Clash.Core.Subst (aeqType)
import Clash.Core.Term
  (CoreContext (..), NameMod (..), Term (..), TickInfo (..), collectArgs, mkApps)
import Clash.Core.Type (Type (ForAllTy, VarTy), mkFunTy, mkPolyFunTy)
import Clash.Core.Var (Id, Var (..))
import Clash.Driver.Types (ClashEnv (..))
import qualified Clash.Normalize.Scoped.Transformations.SeparateArgs as Scoped
import Clash.Normalize.Transformations.SeparateArgs
  (separateArgumentsLambdaWorker, separateArgumentsSpineWorker)
import Clash.Normalize.Types (NormRewrite, NormalizeState)
import Clash.Rewrite.StrategyDSL
  (onAppNode, onLam, onTickNode, onTyAppNode, onVarNode, toTransformation)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ, asScopedRewriteQ)
import Clash.Rewrite.Types (RewriteEnv (..), RewriteState (..), TransformContext (..))
import Clash.Util.Supply (newSupply)

import Test.Clash.Rewrite
  ( assertFires, clkPairTy, clockTy, globalId, inScopeOf, intFunTy, intId, intTy, localId
  , numTy, parseTyConTy, rewriteEnvWithTyCons, runSingleTransformation, showPprU, tyVar
  , vecTy )
import Test.Clash.Differential (Outcome (..), coercionForm)
import Test.Clash.Scoped (assertRewritesAgree, assertRewritesAgreeUpTo)
import Test.Clash.Scoped.ViaFoil (viaFoil)

-- | 'separateArguments' of Clash Core
separateArgumentsCurrent :: NormRewrite
separateArgumentsCurrent = $(asRewriteQ (toTransformation "separateArguments"
  (  onLam 'separateArgumentsLambdaWorker
  <> onVarNode 'separateArgumentsSpineWorker
  <> onAppNode 'separateArgumentsSpineWorker
  <> onTyAppNode 'separateArgumentsSpineWorker
  <> onTickNode 'separateArgumentsSpineWorker)))

-- | The port, on terms of Clash Core: through
-- 'Test.Clash.Scoped.ViaFoil.viaFoil'
separateArgumentsScoped :: NormRewrite
separateArgumentsScoped = (viaFoil "separateArguments" $(asScopedRewriteQ Scoped.separateArguments))

-- | Assert that 'separateArguments' and its port agree on a term, with the
-- given variables in scope, in the given context
agreeIn :: Bool -> [Id] -> [CoreContext] -> Term -> Assertion
agreeIn fires ids frames tm = do
  supply <- newSupply
  let st = def{_uniqSupply = supply} :: RewriteState NormalizeState
      ctx = TransformContext (inScopeOf ids) frames
  assertFires fires rewriteEnvWithTyCons st ctx separateArgumentsCurrent tm
  assertRewritesAgree rewriteEnvWithTyCons st ctx
    separateArgumentsCurrent separateArgumentsScoped tm

-- | Assert that 'separateArguments' and its port agree on a term, with the
-- given variables in scope, and whether they fire
agree :: Bool -> [Id] -> Term -> Assertion
agree fires ids = agreeIn fires ids []

-- | Like 'agree', up to the form of coercions: the port builds vectors with
-- 'Clash.Core.Scoped.Util.primCo', see 'coercionForm'
agreeUpToCoercions :: [Id] -> Term -> Assertion
agreeUpToCoercions ids tm = do
  supply <- newSupply
  outcome <- assertRewritesAgreeUpTo [coercionForm] rewriteEnvWithTyCons
    (def{_uniqSupply = supply} :: RewriteState NormalizeState)
    (TransformContext (inScopeOf ids) [])
    separateArgumentsCurrent separateArgumentsScoped tm
  case outcome of
    KnownDifferent ["coercion-form"] -> pure ()
    _ -> assertFailure "expected the coercion-form known difference"

-- * Variables

p, v, x :: Id
p = localId User "p" 30 clkPairTy
v = localId User "v" 31 (vecTy (numTy 2) clockTy)
x = intId User "x" 32

-- | A local function @h :: ClkPair -> Int@
h :: Id
h = localId User "h" 40 (mkFunTy clkPairTy intTy)

-- | A global function @gp :: Int -> ClkPair -> Int@
gp :: Id
gp = globalId User "gp" 41 (mkPolyFunTy intTy [Right intTy, Right clkPairTy])

-- | A global function @gq :: ClkPair -> Int -> Int@
gq :: Id
gq = globalId User "gq" 44 (mkPolyFunTy intTy [Right clkPairTy, Right intTy])

-- | A global function @gv :: forall a. a -> Vec 2 (Clock System) -> a@
gv :: Id
gv = globalId User "gv" 42 (ForAllTy a (mkPolyFunTy (VarTy a) [Right (VarTy a), Right (vecTy (numTy 2) clockTy)]))
 where
  a = tyVar User "a" 43

srcTick :: TickInfo
srcTick = NameMod PrefixName (parseTyConTy "Pfx")

lambdaTests :: TestTree
lambdaTests = testGroup "Lambdas"
  [ testCase "splits the binder of a product type" $
      agree True [h] (Lam p (App (Var h) (Var p)))
  , testCase "splits the binder of a vector type" $
      agreeUpToCoercions [] (Lam v (Var v))
  , testCase "names the binders after the closest let-binder" $
      agreeIn True [h] [LetBinding p []] (Lam p (App (Var h) (Var p)))
  , testCase "doesn't split another binder" $
      agree False [] (Lam x (Var x))
  ]

spineTests :: TestTree
spineTests = testGroup "Application spines"
  [ testCase "splits the arguments of a global function" $
      agree True [x, p] (App (App (Var gp) (Var x)) (Var p))
  , testCase "splits the arguments after type arguments" $
      agree True [x, v] (App (App (TyApp (Var gv) intTy) (Var x)) (Var v))
  , testCase "keeps the ticks of the function" $
      agree True [x, p] (App (App (Tick srcTick (Var gp)) (Var x)) (Var p))
  , testCase "splits the arguments of a partial application" $
      agree True [p] (App (Var gq) (Var p))
  , testCase "doesn't split the arguments of a local function" $
      agree False [h, p] (App (Var h) (Var p))
  ]

-- | Run the port on an application of a global function, with the given
-- variables in scope, and assert that it gives the global function the
-- expected type, and that the type of the application doesn't change. See
-- "Clash.Tests.Normalize.Transformations.SeparateArgs" for 'separateArguments'
-- of Clash Core.
assertGlobalType :: [Id] -> Type -> Term -> Assertion
assertGlobalType ids expected tm = do
  supply <- newSupply
  let st = def{_uniqSupply = supply} :: RewriteState NormalizeState
  res <- runSingleTransformation rewriteEnvWithTyCons st (inScopeOf ids)
           separateArgumentsScoped tm
  let tcm = envTyConMap (_clashEnv rewriteEnvWithTyCons)
  case collectArgs res of
    (Var g, _) ->
      assertBool
        ("Expected the type\n" <> showPprU expected <> "\nfor the global in:\n"
          <> showPprU res <> "\nbut got:\n" <> showPprU (varType g))
        (aeqType expected (varType g))
    _ -> assertFailure ("Expected an application of a global:\n" <> showPprU res)
  assertBool
    ("The type of\n" <> showPprU tm <> "\nchanged to that of\n" <> showPprU res)
    (aeqType (inferCoreTypeOf tcm tm) (inferCoreTypeOf tcm res))

globalTypeTests :: TestTree
globalTypeTests = testGroup "The types of globals"
  [ -- @gp :: Int -> ClkPair -> Int@ becomes @gp :: Int -> Clock -> Int -> Int@,
    -- not @gp :: Int -> ClkPair -> ClkPair -> Int@
    testCase "gives the global the types of the fields" $
      assertGlobalType [x, p] (mkPolyFunTy intTy [Right intTy, Right clockTy, Right intTy])
        (App (App (Var gp) (Var x)) (Var p))
    -- @gq :: ClkPair -> Int -> Int@ becomes @gq :: Clock -> Int -> Int -> Int@,
    -- also when it isn't applied to all of its arguments
  , testCase "keeps the parameters that aren't applied" $
      assertGlobalType [p] (mkPolyFunTy intTy [Right clockTy, Right intTy, Right intTy])
        (App (Var gq) (Var p))
    -- @g \@(Int -> Int) p f x@ for @g :: forall a. ClkPair -> a -> a@ keeps
    -- the argument @x@, which is applied to the result of @g@
  , testCase "keeps the arguments applied to the result" $ do
      let a = tyVar User "a" 46
          g = globalId User "g" 47
                (ForAllTy a (mkPolyFunTy (VarTy a) [Right clkPairTy, Right (VarTy a)]))
          f = localId User "f" 48 (intFunTy 1)
      assertGlobalType [p, f, x]
        (ForAllTy a (mkPolyFunTy (VarTy a) [Right clockTy, Right intTy, Right (VarTy a)]))
        (mkApps (Var g) [Right (intFunTy 1), Left (Var p), Left (Var f), Left (Var x)])
  ]

tests :: TestTree
tests = testGroup "Clash.Tests.Normalize.Scoped.Transformations.SeparateArgs"
  [ lambdaTests
  , spineTests
  , globalTypeTests
  ]
