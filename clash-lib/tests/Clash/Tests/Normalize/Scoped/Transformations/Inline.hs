{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Scoped.Transformations.Inline": the regression
  tests of "Clash.Normalize.Transformations.Inline", run on the ports through
  'Clash.Rewrite.Scoped.ViaFoil.viaFoil', and tests of the ports that have
  no captured sites in the differential corpus.
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Scoped.Transformations.Inline (tests) where

import Data.Default (def)
import GHC.Types.Basic (InlineSpec (Opaque))
import GHC.Types.SourceText (SourceText (NoSourceText))

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User), mkUnsafeName, noSrcSpan)
import qualified Clash.Core.Term as C
import qualified Clash.Core.Type as C
import Clash.Core.Util (primUCo)
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (mkVarEnv)
import Clash.Driver.Types (Binding (..))
import qualified Clash.Normalize.Scoped.Transformations.Inline as S
import Clash.Normalize.Transformations.Inline
  ( bindConstantVarWorker, collapseRHSNoopsWorker, inlineCastWorker
  , inlineSimIOWorker )
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL (onLet, toTransformation)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteState (..))

import Clash.Core.Scoped.Convert (convScope, fromClashTerm)
import Clash.Core.Scoped.FreeVars (freeTmVarOccurrences)
import Clash.Core.Scoped.NameMap (emptyNameMap, nameMapFromList)
import Clash.Core.Scoped.Syntax
import Clash.Rewrite.Scoped.Util (LetBinding (..), keepBindings, openLetBindings)

import Clash.Tests.Normalize.Transformations.Inline (InlineRewrites (..), inlineTests)
import Test.Clash.Rewrite
  ( assertAlphaEq, globalId, inScopeOf, intFunTy, intId, intLit, intTy, localId, mkBinding
  , parseToTermQQ, runSingleTransformation, testTyConMap, withTyConMap )
import Test.Clash.Scoped (assertScopedAlphaEq, withFreeVarsOf)

-- | The ports, through 'Clash.Rewrite.Scoped.ViaFoil.viaFoil'
scopedRewrites :: InlineRewrites
scopedRewrites = InlineRewrites
  { irInlineCleanup = $(asRewriteQ S.inlineCleanup)
  , irInlineSmall = $(asRewriteQ S.inlineSmall)
  , irInlineWorkFree = $(asRewriteQ S.inlineWorkFree)
  , irCollapseRHSNoops = $(asRewriteQ S.collapseRHSNoops)
  }

-- * inlineBndrsCleanup

-- | Run the scoped 'S.inlineBndrsCleanup' on a let-expression, keeping its
-- first binding and inlining the others, like the #1337 tests in
-- "Clash.Tests.Normalize.Transformations" do for Clash Core. Gives the
-- let-expression with the bindings it keeps.
inlineBndrsCleanup1337 :: C.Term -> C.Term -> Assertion
inlineBndrsCleanup1337 expected tm =
  withFreeVarsOf [tm] $ \env -> go (convScope env) (fromClashTerm env tm)
 where
  go :: Distinct n => Scope n -> Term n -> Assertion
  go sc (Let bind body) = openLetBindings sc bind body $ \scL bs lbs body1 ->
    case [(lb, freeTmVarOccurrences (lbRhs lb)) | lb <- lbs] of
      keep0 : inlines ->
        let keep1 = S.inlineBndrsCleanup scL
                      (nameMapFromList [(lbName lb, i) | i@(lb, _) <- inlines])
                      emptyNameMap
                      [keep0]
        in  case keepBindings sc bs keep1 body1 of
              Just e -> assertScopedAlphaEq expected e
              Nothing -> assertFailure "an inlined binder remains"
      [] -> assertFailure "no bindings"
  go _ _ = assertFailure "not a let-expression"

-- | See @t1337a@ in "Clash.Tests.Normalize.Transformations"
t1337a :: Assertion
t1337a = inlineBndrsCleanup1337
  [parseToTermQQ|
    let
      result_1, b_3 :: Int
      result_1 = b_3
      b_3 = b_3 b_3
    in
      result_1
  |]
  [parseToTermQQ|
    let
      -- Types don't mean anything for this example
      result_1, a_2, b_3, c_4 :: Int

      result_1 = a_2

      a_2 = b_3
      b_3 = c_4
      c_4 = a_2 b_3
    in
      result_1
  |]

-- | See @t1337b@ in "Clash.Tests.Normalize.Transformations"
t1337b :: Assertion
t1337b = inlineBndrsCleanup1337
  [parseToTermQQ|
    let
      result_1, c_4 :: Int
      result_1 = c_4
      c_4 = c_4 c_4 c_4
    in
      result_1
  |]
  [parseToTermQQ|
    let
      -- Types don't mean anything for this example
      result_1, a_2, b_3, c_4, d_5 :: Int

      result_1 = a_2

      a_2 = b_3
      b_3 = c_4
      c_4 = d_5
      d_5 = a_2 b_3 c_4
    in
      result_1
  |]

-- | See @t1337c@ in "Clash.Tests.Normalize.Transformations": a free variable
-- of the let-expression is no let-binding to inline
t1337c :: Assertion
t1337c = inlineBndrsCleanup1337
  [parseToTermQQ|
    let
      result_1, b_3 :: Int
      result_1 = b_3
      b_3 = b_3 b_3 (freevar_5 :: Int)
    in
      result_1
  |]
  [parseToTermQQ|
    let
      result_1, a_2, b_3, c_4 :: Int

      result_1 = a_2

      a_2 = b_3
      b_3 = c_4
      c_4 = a_2 b_3 (freevar_5 :: Int)
    in
      result_1
  |]

-- * The function being normalized

-- | The function being normalized, and a global with a binding
top, g :: Id
top = globalId User "top" 10 intTy
g = globalId User "g" 11 intTy

-- | A state in which 'top' is being normalized, with the given bindings
topState :: [(Id, Binding C.Term)] -> RewriteState extra -> RewriteState extra
topState bs st = st
  { _curFun = (top, noSrcSpan)
  , _bindings = mkVarEnv ((g, mkBinding g (intLit 1)) : bs)
  }

-- | Run a rewrite on a term in which @x@ is free, in a state set up with
-- 'topState'
runInTop :: [(Id, Binding C.Term)] -> NormRewrite -> C.Term -> IO C.Term
runInTop bs rw =
  runSingleTransformation (withTyConMap testTyConMap def) (topState bs def) (inScopeOf [x]) rw

x, y :: Id
x = intId User "x" 20
y = intId User "y" 21

-- | Both implementations of a transformation of let-bindings, by the name of
-- the test, with the given implementation of Clash Core
bothImpls :: NormRewrite -> NormRewrite -> [(String, NormRewrite)]
bothImpls cur scoped = [("Clash Core", cur), ("scoped", scoped)]

tests :: TestTree
tests =
  testGroup "Clash.Tests.Normalize.Scoped.Transformations.Inline"
    [ testGroup "regression tests of Clash Core, through viaFoil"
        (inlineTests scopedRewrites)
    , testGroup "inlineBndrsCleanup (#1337)"
        [ testCase "t1337a" t1337a
        , testCase "t1337b" t1337b
        , testCase "t1337c" t1337c
        ]
    , testGroup "bindConstantVar" $ concat
        [ [ testCase ("doesn't inline the function being normalized (#2839), " <> nm) $
              runInTop [] rw (C.Let (C.NonRec y (C.Var top)) (C.Var y))
                >>= assertAlphaEq (C.Let (C.NonRec y (C.Var top)) (C.Var y))
          , testCase ("doesn't inline another global, which isn't work-free-ish, " <> nm) $
              runInTop [] rw (C.Let (C.NonRec y (C.Var g)) (C.Var y))
                >>= assertAlphaEq (C.Let (C.NonRec y (C.Var g)) (C.Var y))
          , testCase ("inlines a constant, " <> nm) $
              runInTop [] rw (C.Let (C.NonRec y (intLit 3)) (C.Var y))
                >>= assertAlphaEq (intLit 3)
          , testCase ("inlines a local variable, " <> nm) $
              runInTop [] rw (C.Let (C.NonRec y (C.Var x)) (C.Var y))
                >>= assertAlphaEq (C.Var x)
          ]
        | (nm, rw) <- bothImpls
            $(asRewriteQ (toTransformation "bindConstantVar" (onLet 'bindConstantVarWorker)))
            $(asRewriteQ S.bindConstantVar)
        ]
    , testGroup "inlineCast" $ concat
        [ [ testCase ("inlines a cast of a variable, " <> nm) $ do
              let e = C.Cast (C.Var x) intTy intTy
              runInTop [] rw (C.Let (C.NonRec y e) (C.App (C.Var fun) (C.Var y)))
                >>= assertAlphaEq (C.App (C.Var fun) e)
          , testCase ("inlines a cast of a global, " <> nm) $ do
              let e = C.Cast (C.Var g) intTy intTy
              runInTop [] rw (C.Let (C.NonRec y e) (C.App (C.Var fun) (C.Var y)))
                >>= assertAlphaEq (C.App (C.Var fun) e)
          , testCase ("doesn't inline a cast of an application, " <> nm) $ do
              let e = C.Let (C.NonRec y (C.Cast (C.App (C.Var fun) (C.Var x)) intTy intTy))
                        (C.Var y)
              runInTop [] rw e >>= assertAlphaEq e
          ]
        | (nm, rw) <- bothImpls
            $(asRewriteQ (toTransformation "inlineCast" (onLet 'inlineCastWorker)))
            $(asRewriteQ S.inlineCast)
        ]
    , testGroup "inlineSimIO" $ concat
        [ [ testCase ("doesn't inline a binding of another type, " <> nm) $ do
              let e = C.Let (C.NonRec y (C.App (C.Var fun) (C.Var x))) (C.Var y)
              runInTop [] rw e >>= assertAlphaEq e
          , testCase ("inlines a SimIO action at every occurrence, " <> nm) $ do
              let act = C.App (C.Var simIOFun) (C.Var x)
              runInTop [] rw
                (C.Let (C.NonRec io act) (C.mkApps (C.Var bindIO) [Left (C.Var io), Left (C.Var io)]))
                >>= assertAlphaEq (C.mkApps (C.Var bindIO) [Left act, Left act])
          ]
        | (nm, rw) <- bothImpls
            $(asRewriteQ (toTransformation "bindSimIO" (onLet 'inlineSimIOWorker)))
            $(asRewriteQ S.inlineSimIO)
        ]
    , testGroup "collapseRHSNoops" $ concat
        [ [ testCase ("doesn't fire in a function without a binding, " <> nm) $ do
              -- E.g. the fake global of 'normalizeWorkFreeApp'
              let e = C.Let (C.NonRec y (C.mkApps idPrim [Left (C.Var x)])) (C.Var y)
              runInTop [] rw e >>= assertAlphaEq e
          , testCase ("collapses an identity primitive in an OPAQUE function, " <> nm) $ do
              let e = C.Let (C.NonRec y (C.mkApps idPrim [Left (C.Var x)])) (C.Var y)
                  topB = (mkBinding top (intLit 0)) {bindingSpec = Opaque NoSourceText}
              runInTop [(top, topB)] rw e >>= assertAlphaEq
                (C.Let (C.NonRec y (C.mkApps primUCo [Right intTy, Right intTy, Left (C.Var x)]))
                       (C.Var y))
          ]
        | (nm, rw) <- bothImpls
            $(asRewriteQ (toTransformation "collapseRHSNoops" (onLet 'collapseRHSNoopsWorker)))
            $(asRewriteQ S.collapseRHSNoops)
        ]
    ]
 where
  fun = globalId User "fun" 12 (intFunTy 1)
  -- @SimIO Int@, and functions producing and combining actions
  simIOTy = C.AppTy (C.ConstTy (C.TyCon (mkUnsafeName User "Clash.Explicit.SimIO.SimIO" 300))) intTy
  simIOFun = globalId User "act" 13 (C.mkFunTy intTy simIOTy)
  bindIO = globalId User "both" 14 (C.mkFunTy simIOTy (C.mkFunTy simIOTy simIOTy))
  io = localId User "io" 22 simIOTy
  idPrim = C.Prim (C.PrimInfo "idPrim" (intFunTy 1) (C.WorkIdentity 0 []) C.SingleResult C.NoUnfolding)
