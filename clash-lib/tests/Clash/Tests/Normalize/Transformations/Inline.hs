{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.Inline". The regression tests
  are parameterised over the implementation of the transformations
  ('InlineRewrites'): "Clash.Tests.Normalize.Scoped.Transformations.Inline"
  runs them on the ports to scoped Clash Core too.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.Inline
  ( tests
  , InlineRewrites (..)
  , inlineTests
  ) where

import Data.Default (def)
import GHC.Types.Basic (InlineSpec (Opaque))
import GHC.Types.SourceText (SourceText (NoSourceText))

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (System, User), mkUnsafeName, noSrcSpan)
import Clash.Core.Term
  ( Bind (..), IsMultiPrim (..), PrimInfo (..), PrimUnfolding (..), Term (..)
  , WorkInfo (..), mkApps )
import qualified Clash.Core.Type as C
import Clash.Core.Type (mkFunTy)
import Clash.Core.Util (primUCo)
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (mkVarEnv)
import Clash.Driver.Types (Binding (..))
import Clash.Normalize.Transformations.Inline
  ( bindConstantVarWorker, collapseRHSNoopsWorker, inlineCastWorker, inlineCleanupWorker
  , inlineSimIOWorker, inlineSmallWorker, inlineWorkFreeWorker )
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL
  (onAppNode, onLet, onTickNode, onTyAppNode, onVarNode, toTransformation)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteState (..))

import Test.Clash.Rewrite
  ( assertAlphaEq, assertNoFreeLocals, assertStructurallyEqual, globalId
  , inScopeOf, intFunTy, intId, intLit, intTy, localId, mkBinding, mkBindingMap
  , pairDataCon, pairTy, runSingleTransformation, testTyConMap, withTyConMap )
import Clash.Core.Scoped.Convert (mkGlobalIdFromClash)

-- | The implementations of the transformations that the tests run
data InlineRewrites = InlineRewrites
  { irInlineCleanup :: NormRewrite
  , irInlineSmall :: NormRewrite
  , irInlineWorkFree :: NormRewrite
  , irCollapseRHSNoops :: NormRewrite
  }

-- | The implementations of Clash Core. The transformations of
-- "Clash.Normalize.Transformations.Inline" are the ports to scoped Clash Core,
-- so these are built from the workers of Clash Core.
currentRewrites :: InlineRewrites
currentRewrites = InlineRewrites
  { irInlineCleanup =
      $(asRewriteQ (toTransformation "inlineCleanup" (onLet 'inlineCleanupWorker)))
  , irInlineSmall =
      $(asRewriteQ (toTransformation "inlineSmall"
          (onVarNode 'inlineSmallWorker <> onAppNode 'inlineSmallWorker
            <> onTyAppNode 'inlineSmallWorker <> onTickNode 'inlineSmallWorker)))
  , irInlineWorkFree =
      $(asRewriteQ (toTransformation "inlineWorkFree"
          (onVarNode 'inlineWorkFreeWorker <> onAppNode 'inlineWorkFreeWorker
            <> onTyAppNode 'inlineWorkFreeWorker <> onTickNode 'inlineWorkFreeWorker)))
  , irCollapseRHSNoops =
      $(asRewriteQ (toTransformation "collapseRHSNoops" (onLet 'collapseRHSNoopsWorker)))
  }

-- * inlineCleanup

-- | Run 'inlineCleanup' on a let-expression in which the binders that it wants
-- to inline (data constructor applications bound to system names) form two
-- nested cycles: @w -> y -> x -> w@ and @x <-> y@.
--
-- > letrec
-- >   w = MkPair y y
-- >   y = MkPair x x
-- >   x = MkPair w y
-- >   k = MkPair w x
-- > in k
--
-- Starting from @k@, 'inlineCleanup' visits @w@, @y@ and @x@ in that order. So
-- @x@ refers to two let-bindings that are still being processed. It used to
-- inline @x@ anyway, and only mark @y@ as recursive. Then it inlined @w@ into
-- @k@ and dropped it, while the copy of @x@ in @k@ still referred to it:
--
-- > letrec
-- >   y = MkPair (MkPair (MkPair y y) y) (MkPair (MkPair y y) y)
-- >   k = MkPair (MkPair y y) (MkPair w y)
-- > in k
inlineCleanupNestedCycles :: NormRewrite -> IO Term
inlineCleanupNestedCycles inlineCleanupR =
  runSingleTransformation def def (inScopeOf []) inlineCleanupR $
    Letrec
      [ (w, pair y y)
      , (y, pair x x)
      , (x, pair w y)
      , (k, pair w x) ]
      (Var k)
 where
  pair a b = mkApps (Data pairDataCon) [Left (Var a), Left (Var b)]
  -- The uniques determine the order in which 'inlineCleanup' visits binders
  w = localId System "w" 11 pairTy
  y = localId System "y" 12 pairTy
  x = localId System "x" 13 pairTy
  -- A user defined name, so 'inlineCleanup' keeps it
  k = localId User "k" 14 pairTy

-- * Local variables shadowing global binders (#405)

-- | A reference to a local variable which has the same unique as a global
-- binder. Before #405, the inlining transformations didn't check whether the
-- variable was local, and inlined the global binder instead.
--
-- https://github.com/clash-lang/clash-compiler/pull/405
-- https://github.com/clash-lang/clash-compiler/commit/16e65951c40b36ce64861819ff14e1a5b6cc0486
inlineLocalVar :: NormRewrite -> IO Term
inlineLocalVar rw = runSingleTransformation def st (inScopeOf [fLocal]) rw (Var fLocal)
 where
  st = def { _bindings = mkBindingMap [(globalId User "f" 7 intTy, intLit 42)] }

-- | See 'inlineLocalVar'
fLocal :: Id
fLocal = intId User "f" 7

-- | A primitive like @map :: (Int -> Int) -> Int -> Int@: the identity on its
-- second argument, if its first argument is a no-op
mapPrim :: Term
mapPrim = Prim PrimInfo
  { primName = "mapPrim"
  , primType = mkFunTy (intFunTy 1) (intFunTy 1)
  , primWorkInfo = WorkIdentity 1 [0]
  , primMultiResult = SingleResult
  , primUnfolding = NoUnfolding
  }

-- | Run 'collapseRHSNoops' on @let r = mapPrim f xs in r@, where @f@ is a local
-- variable with the same unique as the global no-op @f = \\x -> x@. A local
-- variable is not a no-op, regardless of its unique, so the term should stay as
-- it is. 'collapseRHSNoops' used to look up @f@ in the global binders, and
-- turned the let-binding into @r = unsafeCoerce# xs@.
collapseRHSNoopsLocalVar :: NormRewrite -> IO Term
collapseRHSNoopsLocalVar rw = runCollapseRHSNoops rw fLocalFun

-- | Run 'collapseRHSNoops' on @let r = mapPrim f xs in r@, with the given @f@,
-- in a function in which the global @f = \\x -> x@ is defined. See
-- 'collapseRHSNoopsLocalVar'.
runCollapseRHSNoops :: NormRewrite -> Id -> IO Term
runCollapseRHSNoops collapseRHSNoopsR f =
  runSingleTransformation def st (inScopeOf [fLocalFun, xs]) collapseRHSNoopsR
    (collapseRHSNoopsTerm f)
 where
  -- 'collapseRHSNoops' only runs in NOINLINE/OPAQUE functions
  top = globalId User "top" 10 intTy
  topBinding = (mkBinding top (intLit 0)) { bindingSpec = Opaque NoSourceText }
  x = intId User "x" 8
  st = def
    { _curFun = (mkGlobalIdFromClash top, noSrcSpan)
    , _bindings = mkVarEnv
        [ (top, topBinding)
        , (fGlobal, mkBinding fGlobal (Lam x (Var x))) ]
    }

-- | @let r = mapPrim f xs in r@, see 'runCollapseRHSNoops'
collapseRHSNoopsTerm :: Id -> Term
collapseRHSNoopsTerm f =
  Let (NonRec r (mkApps mapPrim [Left (Var f), Left (Var xs)])) (Var r)

-- | See 'collapseRHSNoopsLocalVar'
fGlobal, fLocalFun, xs, r :: Id
fGlobal = globalId User "f" 7 (intFunTy 1)
fLocalFun = localId User "f" 7 (intFunTy 1)
xs = intId User "xs" 11
r = intId User "r" 12

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.Inline"
    (inlineTests currentRewrites <> letBindingTests)

-- * Transformations of let-bindings

-- | Tests of 'bindConstantVarWorker', 'inlineCastWorker', 'inlineSimIOWorker'
-- and 'collapseRHSNoopsWorker'
letBindingTests :: [TestTree]
letBindingTests =
    [ testGroup "bindConstantVar"
        [ testCase "doesn't inline the function being normalized (#2839)" $
            runInTop [] bindConstantVarR (Let (NonRec y (Var top)) (Var y))
              >>= assertAlphaEq (Let (NonRec y (Var top)) (Var y))
        , testCase "doesn't inline another global, which isn't work-free-ish" $
            runInTop [] bindConstantVarR (Let (NonRec y (Var g)) (Var y))
              >>= assertAlphaEq (Let (NonRec y (Var g)) (Var y))
        , testCase "inlines a constant" $
            runInTop [] bindConstantVarR (Let (NonRec y (intLit 3)) (Var y))
              >>= assertAlphaEq (intLit 3)
        , testCase "inlines a local variable" $
            runInTop [] bindConstantVarR (Let (NonRec y (Var x)) (Var y))
              >>= assertAlphaEq (Var x)
        ]
    , testGroup "inlineCast"
        [ testCase "inlines a cast of a variable" $ do
            let e = Cast (Var x) intTy intTy
            runInTop [] inlineCastR (Let (NonRec y e) (App (Var fun) (Var y)))
              >>= assertAlphaEq (App (Var fun) e)
        , testCase "inlines a cast of a global" $ do
            let e = Cast (Var g) intTy intTy
            runInTop [] inlineCastR (Let (NonRec y e) (App (Var fun) (Var y)))
              >>= assertAlphaEq (App (Var fun) e)
        , testCase "doesn't inline a cast of an application" $ do
            let e = Let (NonRec y (Cast (App (Var fun) (Var x)) intTy intTy)) (Var y)
            runInTop [] inlineCastR e >>= assertAlphaEq e
        ]
    , testGroup "inlineSimIO"
        [ testCase "doesn't inline a binding of another type" $ do
            let e = Let (NonRec y (App (Var fun) (Var x))) (Var y)
            runInTop [] inlineSimIOR e >>= assertAlphaEq e
        , testCase "inlines a SimIO action at every occurrence" $ do
            let act = App (Var simIOFun) (Var x)
            runInTop [] inlineSimIOR
              (Let (NonRec io act) (mkApps (Var bindIO) [Left (Var io), Left (Var io)]))
              >>= assertAlphaEq (mkApps (Var bindIO) [Left act, Left act])
        ]
    , testGroup "collapseRHSNoops"
        [ testCase "doesn't fire in a function without a binding" $ do
            -- E.g. the fake global of 'normalizeWorkFreeApp'
            let e = Let (NonRec y (mkApps idPrim [Left (Var x)])) (Var y)
            runInTop [] collapseRHSNoopsR e >>= assertAlphaEq e
        , testCase "collapses an identity primitive in an OPAQUE function" $ do
            let e = Let (NonRec y (mkApps idPrim [Left (Var x)])) (Var y)
                topB = (mkBinding top (intLit 0)) {bindingSpec = Opaque NoSourceText}
            runInTop [(top, topB)] collapseRHSNoopsR e >>= assertAlphaEq
              (Let (NonRec y (mkApps primUCo [Right intTy, Right intTy, Left (Var x)]))
                   (Var y))
        ]
    ]
 where
  -- The function being normalized, and a global with a binding
  top = globalId User "top" 10 intTy
  g = globalId User "g" 11 intTy

  -- A state in which 'top' is being normalized, with the given bindings
  topState :: [(Id, Binding Term)] -> RewriteState extra -> RewriteState extra
  topState bs st = st
    { _curFun = (mkGlobalIdFromClash top, noSrcSpan)
    , _bindings = mkVarEnv ((g, mkBinding g (intLit 1)) : bs)
    }

  -- Run a rewrite on a term in which @x@ is free, in a state set up with
  -- 'topState'
  runInTop :: [(Id, Binding Term)] -> NormRewrite -> Term -> IO Term
  runInTop bs rw =
    runSingleTransformation (withTyConMap testTyConMap def) (topState bs def) (inScopeOf [x]) rw

  x = intId User "x" 20
  y = intId User "y" 21
  bindConstantVarR = $(asRewriteQ (toTransformation "bindConstantVar" (onLet 'bindConstantVarWorker)))
  inlineCastR = $(asRewriteQ (toTransformation "inlineCast" (onLet 'inlineCastWorker)))
  inlineSimIOR = $(asRewriteQ (toTransformation "bindSimIO" (onLet 'inlineSimIOWorker)))
  collapseRHSNoopsR = $(asRewriteQ (toTransformation "collapseRHSNoops" (onLet 'collapseRHSNoopsWorker)))
  fun = globalId User "fun" 12 (intFunTy 1)
  -- @SimIO Int@, and functions producing and combining actions
  simIOTy = C.AppTy (C.ConstTy (C.TyCon (mkUnsafeName User "Clash.Explicit.SimIO.SimIO" 300))) intTy
  simIOFun = globalId User "act" 13 (mkFunTy intTy simIOTy)
  bindIO = globalId User "both" 14 (mkFunTy simIOTy (mkFunTy simIOTy simIOTy))
  io = localId User "io" 22 simIOTy
  idPrim = Prim (PrimInfo "idPrim" (intFunTy 1) (WorkIdentity 0 []) SingleResult NoUnfolding)

-- | The regression tests, on the given implementation of the transformations
inlineTests :: InlineRewrites -> [TestTree]
inlineTests rws =
    [ testCase "inlineCleanup keeps binders that inlined binders refer to" $
        inlineCleanupNestedCycles (irInlineCleanup rws) >>= assertNoFreeLocals
    , testCase "inlineSmall does not inline a local variable (#405)" $
        inlineLocalVar (irInlineSmall rws) >>= assertStructurallyEqual (Var fLocal)
    , testCase "inlineWorkFree does not inline a local variable (#405)" $
        inlineLocalVar (irInlineWorkFree rws) >>= assertStructurallyEqual (Var fLocal)
    , testCase "collapseRHSNoops collapses a global no-op" $
        runCollapseRHSNoops (irCollapseRHSNoops rws) fGlobal >>= assertAlphaEq
          (Let
            (NonRec r
              (mkApps primUCo [Right intTy, Right intTy, Left (Var xs)]))
            (Var r))
    , testCase "collapseRHSNoops does not treat a local variable as a no-op" $
        collapseRHSNoopsLocalVar (irCollapseRHSNoops rws)
          >>= assertAlphaEq (collapseRHSNoopsTerm fLocalFun)
    ]
