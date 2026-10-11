{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.Inline"
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.Inline (tests) where

import Data.Default (def)
import GHC.Types.Basic (InlineSpec (Opaque))
import GHC.Types.SourceText (SourceText (NoSourceText))

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User), noSrcSpan)
import Clash.Core.Term
  ( Bind (..), IsMultiPrim (..), PrimInfo (..), PrimUnfolding (..), Term (..)
  , WorkInfo (..), mkApps )
import Clash.Core.Type (mkFunTy)
import Clash.Core.Util (primUCo)
import Clash.Core.Var (Id)
import Clash.Core.VarEnv (mkVarEnv)
import Clash.Driver.Types (Binding (..))
import Clash.Normalize.Transformations.Inline
  (collapseRHSNoops, inlineSmall, inlineWorkFree)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteState (..))

import Test.Clash.Rewrite
  ( assertAlphaEq, assertStructurallyEqual, globalId, inScopeOf, intFunTy
  , intId, intLit, intTy, localId, mkBinding, mkBindingMap
  , runSingleTransformation )

collapseRHSNoopsR, inlineSmallR, inlineWorkFreeR :: NormRewrite
collapseRHSNoopsR = $(asRewriteQ collapseRHSNoops)
inlineSmallR = $(asRewriteQ inlineSmall)
inlineWorkFreeR = $(asRewriteQ inlineWorkFree)

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
collapseRHSNoopsLocalVar :: IO Term
collapseRHSNoopsLocalVar = runCollapseRHSNoops fLocalFun

-- | Run 'collapseRHSNoops' on @let r = mapPrim f xs in r@, with the given @f@,
-- in a function in which the global @f = \\x -> x@ is defined. See
-- 'collapseRHSNoopsLocalVar'.
runCollapseRHSNoops :: Id -> IO Term
runCollapseRHSNoops f =
  runSingleTransformation def st (inScopeOf [fLocalFun, xs]) collapseRHSNoopsR
    (collapseRHSNoopsTerm f)
 where
  -- 'collapseRHSNoops' only runs in NOINLINE/OPAQUE functions
  top = globalId User "top" 10 intTy
  topBinding = (mkBinding top (intLit 0)) { bindingSpec = Opaque NoSourceText }
  x = intId User "x" 8
  st = def
    { _curFun = (top, noSrcSpan)
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
    [ testCase "inlineSmall does not inline a local variable (#405)" $
        inlineLocalVar inlineSmallR >>= assertStructurallyEqual (Var fLocal)
    , testCase "inlineWorkFree does not inline a local variable (#405)" $
        inlineLocalVar inlineWorkFreeR >>= assertStructurallyEqual (Var fLocal)
    , testCase "collapseRHSNoops collapses a global no-op" $
        runCollapseRHSNoops fGlobal >>= assertAlphaEq
          (Let
            (NonRec r
              (mkApps primUCo [Right intTy, Right intTy, Left (Var xs)]))
            (Var r))
    , testCase "collapseRHSNoops does not treat a local variable as a no-op" $
        collapseRHSNoopsLocalVar
          >>= assertAlphaEq (collapseRHSNoopsTerm fLocalFun)
    ]
