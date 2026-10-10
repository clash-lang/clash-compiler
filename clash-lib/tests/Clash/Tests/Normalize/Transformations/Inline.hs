{-|
  Copyright   :  (C) 2026, QBayLogic B.V.
  License     :  BSD2 (see the file LICENSE)
  Maintainer  :  QBayLogic B.V. <devops@qbaylogic.com>

  Tests for "Clash.Normalize.Transformations.Inline"
-}

{-# LANGUAGE TemplateHaskell #-}

module Clash.Tests.Normalize.Transformations.Inline (tests) where

import Data.Default (def)

import Test.Tasty
import Test.Tasty.HUnit

import Clash.Core.Name (NameSort (User))
import Clash.Core.Term (Term (..))
import Clash.Core.Var (Id)
import Clash.Normalize.Transformations.Inline (inlineSmall, inlineWorkFree)
import Clash.Normalize.Types (NormRewrite)
import Clash.Rewrite.StrategyDSL.TH (asRewriteQ)
import Clash.Rewrite.Types (RewriteState (..))

import Test.Clash.Rewrite
  ( assertStructurallyEqual, globalId, inScopeOf, intId, intLit, intTy
  , mkBindingMap, runSingleTransformation )

inlineSmallR, inlineWorkFreeR :: NormRewrite
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

tests :: TestTree
tests =
  testGroup
    "Clash.Tests.Normalize.Transformations.Inline"
    [ testCase "inlineSmall does not inline a local variable (#405)" $
        inlineLocalVar inlineSmallR >>= assertStructurallyEqual (Var fLocal)
    , testCase "inlineWorkFree does not inline a local variable (#405)" $
        inlineLocalVar inlineWorkFreeR >>= assertStructurallyEqual (Var fLocal)
    ]
